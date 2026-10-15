###########################################################
### Update CAVE compartment_synapses_pre_v2 and
### compartment_synapses_post_v2
###
### Labels each synapses_v2 synapse with the compartment it
### sits on, separately for the presynaptic and the
### postsynaptic neuron. Both tables use the reference_tag
### schema, referencing synapses_v2:
###   - target_id: annotation ID in synapses_v2
###   - tag:       "axon", "dendrite", "primary_dendrite",
###                "primary_neurite" or "soma"
### A synapse has at most one row in each table.
###
### Labelled neurons:
###   - afferent neurons: every synapse "axon" (by rule)
###   - efferent neurons: every synapse "dendrite" (by rule)
###   - other BACKBONE_PROOFREAD neurons: flow-centrality split
### Labels come from l2split/synapses/{root_id}.csv, written
### by crant-split.R. Synapses flagged as bad are skipped.
###
### CAVE annotations cannot be edited in place, so this
### script keeps a local manifest of what it has posted:
###   compartments/manifest/{root_id}.csv  annotation_id,
###                                       target_id, side, tag
###   compartments/synced.csv             root_id, csv_mtime
### Each run diffs the labels each neuron should have
### against the manifest, deletes what is stale and posts
### what is missing. Neurons that are edited (new root_id),
### lose proofread status or change flow class have their
### old rows removed.
###
### Environment variables:
###   CRANT_COMPARTMENT_DRY_RUN=TRUE  report changes only
###   CRANT_COMPARTMENT_HOURS=4       time budget for posting;
###                                   the rest is done next run
###
### The tables are created once by crant-compartment-tables.R
###########################################################
source("R/crant-startup.R")

local({

cave_tables <- c(pre = "compartment_synapses_pre_v2",
                 post = "compartment_synapses_post_v2")
compartment_tags <- c("axon", "dendrite", "primary_dendrite",
                      "primary_neurite", "soma")
synapses.folder <- file.path(crant.l2split.save.path, "synapses")
compartments.folder <- file.path(crant.save.path, "compartments")
manifest.folder <- file.path(compartments.folder, "manifest")
synced.file <- file.path(compartments.folder, "synced.csv")
dry_run <- isTRUE(as.logical(Sys.getenv("CRANT_COMPARTMENT_DRY_RUN", "FALSE")))
time_budget <- as.numeric(Sys.getenv("CRANT_COMPARTMENT_HOURS", "4")) * 3600
chunksize <- 5000
start_time <- Sys.time()
if (dry_run) message("DRY RUN: no changes will be made to CAVE or the manifest")

###########################
### Read current state  ###
###########################

message("### crantb: querying seatable for labelled neurons ###")
ac <- crant_table_query(
  sql = "SELECT _id, root_id, status, super_class, flow, root_id_processed FROM CRANTb_meta"
)
ac[ac == ""] <- NA
ac[ac == "0"] <- NA
ac <- ac %>%
  dplyr::filter(!is_non_neuron(status)) %>%
  dplyr::filter(!is.na(root_id)) %>%
  dplyr::distinct(root_id, .keep_all = TRUE)

# Afferent/efferent neurons (any status) and proofread neurons whose split
# output is up to date for their current root_id
ac$rule <- compartment_rule(ac$flow, ac$super_class)
ac$has_split <- sapply(seq_len(nrow(ac)), function(i) {
  identical(get_processed_rootid(ac$root_id_processed[i], "split"), ac$root_id[i])
})
labelled <- ac %>%
  dplyr::filter(has_split, !is.na(rule) | is_backbone_proofread(status))
labelled$csv <- file.path(synapses.folder, paste0(labelled$root_id, ".csv"))
labelled <- labelled[file.exists(labelled$csv), ]
labelled$csv_mtime <- format(file.mtime(labelled$csv), "%Y-%m-%d %H:%M:%OS3")
message(sprintf("Found %d neurons with compartment labels", nrow(labelled)))

######################################
### Check the CAVE tables exist    ###
######################################

fcc <- crant_cave_client()
existing_tables <- unlist(reticulate::py_to_r(fcc$annotation$get_tables()))
missing_tables <- setdiff(cave_tables, existing_tables)
if (length(missing_tables) && !dry_run) {
  message(sprintf("CAVE tables missing: %s. Create them with R/crant-compartment-tables.R",
                  paste(missing_tables, collapse = ", ")))
  return(invisible())
}

###########################
### Read the manifest   ###
###########################

if (!dry_run) dir.create(manifest.folder, recursive = TRUE, showWarnings = FALSE)
manifest_cols <- readr::cols(.default = readr::col_character())
read_manifest <- function(rid) {
  f <- file.path(manifest.folder, paste0(rid, ".csv"))
  if (!file.exists(f)) {
    return(data.frame(annotation_id = character(0), target_id = character(0),
                      side = character(0), tag = character(0)))
  }
  readr::read_csv(f, col_types = manifest_cols, progress = FALSE)
}
write_manifest <- function(rid, m) {
  f <- file.path(manifest.folder, paste0(rid, ".csv"))
  if (nrow(m)) readr::write_csv(m, f) else if (file.exists(f)) file.remove(f)
}

manifest_files <- list.files(manifest.folder, pattern = "\\.csv$", full.names = TRUE)
manifest <- if (length(manifest_files)) {
  readr::read_csv(manifest_files, col_types = manifest_cols, progress = FALSE, id = "file") %>%
    dplyr::mutate(root_id = sub("\\.csv$", "", basename(file))) %>%
    dplyr::select(-file)
} else {
  data.frame(annotation_id = character(0), target_id = character(0),
             side = character(0), tag = character(0), root_id = character(0))
}
synced <- if (file.exists(synced.file)) {
  readr::read_csv(synced.file, col_types = manifest_cols, progress = FALSE)
} else {
  data.frame(root_id = character(0), csv_mtime = character(0))
}
message(sprintf("Manifest: %d annotations for %d neurons",
                nrow(manifest), length(unique(manifest$root_id))))

###########################
### CAVE helpers        ###
###########################

np <- reticulate::import("numpy", convert = FALSE)
as_py_int64 <- function(x) np$array(as.numeric(x))$astype("int64")$tolist()

post_rows <- function(table, df) {
  ids <- character(0)
  for (chunk in split(df, ceiling(seq_len(nrow(df)) / chunksize))) {
    pdf <- reticulate::r_to_py(data.frame(target_id = as.numeric(chunk$target_id),
                                          tag = chunk$tag))
    pdf$`__setitem__`("target_id", pdf$`__getitem__`("target_id")$astype("int64"))
    res <- reticulate::py_to_r(
      fcc$annotation$post_annotation(table, pdf$to_dict(orient = "records")))
    new_ids <- sprintf("%.0f", as.numeric(unlist(res)))
    if (length(new_ids) != nrow(chunk)) {
      stop(sprintf("CAVE returned %d ids for %d annotations posted to %s",
                   length(new_ids), nrow(chunk), table))
    }
    ids <- c(ids, new_ids)
  }
  ids
}

delete_rows <- function(table, ids) {
  for (chunk in split(ids, ceiling(seq_along(ids) / chunksize))) {
    fcc$annotation$delete_annotation(table, as_py_int64(chunk))
  }
}

# Delete manifest rows (any root, both tables) from CAVE
delete_manifest_rows <- function(m) {
  for (s in names(cave_tables)) {
    ids <- m$annotation_id[m$side == s]
    if (length(ids)) delete_rows(cave_tables[[s]], ids)
  }
}

############################################
### Remove neurons no longer labelled    ###
############################################

# Neurons edited (new root_id), demoted from proofread, reclassified or
# deleted: everything posted for their old root_id is removed
stale_roots <- setdiff(unique(manifest$root_id), labelled$root_id)
stale <- manifest %>% dplyr::filter(root_id %in% stale_roots)
message(sprintf("%d annotations to remove for %d neurons no longer labelled",
                nrow(stale), length(stale_roots)))
if (nrow(stale) && !dry_run) {
  delete_manifest_rows(stale)
  for (rid in stale_roots) write_manifest(rid, manifest[0, c("annotation_id", "target_id", "side", "tag")])
  manifest <- manifest %>% dplyr::filter(!(root_id %in% stale_roots))
  synced <- synced %>% dplyr::filter(!(root_id %in% stale_roots))
  readr::write_csv(synced, synced.file)
}

############################################
### Sync labelled neurons                ###
############################################

# A neuron needs syncing if its labels CSV changed since it was last synced
to_sync <- labelled %>%
  dplyr::left_join(synced %>% dplyr::rename(synced_mtime = csv_mtime), by = "root_id") %>%
  dplyr::filter(is.na(synced_mtime) | synced_mtime != csv_mtime)
message(sprintf("%d neurons to sync (%d already in sync)",
                nrow(to_sync), nrow(labelled) - nrow(to_sync)))

# Owner of each (synapse, side) in the manifest, to resolve conflicts when
# a synapse moves from one neuron to another
owner_key <- paste(manifest$target_id, manifest$side)
n_added <- 0
n_deleted <- 0
n_synced <- 0

for (i in seq_len(nrow(to_sync))) {
  if (!dry_run && as.numeric(difftime(Sys.time(), start_time, units = "secs")) > time_budget) {
    message(sprintf("Time budget reached; %d neurons left for the next run",
                    nrow(to_sync) - i + 1))
    break
  }
  rid <- to_sync$root_id[i]

  tryCatch({
    # Labels this neuron should have
    syns <- readr::read_csv(to_sync$csv[i], col_types = manifest_cols, progress = FALSE)
    if (!nrow(syns) || is.null(syns$Label)) {
      syns <- data.frame(connector_id = character(0), prepost = character(0),
                         Label = character(0), status = character(0))
    }
    if (is.null(syns$status)) syns$status <- "good"
    want <- syns %>%
      dplyr::filter(status %in% "good", !is.na(connector_id)) %>%
      dplyr::transmute(target_id = connector_id,
                       side = ifelse(prepost == "0", "pre", "post"),
                       tag = gsub("\\.", "_", hemibrainr::standard_compartments(as.integer(Label)))) %>%
      dplyr::filter(tag %in% compartment_tags) %>%
      dplyr::distinct(target_id, side, .keep_all = TRUE)

    # Diff against what was posted for this root_id
    have <- read_manifest(rid)
    want_key <- paste(want$target_id, want$side, want$tag)
    have_key <- paste(have$target_id, have$side, have$tag)
    to_delete <- have[!(have_key %in% want_key), ]
    to_add <- want[!(want_key %in% have_key), ]

    # Synapses on this side still labelled under another root_id
    conflict <- manifest[owner_key %in% paste(to_add$target_id, to_add$side) &
                           manifest$root_id != rid, ]

    if (dry_run) {
      n_added <- n_added + nrow(to_add)
      n_deleted <- n_deleted + nrow(to_delete) + nrow(conflict)
      n_synced <- n_synced + 1
      next
    }

    # Delete first, so no synapse ever has two rows in a table
    if (nrow(to_delete) || nrow(conflict)) {
      delete_manifest_rows(dplyr::bind_rows(to_delete, conflict))
      have <- have[have_key %in% want_key, ]
      write_manifest(rid, have)
      for (orid in unique(conflict$root_id)) {
        om <- read_manifest(orid)
        write_manifest(orid, om[!(om$annotation_id %in% conflict$annotation_id), ])
      }
      keep <- !(manifest$annotation_id %in% c(conflict$annotation_id, to_delete$annotation_id))
      manifest <- manifest[keep, ]
      owner_key <- owner_key[keep]
      n_deleted <- n_deleted + nrow(to_delete) + nrow(conflict)
    }

    for (s in names(cave_tables)) {
      add <- to_add[to_add$side == s, ]
      if (!nrow(add)) next
      add$annotation_id <- post_rows(cave_tables[[s]], add)
      add <- add[, c("annotation_id", "target_id", "side", "tag")]
      have <- dplyr::bind_rows(have, add)
      write_manifest(rid, have)
      manifest <- dplyr::bind_rows(manifest, dplyr::mutate(add, root_id = rid))
      owner_key <- c(owner_key, paste(add$target_id, add$side))
      n_added <- n_added + nrow(add)
    }

    # Record as synced only once CAVE matches the labels CSV
    synced <- synced %>%
      dplyr::filter(root_id != rid) %>%
      dplyr::bind_rows(data.frame(root_id = rid, csv_mtime = to_sync$csv_mtime[i]))
    readr::write_csv(synced, synced.file)
    n_synced <- n_synced + 1
  }, error = function(e) {
    message(sprintf("  Error syncing %s: %s", rid, e$message))
  })

  if (i %% 50 == 0) {
    message(sprintf("  Synced %d/%d neurons (%d added, %d deleted)",
                    i, nrow(to_sync), n_added, n_deleted))
  }
}

message(sprintf("%s %d neurons: %d annotations %s, %d %s",
                if (dry_run) "Would sync" else "Synced", n_synced,
                n_added, if (dry_run) "to add" else "added",
                n_deleted, if (dry_run) "to delete" else "deleted"))
message("### crantb: compartment synapse sync complete ###")

})
