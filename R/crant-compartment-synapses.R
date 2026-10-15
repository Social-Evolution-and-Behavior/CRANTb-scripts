###########################################################
### Update CAVE compartment_synapses_pre_v2 and
### compartment_synapses_post_v2
###
### Labels synapses_v2 synapses with the compartment they
### sit on, separately for the presynaptic and the
### postsynaptic neuron. Both tables use the reference_tag
### schema, referencing synapses_v2:
###   - target_id: annotation ID in synapses_v2
###   - tag:       "axon", "dendrite", "primary_dendrite",
###                "primary_neurite" or "soma"
###
### Labelled neurons:
###   - afferent neurons: every synapse "axon" (by rule)
###   - efferent neurons: every synapse "dendrite" (by rule)
###   - other BACKBONE_PROOFREAD neurons: flow-centrality split
### Labels come from l2split/synapses/{root_id}.csv, written
### by crant-split.R. Synapses flagged as bad are skipped.
###
### A synapse is only labelled when BOTH its presynaptic and
### postsynaptic neurons are labelled neurons, i.e. it is a
### good output in one neuron's labels and a good input in
### another's. It then has one row in each table.
###
### CAVE annotations cannot be edited in place, so this
### script keeps a local manifest of what it has posted
### (compartments/manifest.csv: annotation_id, target_id,
### side, tag). Each run works out the full set of labels
### wanted, deletes posted rows that are no longer wanted
### (neuron edited, demoted, reclassified, or its partner
### was) and posts the missing ones.
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
manifest.file <- file.path(compartments.folder, "manifest.csv")
dry_run <- isTRUE(as.logical(Sys.getenv("CRANT_COMPARTMENT_DRY_RUN", "FALSE")))
time_budget <- as.numeric(Sys.getenv("CRANT_COMPARTMENT_HOURS", "4")) * 3600
chunksize <- 5000
start_time <- Sys.time()
chr_cols <- readr::cols(.default = readr::col_character())
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
message(sprintf("Found %d labelled neurons", nrow(labelled)))

###############################################
### Labels wanted                           ###
###############################################

message("### crantb: reading compartment labels ###")
syns <- dplyr::bind_rows(lapply(labelled$csv, function(f) {
  x <- readr::read_csv(f, col_types = chr_cols, progress = FALSE)
  if (!nrow(x) || !all(c("connector_id", "prepost", "status", "Label") %in% colnames(x))) return(NULL)
  x <- x[, c("connector_id", "prepost", "status", "Label")]
  x$mtime <- file.mtime(f)
  x
}))
want <- syns %>%
  dplyr::filter(status %in% "good", !is.na(connector_id)) %>%
  dplyr::transmute(target_id = connector_id,
                   side = ifelse(prepost == "0", "pre", "post"),
                   tag = gsub("\\.", "_", hemibrainr::standard_compartments(as.integer(Label))),
                   mtime = mtime) %>%
  dplyr::filter(tag %in% compartment_tags) %>%
  # If two neurons' labels claim the same side of a synapse, one is stale:
  # keep the most recently written
  dplyr::arrange(dplyr::desc(mtime)) %>%
  dplyr::distinct(target_id, side, .keep_all = TRUE) %>%
  dplyr::select(-mtime)
rm(syns)

# Only synapses labelled on both sides, i.e. between two labelled neurons
both <- intersect(want$target_id[want$side == "pre"], want$target_id[want$side == "post"])
want <- want[want$target_id %in% both, ]
message(sprintf("%d synapses between labelled neurons (%d rows across both tables)",
                length(both), nrow(want)))

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
### Diff with manifest  ###
###########################

manifest <- if (file.exists(manifest.file)) {
  readr::read_csv(manifest.file, col_types = chr_cols, progress = FALSE)
} else {
  data.frame(annotation_id = character(0), target_id = character(0),
             side = character(0), tag = character(0))
}
message(sprintf("Manifest: %d annotations posted", nrow(manifest)))

want_key <- paste(want$target_id, want$side, want$tag)
have_key <- paste(manifest$target_id, manifest$side, manifest$tag)
to_delete <- manifest[!(have_key %in% want_key), ]
to_add <- want[!(want_key %in% have_key), ]
message(sprintf("%d annotations to delete, %d to add", nrow(to_delete), nrow(to_add)))

if (dry_run) {
  print(table(side = to_add$side, tag = to_add$tag))
  message("### crantb: compartment synapse sync complete (dry run) ###")
  return(invisible())
}

###########################
### CAVE helpers        ###
###########################

dir.create(compartments.folder, recursive = TRUE, showWarnings = FALSE)
np <- reticulate::import("numpy", convert = FALSE)
as_py_int64 <- function(x) np$array(as.numeric(x))$astype("int64")$tolist()

post_rows <- function(table, df) {
  pdf <- reticulate::r_to_py(data.frame(target_id = as.numeric(df$target_id),
                                        tag = df$tag))
  pdf$`__setitem__`("target_id", pdf$`__getitem__`("target_id")$astype("int64"))
  res <- reticulate::py_to_r(
    fcc$annotation$post_annotation(table, pdf$to_dict(orient = "records")))
  ids <- sprintf("%.0f", as.numeric(unlist(res)))
  if (length(ids) != nrow(df)) {
    stop(sprintf("CAVE returned %d ids for %d annotations posted to %s",
                 length(ids), nrow(df), table))
  }
  ids
}

# Record each change in the manifest as soon as CAVE has accepted it
save_manifest <- function() readr::write_csv(manifest, manifest.file)

###########################
### Delete, then add    ###
###########################

# Delete first, so no synapse ever has two rows in a table
n_deleted <- 0
for (s in names(cave_tables)) {
  ids <- to_delete$annotation_id[to_delete$side == s]
  for (chunk in split(ids, ceiling(seq_along(ids) / chunksize))) {
    fcc$annotation$delete_annotation(cave_tables[[s]], as_py_int64(chunk))
    manifest <- manifest[!(manifest$annotation_id %in% chunk & manifest$side == s), ]
    save_manifest()
    n_deleted <- n_deleted + length(chunk)
  }
}
if (n_deleted) message(sprintf("Deleted %d annotations", n_deleted))

n_added <- 0
out_of_time <- FALSE
for (s in names(cave_tables)) {
  add <- to_add[to_add$side == s, ]
  for (chunk in split(add, ceiling(seq_len(nrow(add)) / chunksize))) {
    if (as.numeric(difftime(Sys.time(), start_time, units = "secs")) > time_budget) {
      out_of_time <- TRUE
      break
    }
    chunk$annotation_id <- post_rows(cave_tables[[s]], chunk)
    manifest <- dplyr::bind_rows(manifest, chunk[, c("annotation_id", "target_id", "side", "tag")])
    save_manifest()
    n_added <- n_added + nrow(chunk)
  }
}
if (out_of_time) {
  message(sprintf("Time budget reached; %d annotations left for the next run",
                  nrow(to_add) - n_added))
}
message(sprintf("Added %d annotations; manifest now has %d", n_added, nrow(manifest)))
message("### crantb: compartment synapse sync complete ###")

})
