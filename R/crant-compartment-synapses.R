###########################################################
### Update CAVE compartment_synapses_v2 table
###
### For each proofread neuron that has been split into
### axon/dendrite (by crant-split.R), annotate each synapse
### in CAVE with its compartment label.
###
### This is a REFERENCE TABLE — each row references an
### annotation in synapses_v2 by its ID (target_id), with
### no spatial point needed.
###
### Each row represents one synapse endpoint on a split
### neuron, with fields:
###   - target_id:    annotation ID from synapses_v2
###   - compartment:  "axon", "dendrite", "primary_dendrite",
###                   "primary_neurite", "soma", or "unknown"
###   - prepost:      0 = presynaptic/output,
###                   1 = postsynaptic/input
###
### Suggested CAVE schema for compartment_synapses_v2:
###   Schema type: reference annotation (references synapses_v2)
###   Fields:
###     target_id     - int64, references synapses_v2.id
###     compartment   - string: one of "axon", "dendrite",
###                     "primary_dendrite", "primary_neurite",
###                     "soma", "unknown"
###     prepost       - int (0=output, 1=input)
###
### One synapse may have two rows if both the pre and post
### neurons have been split. The prepost field disambiguates
### which neuron's compartment is being annotated.
###
### NOTE: The CAVE table must be created manually before
### this script can run. This script only populates it.
###########################################################
source("R/crant-startup.R")

local({

cave_table_name <- "compartment_synapses_v2"
cave_user_id <- 92L
synapses.folder <- file.path(crant.l2split.save.path, "synapses")

###########################
### Read current state  ###
###########################

message("### crantb: querying seatable for split neurons ###")
ac <- crant_table_query(
  sql = "SELECT _id, root_id, status, root_id_processed FROM CRANTb_meta"
)
ac[ac == ""] <- NA
ac[ac == "0"] <- NA

# Filter non-neurons
ac <- ac %>%
  dplyr::filter(!(grepl("DELETE|GLIA|NOT_A_NEURON|DEBRIS", status, ignore.case = TRUE) & !is.na(status))) %>%
  dplyr::filter(!is.na(root_id)) %>%
  dplyr::distinct(root_id, .keep_all = TRUE)

# Only neurons that have been split
ac$has_split <- sapply(seq_len(nrow(ac)), function(i) {
  stored <- get_processed_rootid(ac$root_id_processed[i], "split")
  !is.na(stored) && stored == as.character(ac$root_id[i])
})
split_neurons <- ac %>% dplyr::filter(has_split)

message(sprintf("Found %d neurons with up-to-date split data", nrow(split_neurons)))

if (nrow(split_neurons) == 0) {
  message("No split neurons found. Nothing to do.")
  return(invisible())
}

###############################################
### Read CAVE table to find existing annots ###
###############################################

message(sprintf("### crantb: reading CAVE %s table ###", cave_table_name))
cave_existing <- tryCatch(
  crant_cave_query(table = cave_table_name, live = 2),
  error = function(e) {
    message(sprintf("  Could not read CAVE %s: %s", cave_table_name, e$message))
    message("  If the table does not exist yet, create it first.")
    return(invisible())
  }
)

if (is.null(cave_existing)) {
  return(invisible())
}

# Determine which root_ids already have annotations by looking up
# the target synapses. We track by root_id via the synapse CSVs we produced.
# Build a set of root_ids whose synapses are already in the CAVE table.
if (nrow(cave_existing) > 0) {
  # The existing annotations reference synapses_v2 IDs via target_id.
  # To find which root_ids are covered, we check which synapse CSV files
  # have ALL their connector_ids already present in the CAVE table.
  existing_target_ids <- as.character(cave_existing$target_id)
  annotated_rids <- character(0)

  for (rid in split_neurons$root_id) {
    syn_file <- file.path(synapses.folder, paste0(rid, ".csv"))
    if (!file.exists(syn_file)) next
    syns <- tryCatch(
      readr::read_csv(syn_file, show_col_types = FALSE, progress = FALSE),
      error = function(e) NULL
    )
    if (is.null(syns) || nrow(syns) == 0) next
    # If all connector_ids for this neuron are already in CAVE, skip it
    if (all(as.character(syns$connector_id) %in% existing_target_ids)) {
      annotated_rids <- c(annotated_rids, rid)
    }
  }
  message(sprintf("CAVE %s: %d existing annotations, %d neurons fully annotated",
                  cave_table_name, nrow(cave_existing), length(annotated_rids)))
} else {
  annotated_rids <- character(0)
  message(sprintf("CAVE %s: 0 existing annotations", cave_table_name))
}

###############################################
### Determine what to add                   ###
###############################################

to_add <- split_neurons %>%
  dplyr::filter(!(root_id %in% annotated_rids))

message(sprintf("%d neurons to annotate in CAVE %s", nrow(to_add), cave_table_name))

###############################################
### Determine what to remove                ###
###############################################

# Remove annotations whose target synapse belongs to a neuron that is
# no longer split/proofread. Since this is a reference table, we track
# which annotations to remove by checking if ANY existing annotation's
# target_id is NOT in the set of synapses from currently-split neurons.
# For efficiency, we remove annotations for neurons whose synapse CSVs
# no longer exist or whose root_id is stale.
split_rids <- split_neurons$root_id
all_valid_connector_ids <- character(0)
for (rid in split_rids) {
  syn_file <- file.path(synapses.folder, paste0(rid, ".csv"))
  if (!file.exists(syn_file)) next
  syns <- tryCatch(
    readr::read_csv(syn_file, show_col_types = FALSE, progress = FALSE),
    error = function(e) NULL
  )
  if (!is.null(syns) && nrow(syns) > 0) {
    all_valid_connector_ids <- c(all_valid_connector_ids, as.character(syns$connector_id))
  }
}
all_valid_connector_ids <- unique(all_valid_connector_ids)

to_remove <- data.frame()
if (nrow(cave_existing) > 0) {
  to_remove <- cave_existing[!(as.character(cave_existing$target_id) %in% all_valid_connector_ids), ]
}

message(sprintf("%d annotations to remove (neuron no longer split/proofread)",
                nrow(to_remove)))

###############################################
### Add new annotations                     ###
###############################################

if (nrow(to_add) > 0) {
  message(sprintf("### crantb: adding compartment annotations for %d neurons ###",
                  nrow(to_add)))

  client <- crant_cave_client()
  n_added <- 0

  for (i in seq_len(nrow(to_add))) {
    rid <- to_add$root_id[i]
    syn_file <- file.path(synapses.folder, paste0(rid, ".csv"))

    if (!file.exists(syn_file)) {
      message(sprintf("  Skipping %s: no synapse CSV", rid))
      next
    }

    tryCatch({
      syns <- readr::read_csv(syn_file, show_col_types = FALSE, progress = FALSE)

      if (nrow(syns) == 0 || is.null(syns$Label)) {
        message(sprintf("  Skipping %s: no labelled synapses", rid))
        next
      }

      # Map Label to compartment string (normalize dots to underscores)
      syns$compartment <- gsub("\\.", "_", hemibrainr::standard_compartments(syns$Label))

      stage <- client$annotation$stage_annotations(cave_table_name)
      batch_count <- 0

      for (j in seq_len(nrow(syns))) {
        connector_id <- syns$connector_id[j]
        compartment <- syns$compartment[j]
        prepost_val <- as.integer(syns$prepost[j])

        if (is.na(connector_id) || is.na(compartment)) next

        stage$add(
          target_id = as.numeric(connector_id),
          compartment = compartment,
          prepost = prepost_val
        )
        batch_count <- batch_count + 1

        # Upload in batches of 500
        if (batch_count >= 500) {
          client$annotation$upload_staged_annotations(stage)
          n_added <- n_added + batch_count
          stage$clear_annotations()
          batch_count <- 0
        }
      }

      # Upload remaining
      if (batch_count > 0) {
        client$annotation$upload_staged_annotations(stage)
        n_added <- n_added + batch_count
        stage$clear_annotations()
      }

    }, error = function(e) {
      message(sprintf("  Error annotating %s: %s", rid, e$message))
      tryCatch(stage$clear_annotations(), error = function(e2) NULL)
    })

    if (i %% 10 == 0) {
      message(sprintf("  Processed %d/%d neurons (%d annotations added)",
                      i, nrow(to_add), n_added))
    }
  }

  message(sprintf("Added %d compartment annotations to %s", n_added, cave_table_name))

  # Pause for CAVE ingest
  if (n_added > 0) {
    wait_secs <- max(n_added * 0.01, 5)
    message(sprintf("Waiting %.0fs for CAVE to ingest annotations...", wait_secs))
    Sys.sleep(wait_secs)
  }
}

###############################################
### Remove stale annotations                ###
###############################################

if (nrow(to_remove) > 0) {
  message(sprintf("### crantb: removing %d stale compartment annotations ###",
                  nrow(to_remove)))

  client <- crant_cave_client()
  annotation_ids <- as.integer(to_remove$id)

  tryCatch({
    result <- client$annotation$delete_annotation(cave_table_name, annotation_ids)
    message(sprintf("Removed %d annotations from %s", length(result), cave_table_name))
  }, error = function(e) {
    message(sprintf("Batch removal failed: %s", e$message))
    message("Attempting one-by-one removal...")
    n_removed <- 0
    for (aid in annotation_ids) {
      tryCatch({
        client$annotation$delete_annotation(cave_table_name, aid)
        n_removed <- n_removed + 1
      }, error = function(e2) {
        message(sprintf("  Failed to remove annotation %s: %s", aid, e2$message))
      })
    }
    message(sprintf("Removed %d/%d annotations", n_removed, length(annotation_ids)))
  })
}

###############################################
### Verify                                  ###
###############################################

message(sprintf("### crantb: verifying CAVE %s ###", cave_table_name))
Sys.sleep(5)
cave_final <- tryCatch(
  crant_cave_query(table = cave_table_name, live = 2),
  error = function(e) {
    message(sprintf("  Verification failed: %s", e$message))
    NULL
  }
)
if (!is.null(cave_final)) {
  message(sprintf("CAVE %s now has %d annotations", cave_table_name, nrow(cave_final)))
}

message(sprintf("### crantb: %s sync complete ###", cave_table_name))

})
