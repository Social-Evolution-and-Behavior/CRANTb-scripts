###########################################################
### Compile compartment metrics from split data and
### push axon/dendrite synapse counts + segregation index
### to seatable for CRANTb neurons
###
### Reads synapse CSVs from l2split/synapses/ (with Label
### column) and metrics CSVs from l2split/metrics/.
### Pushes: axon_input, axon_output, dendrite_input,
###         dendrite_output, segregation_index
###########################################################
source("R/crant-startup.R")

local({

synapses.folder <- file.path(crant.l2split.save.path, "synapses")
metrics.folder  <- file.path(crant.l2split.save.path, "metrics")

###########################
### Read current state  ###
###########################

message("### crantb: querying seatable for split metrics update ###")
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

# Only consider neurons that have been split (have split_[root_id] tag)
ac$has_split <- sapply(seq_len(nrow(ac)), function(i) {
  stored <- get_processed_rootid(ac$root_id_processed[i], "split")
  !is.na(stored) && stored == as.character(ac$root_id[i])
})
ac <- ac %>% dplyr::filter(has_split)

message(sprintf("Found %d neurons with up-to-date split data", nrow(ac)))

if (nrow(ac) == 0) {
  message("No split neurons found. Nothing to do.")
  return(invisible())
}

# Determine which neurons need split_metrics update
ac$needs_update <- needs_metric_update(ac$root_id, ac$root_id_processed, "split_metrics")
to_process <- ac %>% dplyr::filter(needs_update)

message(sprintf("%d neurons need split metrics update", nrow(to_process)))

if (nrow(to_process) == 0) {
  message("All split metrics are up to date. Nothing to do.")
  return(invisible())
}

###############################################
### Read segregation index from metrics     ###
###############################################

# Read all batch metrics CSVs and build a lookup
seg_index_lookup <- NULL
metrics_files <- list.files(metrics.folder, pattern = "\\.csv$", full.names = TRUE)
if (length(metrics_files) > 0) {
  seg_index_lookup <- tryCatch({
    all_mets <- lapply(metrics_files, function(f) {
      readr::read_csv(f, show_col_types = FALSE, progress = FALSE)
    })
    all_mets <- dplyr::bind_rows(all_mets)
    # Keep most recent entry per root_id
    all_mets <- all_mets %>%
      dplyr::distinct(root_id, .keep_all = TRUE)
    all_mets
  }, error = function(e) {
    message(sprintf("  Could not read metrics CSVs: %s", e$message))
    NULL
  })
}

###############################################
### Compile compartment synapse counts      ###
###############################################

message("### crantb: compiling compartment synapse counts ###")
results <- pbapply::pblapply(seq_len(nrow(to_process)), function(i) {
  rid <- to_process$root_id[i]
  syn_file <- file.path(synapses.folder, paste0(rid, ".csv"))

  if (!file.exists(syn_file)) {
    return(NULL)
  }

  tryCatch({
    syns <- readr::read_csv(syn_file, show_col_types = FALSE, progress = FALSE)

    if (nrow(syns) == 0 || is.null(syns$Label) || is.null(syns$prepost)) {
      return(NULL)
    }

    # Map numeric Label to compartment string (normalize dots to underscores)
    syns$compartment <- gsub("\\.", "_", hemibrainr::standard_compartments(syns$Label))

    # Count synapses per compartment and direction
    axon_input      <- sum(syns$prepost == 1 & syns$compartment == "axon", na.rm = TRUE)
    axon_output     <- sum(syns$prepost == 0 & syns$compartment == "axon", na.rm = TRUE)
    dendrite_input  <- sum(syns$prepost == 1 & syns$compartment == "dendrite", na.rm = TRUE)
    dendrite_output <- sum(syns$prepost == 0 & syns$compartment == "dendrite", na.rm = TRUE)

    # Get segregation index from metrics lookup
    seg_idx <- NA_real_
    if (!is.null(seg_index_lookup) && "segregation_index" %in% colnames(seg_index_lookup)) {
      match_row <- seg_index_lookup[seg_index_lookup$root_id == rid, ]
      if (nrow(match_row) > 0) {
        seg_idx <- match_row$segregation_index[1]
      }
    }

    data.frame(
      root_id = rid,
      axon_input = axon_input,
      axon_output = axon_output,
      dendrite_input = dendrite_input,
      dendrite_output = dendrite_output,
      segregation_index = seg_idx,
      stringsAsFactors = FALSE
    )
  }, error = function(e) {
    message(sprintf("  Error for %s: %s", rid, e$message))
    NULL
  })
})
names(results) <- to_process$root_id
results <- Filter(Negate(is.null), results)

if (length(results) == 0) {
  message("No compartment metrics computed. Nothing to update.")
  return(invisible())
}

metrics_df <- dplyr::bind_rows(results)
message(sprintf("Compiled compartment metrics for %d neurons", nrow(metrics_df)))

###############################################
### Push to seatable                        ###
###############################################

# Join with seatable _id and root_id_processed
metrics.update <- to_process %>%
  dplyr::select(`_id`, root_id, root_id_processed) %>%
  dplyr::left_join(metrics_df, by = "root_id") %>%
  dplyr::filter(!is.na(axon_input))

# Update root_id_processed tags
metrics.update$root_id_processed <- sapply(seq_len(nrow(metrics.update)), function(i) {
  set_processed_rootid(metrics.update$root_id_processed[i], "split_metrics", metrics.update$root_id[i])
})

# Clean for seatable
metrics.update[is.na(metrics.update)] <- ""
metrics.update$axon_input       <- as.numeric(metrics.update$axon_input)
metrics.update$axon_output      <- as.numeric(metrics.update$axon_output)
metrics.update$dendrite_input   <- as.numeric(metrics.update$dendrite_input)
metrics.update$dendrite_output  <- as.numeric(metrics.update$dendrite_output)
metrics.update$segregation_index <- as.numeric(metrics.update$segregation_index)

message(sprintf("Updating %d rows in seatable", nrow(metrics.update)))
message("Columns: axon_input, axon_output, dendrite_input, dendrite_output, segregation_index")

crant_table_update_rows(
  df = metrics.update %>%
    dplyr::select(`_id`, axon_input, axon_output, dendrite_input,
                  dendrite_output, segregation_index, root_id_processed),
  table = "CRANTb_meta",
  base = "CRANTb",
  append_allowed = FALSE,
  chunksize = 1000
)

message("### crantb: split metrics update complete ###")

})
