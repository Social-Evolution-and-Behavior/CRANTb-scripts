###########################################################
### Calculate L2 metrics (nodes, cable length) and push
### to seatable for CRANTb neurons
###########################################################
source("R/crant-startup.R")

local({

# Query seatable for all neurons
message("### crantb: querying seatable for neurons ###")
ac <- crant_table_query(
  sql = "SELECT _id, root_id, status, root_id_processed FROM CRANTb_meta"
)
ac[ac == ""] <- NA
ac[ac == "0"] <- NA

# Filter out non-neurons
ac <- ac %>%
  dplyr::filter(!(grepl("DELETE|GLIA|NOT_A_NEURON|DEBRIS", status, ignore.case = TRUE) & !is.na(status))) %>%
  dplyr::filter(!is.na(root_id)) %>%
  dplyr::distinct(root_id, .keep_all = TRUE)

message(sprintf("Found %d neurons in seatable", nrow(ac)))

# Determine which neurons need L2 metric update
ac$needs_update <- needs_metric_update(ac$root_id, ac$root_id_processed, "l2")

# Of those needing update, check which have SWC files available
ac$has_swc <- sapply(ac$root_id, function(rid) {
  file.exists(file.path(crant.l2swc.save.path, paste0(rid, ".swc")))
})

# Clean up stale metric CSVs for neurons whose root_id has changed
cleanup_stale_files(ac$root_id, ac$root_id_processed, "l2",
                    dir = crant.metrics.save.path, ext = ".csv")

to_process <- ac %>% dplyr::filter(needs_update & has_swc)
message(sprintf("%d neurons need L2 metric update (%d have SWC files)",
                sum(ac$needs_update), nrow(to_process)))

if (nrow(to_process) == 0) {
  message("All L2 metrics are up to date. Nothing to do.")
  return(invisible())
}

# Calculate metrics for each neuron by reading SWC files
message("### crantb: calculating L2 metrics ###")
metrics_list <- pbapply::pblapply(seq_len(nrow(to_process)), function(i) {
  rid <- to_process$root_id[i]
  swc_file <- file.path(crant.l2swc.save.path, paste0(rid, ".swc"))

  tryCatch({
    neuron <- nat::read.neuron(swc_file)
    stats <- summary(nat::as.neuronlist(neuron))
    # Stub SWC files (single node, from failed L2 downloads) get l2_nodes=1, cable_length=0
    if (stats$nodes <= 1) {
      data.frame(
        root_id = rid, l2_nodes = 1,
        l2_cable_length = 0, l2_cable_length_um = 0,
        stringsAsFactors = FALSE
      )
    } else {
      data.frame(
        root_id = rid, l2_nodes = stats$nodes,
        l2_cable_length = round(stats$cable.length, 2),
        l2_cable_length_um = round(stats$cable.length / 1000, 2),
        stringsAsFactors = FALSE
      )
    }
  }, error = function(e) {
    message(sprintf("  Error reading %s: %s", rid, e$message))
    NULL
  })
})
names(metrics_list) <- to_process$root_id
metrics_list <- Filter(Negate(is.null), metrics_list)

if (length(metrics_list) == 0) {
  message("No metrics calculated. Nothing to update.")
  return(invisible())
}

metrics <- dplyr::bind_rows(metrics_list)
message(sprintf("Calculated metrics for %d neurons", nrow(metrics)))

# Save metrics locally
dir.create(crant.metrics.save.path, showWarnings = FALSE, recursive = TRUE)
for (i in seq_len(nrow(metrics))) {
  rid <- metrics$root_id[i]
  readr::write_csv(metrics[i, ], file.path(crant.metrics.save.path, paste0(rid, ".csv")))
}
message(sprintf("Saved metrics CSVs to %s", crant.metrics.save.path))

# Prepare seatable update
metrics.update <- to_process %>%
  dplyr::select(`_id`, root_id, root_id_processed) %>%
  dplyr::left_join(metrics %>% dplyr::select(root_id, l2_nodes, l2_cable_length_um),
                   by = "root_id") %>%
  dplyr::filter(!is.na(l2_nodes))

# Update root_id_processed tags
metrics.update$root_id_processed <- sapply(seq_len(nrow(metrics.update)), function(i) {
  set_processed_rootid(metrics.update$root_id_processed[i], "l2", metrics.update$root_id[i])
})

# Clean for seatable
metrics.update[is.na(metrics.update)] <- ""
metrics.update$l2_nodes <- as.numeric(metrics.update$l2_nodes)
metrics.update$l2_cable_length_um <- as.numeric(metrics.update$l2_cable_length_um)

message(sprintf("Updating %d rows in seatable", nrow(metrics.update)))
message("Columns to update: l2_nodes, l2_cable_length_um, root_id_processed")

# Push to seatable
crant_table_update_rows(
  df = metrics.update %>% dplyr::select(`_id`, l2_nodes, l2_cable_length_um, root_id_processed),
  table = "CRANTb_meta",
  base = "CRANTb",
  append_allowed = FALSE,
  chunksize = 1000
)

message("### crantb: L2 metric update complete ###")

})
