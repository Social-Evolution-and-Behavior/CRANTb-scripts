###########################################################
### Calculate neuron volumes via L2 cache and push
### volume_nm3 to seatable for CRANTb neurons
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

# Determine which neurons need volume update
ac$needs_update <- needs_metric_update(ac$root_id, ac$root_id_processed, "volume")

# Clean up stale volume CSVs for neurons whose root_id has changed
cleanup_stale_files(ac$root_id, ac$root_id_processed, "volume",
                    dir = crant.volumes.save.path, ext = ".csv")

to_process <- ac %>% dplyr::filter(needs_update)
message(sprintf("%d neurons need volume update", nrow(to_process)))

if (nrow(to_process) == 0) {
  message("All volumes are up to date. Nothing to do.")
  return(invisible())
}

# Calculate volumes via L2 cache
message("### crantb: calculating neuron volumes ###")
dir.create(crant.volumes.save.path, showWarnings = FALSE, recursive = TRUE)

volume_list <- pbapply::pblapply(seq_len(nrow(to_process)), function(i) {
  rid <- to_process$root_id[i]
  vol_file <- file.path(crant.volumes.save.path, paste0(rid, ".csv"))

  tryCatch({
    # Check for cached volume file with current root_id
    if (file.exists(vol_file)) {
      vol <- readr::read_csv(vol_file, show_col_types = FALSE, progress = FALSE)
    } else {
      vol <- crant_neuron_volume(rid, OmitFailures = FALSE)
      # Save locally
      readr::write_csv(vol, vol_file)
    }

    vol
  }, error = function(e) {
    message(sprintf("  Error for %s: %s", rid, e$message))
    NULL
  })
})
names(volume_list) <- to_process$root_id
volume_list <- Filter(Negate(is.null), volume_list)

if (length(volume_list) == 0) {
  message("No volumes calculated. Nothing to update.")
  return(invisible())
}

volumes <- dplyr::bind_rows(volume_list)
message(sprintf("Calculated volumes for %d neurons", nrow(volumes)))

# Prepare seatable update
volume.update <- to_process %>%
  dplyr::select(`_id`, root_id, root_id_processed) %>%
  dplyr::left_join(volumes, by = "root_id") %>%
  dplyr::filter(!is.na(volume_nm3))

# Update root_id_processed tags
volume.update$root_id_processed <- sapply(seq_len(nrow(volume.update)), function(i) {
  set_processed_rootid(volume.update$root_id_processed[i], "volume", volume.update$root_id[i])
})

# Clean for seatable
volume.update[is.na(volume.update)] <- ""
volume.update$volume_nm3 <- as.numeric(volume.update$volume_nm3)

message(sprintf("Updating %d rows in seatable", nrow(volume.update)))
message("Columns to update: volume_nm3, root_id_processed")

# Push to seatable
crant_table_update_rows(
  df = volume.update %>% dplyr::select(`_id`, volume_nm3, root_id_processed),
  table = "CRANTb_meta",
  base = "CRANTb",
  append_allowed = FALSE,
  chunksize = 1000
)

message("### crantb: volume update complete ###")

})
