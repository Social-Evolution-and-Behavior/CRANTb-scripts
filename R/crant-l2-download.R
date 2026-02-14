############################################
### Download L2 skeletons for CRANTb neurons
############################################
source("R/crant-startup.R")

local({

# Query seatable for all neurons
message("### crantb: querying seatable for neurons ###")
ac <- crant_table_query(
  sql = "SELECT _id, root_id, supervoxel_id, position, status, root_id_processed FROM CRANTb_meta"
)
ac[ac == ""] <- NA
ac[ac == "0"] <- NA

# Filter out non-neurons
ac <- ac %>%
  dplyr::filter(!(grepl("DELETE|GLIA|NOT_A_NEURON|DEBRIS", status, ignore.case = TRUE) & !is.na(status))) %>%
  dplyr::filter(!is.na(root_id)) %>%
  dplyr::distinct(root_id, .keep_all = TRUE)

message(sprintf("Found %d neurons in seatable", nrow(ac)))

# Determine which neurons need L2 skeleton download
# A neuron needs download if:
#   1. No SWC file exists for its current root_id, OR
#   2. The root_id has changed since last L2 processing
ac$needs_download <- sapply(seq_len(nrow(ac)), function(i) {
  rid <- ac$root_id[i]
  swc_file <- file.path(crant.l2swc.save.path, paste0(rid, ".swc"))
  if (!file.exists(swc_file)) return(TRUE)
  # Also check root_id_processed tag
  processed <- ac$root_id_processed[i]
  stored <- get_processed_rootid(processed, "l2")
  if (is.na(stored)) return(TRUE)
  stored != as.character(rid)
})

# Clean up stale SWC files for neurons whose root_id has changed
cleanup_stale_files(ac$root_id, ac$root_id_processed, "l2",
                    dir = crant.l2swc.save.path, ext = ".swc")

neuron.ids <- ac$root_id[ac$needs_download]
message(sprintf("%d neurons need L2 skeleton download", length(neuron.ids)))

if (length(neuron.ids) == 0) {
  message("All L2 skeletons are up to date. Nothing to do.")
  return(invisible())
}

# Download L2 skeletons in batches
batch_size <- 50
n_batches <- ceiling(length(neuron.ids) / batch_size)

for (b in seq_len(n_batches)) {
  start_idx <- (b - 1) * batch_size + 1
  end_idx <- min(b * batch_size, length(neuron.ids))
  batch_ids <- neuron.ids[start_idx:end_idx]

  message(sprintf("Batch %d/%d: downloading %d skeletons (%d-%d of %d)",
                  b, n_batches, length(batch_ids), start_idx, end_idx, length(neuron.ids)))

  # Download
  tryCatch({
    crant.l2.skels <- crant_read_l2skel(batch_ids, OmitFailures = TRUE)

    if (length(crant.l2.skels) > 0) {
      # Save as SWC files
      successful_ids <- names(crant.l2.skels)
      dir.create(crant.l2swc.save.path, showWarnings = FALSE, recursive = TRUE)
      nat::write.neurons(crant.l2.skels,
                         file = successful_ids,
                         dir = crant.l2swc.save.path,
                         format = 'swc',
                         Force = TRUE)
      message(sprintf("  Saved %d skeletons to %s", length(successful_ids), crant.l2swc.save.path))
    }

    # For neurons that failed (too few L2 nodes etc.), write a stub SWC
    # so the metrics script can record them as l2_nodes=1, cable_length=0
    failed_ids <- setdiff(batch_ids, names(crant.l2.skels))
    if (length(failed_ids) > 0) {
      dir.create(crant.l2swc.save.path, showWarnings = FALSE, recursive = TRUE)
      for (fid in failed_ids) {
        stub_file <- file.path(crant.l2swc.save.path, paste0(fid, ".swc"))
        writeLines("1 1 0 0 0 0 -1", stub_file)
      }
      message(sprintf("  Wrote %d stub SWC files for failed neurons", length(failed_ids)))
    }
  }, error = function(e) {
    message(sprintf("  Error in batch %d: %s", b, e$message))
  })
}

message("### crantb: L2 skeleton download complete ###")

})
