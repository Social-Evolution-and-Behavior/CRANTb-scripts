############################################
### Download L2 skeletons for CRANTb neurons
############################################
source("R/crant-startup.R")

local({

# Query seatable for all neurons
message("### crantb: querying seatable for neurons ###")
ac <- crant_table_query(
  sql = "SELECT _id, root_id, supervoxel_id, position, status, root_id_processed, volume_nm3 FROM CRANTb_meta"
)
ac[ac == ""] <- NA
ac[ac == "0"] <- NA

# Filter out non-neurons
ac <- ac %>%
  dplyr::filter(!(grepl("DELETE|GLIA|NOT_A_NEURON|DEBRIS", status, ignore.case = TRUE) & !is.na(status))) %>%
  dplyr::filter(!is.na(root_id), volume_nm3 > 10) %>%
  dplyr::distinct(root_id, .keep_all = TRUE)

message(sprintf("Found %d neurons in seatable", nrow(ac)))

# Determine which neurons need L2 skeleton download
# A neuron needs download if no SWC file exists for its current root_id.
# (root_id_processed tracking is handled by crant-l2-metrics.R, not here)
ac$needs_download <- !file.exists(file.path(crant.l2swc.save.path, paste0(ac$root_id, ".swc")))

# Clean up stale SWC files for neurons whose root_id has changed
cleanup_stale_files(ac$root_id, ac$root_id_processed, "l2",
                    dir = crant.l2swc.save.path, ext = ".swc")

neuron.ids <- ac$root_id[ac$needs_download]
message(sprintf("%d neurons need L2 skeleton download", length(neuron.ids)))

if (length(neuron.ids) == 0) {
  message("All L2 skeletons are up to date. Nothing to do.")
  return(invisible())
}

# Download L2 skeletons with per-neuron timeout (10 min)
crantr::choose_crant()
dir.create(crant.l2swc.save.path, showWarnings = FALSE, recursive = TRUE)

message(sprintf("### crantb: downloading %d L2 skeletons (10 min timeout per neuron) ###",
                length(neuron.ids)))
n_ok <- 0
n_fail <- 0

for (i in seq_along(neuron.ids)) {
  rid <- neuron.ids[i]
  swc_file <- file.path(crant.l2swc.save.path, paste0(rid, ".swc"))

  if (i %% 50 == 1) {
    message(sprintf("  Progress: %d/%d (ok: %d, fail: %d)", i, length(neuron.ids), n_ok, n_fail))
  }

  ok <- tryCatch({
    R.utils::withTimeout({
      skel <- crant_read_l2skel(rid, OmitFailures = TRUE)
      if (length(skel) > 0) {
        nat::write.neurons(skel, file = names(skel),
                           dir = crant.l2swc.save.path,
                           format = 'swc', Force = TRUE)
        TRUE
      } else {
        # Too few L2 nodes etc. — write stub so metrics script records l2_nodes=1, cable_length=0
        writeLines("1 1 0 0 0 0 -1", swc_file)
        FALSE
      }
    }, timeout = 600, onTimeout = "error")
  }, error = function(e) {
    msg <- if (grepl("timeout|elapsed", e$message, ignore.case = TRUE))
      sprintf("  TIMEOUT: %s (>10 min)", rid)
    else
      sprintf("  Error: %s — %s", rid, e$message)
    message(msg)
    writeLines("1 1 0 0 0 0 -1", swc_file)
    FALSE
  })

  if (ok) n_ok <- n_ok + 1 else n_fail <- n_fail + 1
}

message(sprintf("### crantb: L2 download complete — %d ok, %d fail ###", n_ok, n_fail))

})
