###########################################################
### Pull synapse counts and push input_connections /
### output_connections to seatable for CRANTb neurons
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

# Determine which neurons need synapse update
ac$needs_update <- needs_metric_update(ac$root_id, ac$root_id_processed, "synapses")

# Clean up stale synapse CSVs for neurons whose root_id has changed
cleanup_stale_files(ac$root_id, ac$root_id_processed, "synapses",
                    dir = crant.synapses.save.path, ext = ".csv")

to_process <- ac %>% dplyr::filter(needs_update)
message(sprintf("%d neurons need synapse count update", nrow(to_process)))

if (nrow(to_process) == 0) {
  message("All synapse counts are up to date. Nothing to do.")
  return(invisible())
}

# Pull synapse data for each neuron
message("### crantb: pulling synapse counts ###")
dir.create(crant.synapses.save.path, showWarnings = FALSE, recursive = TRUE)
synapse_list <- list()

for (i in seq_len(nrow(to_process))) {
  rid <- to_process$root_id[i]
  syn_file <- file.path(crant.synapses.save.path, paste0(rid, ".csv"))

  tryCatch({
    # Check for cached synapse file with current root_id
    if (file.exists(syn_file)) {
      syns <- readr::read_csv(syn_file, show_col_types = FALSE, progress = FALSE)
    } else {
      # Query CAVE for input synapses
      in.syns <- crant_partners(rid, partners = "input")
      if (nrow(in.syns) > 0) {
        in.syns$prepost <- 1L
      }

      # Query CAVE for output synapses
      out.syns <- crant_partners(rid, partners = "output")
      if (nrow(out.syns) > 0) {
        out.syns$prepost <- 0L
      }

      # Combine
      syns <- plyr::rbind.fill(in.syns, out.syns)

      # Save locally
      if (nrow(syns) > 0) {
        readr::write_csv(syns, syn_file)
      }
    }

    # Count connections
    input_connections <- sum(syns$prepost == 1, na.rm = TRUE)
    output_connections <- sum(syns$prepost == 0, na.rm = TRUE)

    synapse_list[[rid]] <- data.frame(
      root_id = rid,
      input_connections = input_connections,
      output_connections = output_connections,
      stringsAsFactors = FALSE
    )
  }, error = function(e) {
    message(sprintf("  Error for %s: %s", rid, e$message))
  })

  if (i %% 50 == 0) message(sprintf("  Processed %d/%d", i, nrow(to_process)))
}

if (length(synapse_list) == 0) {
  message("No synapse data retrieved. Nothing to update.")
  return(invisible())
}

synapse_counts <- dplyr::bind_rows(synapse_list)
message(sprintf("Got synapse counts for %d neurons", nrow(synapse_counts)))

# Prepare seatable update
synapse.update <- to_process %>%
  dplyr::select(`_id`, root_id, root_id_processed) %>%
  dplyr::left_join(synapse_counts, by = "root_id") %>%
  dplyr::filter(!is.na(input_connections))

# Update root_id_processed tags
synapse.update$root_id_processed <- sapply(seq_len(nrow(synapse.update)), function(i) {
  set_processed_rootid(synapse.update$root_id_processed[i], "synapses", synapse.update$root_id[i])
})

# Clean for seatable
synapse.update[is.na(synapse.update)] <- ""
synapse.update$input_connections <- as.numeric(synapse.update$input_connections)
synapse.update$output_connections <- as.numeric(synapse.update$output_connections)

message(sprintf("Updating %d rows in seatable", nrow(synapse.update)))
message("Columns to update: input_connections, output_connections, root_id_processed")

# Push to seatable
crant_table_update_rows(
  df = synapse.update %>% dplyr::select(`_id`, input_connections, output_connections, root_id_processed),
  table = "CRANTb_meta",
  base = "CRANTb",
  append_allowed = FALSE,
  chunksize = 1000
)

message("### crantb: synapse count update complete ###")

})
