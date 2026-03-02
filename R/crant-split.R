###########################################################
### Split proofread CRANTb neurons into axon / dendrite
###
### Uses hemibrainr::flow_centrality() on L2 skeletons
### with synapses attached. Saves:
###   l2split/swc/{root_id}.swc       — skeleton + Label col
###   l2split/synapses/{root_id}.csv  — connectors + Label
###   l2split/metrics/                — segregation + compartment metrics
###
### Only processes BACKBONE_PROOFREAD neurons.
### Modelled on bancpipeline/banc/banc-split.R
###########################################################
source("R/crant-startup.R")

local({

###########################
### Output directories  ###
###########################

split.folder    <- file.path(crant.l2split.save.path, "swc")
synapses.folder <- file.path(crant.l2split.save.path, "synapses")
metrics.folder  <- file.path(crant.l2split.save.path, "metrics")
dir.create(split.folder,    recursive = TRUE, showWarnings = FALSE)
dir.create(synapses.folder, recursive = TRUE, showWarnings = FALSE)
dir.create(metrics.folder,  recursive = TRUE, showWarnings = FALSE)

###########################
### Read current state  ###
###########################

message("### crantb: querying seatable for proofread neurons ###")
ac <- crant_table_query(
  sql = "SELECT _id, root_id, status, position, super_class, cell_class, root_id_processed FROM CRANTb_meta"
)
ac[ac == ""] <- NA
ac[ac == "0"] <- NA

# Filter non-neurons
ac <- ac %>%
  dplyr::filter(!(grepl("DELETE|GLIA|NOT_A_NEURON|DEBRIS", status, ignore.case = TRUE) & !is.na(status))) %>%
  dplyr::filter(!is.na(root_id)) %>%
  dplyr::distinct(root_id, .keep_all = TRUE)

# Filter to BACKBONE_PROOFREAD only
ac$is_backbone_proofread <- sapply(ac$status, function(s) {
  if (is.na(s)) return(FALSE)
  entries <- trimws(unlist(strsplit(s, split = ",|, ")))
  "BACKBONE_PROOFREAD" %in% entries & !("NOT_BACKBONE_PROOFREAD" %in% entries)
})
ac <- ac %>% dplyr::filter(is_backbone_proofread, !is.na(position))

message(sprintf("Found %d backbone-proofread neurons with positions", nrow(ac)))

if (nrow(ac) == 0) {
  message("No proofread neurons to split. Done.")
  return(invisible())
}

# Determine which neurons need split update
ac$needs_update <- needs_metric_update(ac$root_id, ac$root_id_processed, "split")

# Clean up stale files for neurons whose root_id has changed
cleanup_stale_files(ac$root_id, ac$root_id_processed, "split",
                    dir = split.folder, ext = ".swc")
cleanup_stale_files(ac$root_id, ac$root_id_processed, "split",
                    dir = synapses.folder, ext = ".csv")

###############################################
### Determine which neurons to process      ###
###############################################

# Check which neurons already have both split SWC and synapse CSV
done.swc <- gsub("\\.swc$", "", list.files(split.folder))
done.syn <- gsub("\\.csv$", "", list.files(synapses.folder))
done.ids <- intersect(done.swc, done.syn)

# Need processing: needs_update OR missing output files
to_process <- ac %>%
  dplyr::filter(needs_update | !(root_id %in% done.ids))

# Check that L2 skeletons exist for these neurons
swc_files <- file.path(crant.l2swc.save.path, paste0(to_process$root_id, ".swc"))
has_swc <- file.exists(swc_files)
if (sum(!has_swc) > 0) {
  message(sprintf("  Skipping %d neurons without L2 skeleton SWC files", sum(!has_swc)))
}
to_process <- to_process[has_swc, ]

message(sprintf("%d neurons to split (already done: %d)", nrow(to_process), length(done.ids)))

if (nrow(to_process) == 0) {
  message("All splits are up to date. Nothing to do.")
  return(invisible())
}

# Identify sensory and motor neurons for label overrides
sensories <- to_process$root_id[sapply(seq_len(nrow(to_process)), function(i) {
  sc <- to_process$super_class[i]
  cc <- to_process$cell_class[i]
  any(grepl("sensory|afferent", c(sc, cc), ignore.case = TRUE), na.rm = TRUE)
})]
motors <- to_process$root_id[sapply(seq_len(nrow(to_process)), function(i) {
  sc <- to_process$super_class[i]
  cc <- to_process$cell_class[i]
  any(grepl("motor|efferent|endocrine|visceral", c(sc, cc), ignore.case = TRUE), na.rm = TRUE)
})]

###########################
### Process in batches  ###
###########################

message("### crantb: splitting neurons into axon/dendrite ###")

# Build position lookup (root_id → soma position in nm)
position_lookup <- to_process %>%
  dplyr::select(root_id, position) %>%
  dplyr::filter(!is.na(position))
position_lookup$position_clean <- gsub(" ", "", position_lookup$position)

# Batch neurons
batch_size <- 10
ids_to_process <- to_process$root_id
batches <- split(ids_to_process, ceiling(seq_along(ids_to_process) / batch_size))

message(sprintf("Processing %d neurons in %d batches of up to %d",
                length(ids_to_process), length(batches), batch_size))

for (batch_idx in seq_along(batches)) {
  batch_ids <- batches[[batch_idx]]
  message(sprintf("\n--- Batch %d/%d (%d neurons) ---", batch_idx, length(batches), length(batch_ids)))

  tryCatch({

    # a. Read SWC files
    swc_paths <- file.path(crant.l2swc.save.path, paste0(batch_ids, ".swc"))
    neurons <- nat::read.neurons(swc_paths, neuronnames = basename)
    names(neurons) <- gsub("\\.swc$", "", names(neurons))

    # Skip stub SWC files (single node, from failed L2 downloads)
    good <- sapply(neurons, function(n) nrow(n$d) > 1)
    if (sum(!good) > 0) {
      message(sprintf("  Skipping %d stub skeletons", sum(!good)))
    }
    neurons <- neurons[good]
    if (length(neurons) == 0) next

    # Set id field on each neuron
    neurons <- crantr:::add_field_seq(neurons, entries = names(neurons), field = "id")

    # b. Resample to 100nm
    neurons <- nat:::resample.neuronlist(neurons, stepsize = 100, OmitFailures = TRUE)

    # c. Re-root at soma position
    for (id in names(neurons)) {
      pos_row <- position_lookup[position_lookup$root_id == id, ]
      if (nrow(pos_row) > 0 && !is.na(pos_row$position_clean[1])) {
        pos_raw <- as.numeric(unlist(strsplit(pos_row$position_clean[1], ",")))
        if (length(pos_raw) == 3 && !any(is.na(pos_raw))) {
          soma_nm <- as.numeric(crant_raw2nm(matrix(pos_raw, ncol = 3)))
          tryCatch({
            neurons[[id]] <- nat::reroot(neurons[[id]], point = soma_nm)
            neurons[[id]]$tags$soma <- nat::rootpoints(neurons[[id]])
          }, error = function(e) {
            message(sprintf("  Could not reroot %s: %s", id, e$message))
          })
        }
      }
    }

    # d. Add synapses
    neurons <- nat::nlapply(neurons,
                            crant_add_synapses,
                            update.id = FALSE,
                            OmitFailures = TRUE)

    # e. Drop neurons with no synapses
    good_ids <- c()
    for (i in seq_along(neurons)) {
      conn <- neurons[[i]]$connectors
      if (!is.null(conn) && is.data.frame(conn) && nrow(conn) > 0) {
        good_ids <- c(good_ids, i)
      } else {
        message(sprintf("  No synapses for: %s", names(neurons)[i]))
      }
    }
    neurons <- neurons[good_ids]
    if (length(neurons) == 0) next

    # f. Flag bad synapses (pre-split)
    for (i in seq_along(neurons)) {
      tryCatch({
        somas <- !is.null(neurons[[i]]$tags$soma)
        rem <- hemibrainr::remove_bad_synapses(neurons[i],
                                                 meshes = NULL,
                                                 soma = somas,
                                                 min.nodes.from.soma = 150,
                                                 min.nodes.from.pnt = 10,
                                                 primary.branchpoint = 0.25,
                                                 OmitFailures = TRUE,
                                                 .parallel = FALSE,
                                                 wipe = TRUE)
        neurons[[i]] <- rem[[1]]
      }, error = function(e) {
        message(sprintf("  remove_bad_synapses pre-split failed for %s: %s",
                        names(neurons)[i], e$message))
      })
    }

    # g. Split: flow centrality
    neurons.flow <- hemibrainr::flow_centrality(neurons,
                                                 mode = mode,
                                                 polypre = polypre,
                                                 split = split,
                                                 .parallel = FALSE,
                                                 OmitFailures = FALSE)

    # h. Flag bad synapses (post-split, tighter thresholds)
    for (i in seq_along(neurons.flow)) {
      tryCatch({
        rem <- hemibrainr::remove_bad_synapses(neurons.flow[i],
                                                 meshes = NULL,
                                                 soma = TRUE,
                                                 min.nodes.from.soma = 50,
                                                 min.nodes.from.pnt = 5,
                                                 primary.branchpoint = 0.25,
                                                 OmitFailures = TRUE,
                                                 .parallel = FALSE,
                                                 wipe = TRUE)
        neurons.flow[[i]] <- rem[[1]]
      }, error = function(e) NULL)
    }
    neurons.flow <- neurons.flow[sapply(neurons.flow, nat::is.neuron)]

    if (length(neurons.flow) == 0) next

    # i. Override labels for sensory/motor neurons
    ids.sensories <- intersect(names(neurons.flow), sensories)
    if (length(ids.sensories)) {
      for (sid in ids.sensories) {
        neurons.flow[[sid]] <- hemibrainr::add_Label(neurons.flow[[sid]], Label = 2)
      }
    }
    ids.motors <- intersect(names(neurons.flow), motors)
    if (length(ids.motors)) {
      for (mid in ids.motors) {
        neurons.flow[[mid]] <- hemibrainr::add_Label(neurons.flow[[mid]], Label = 3)
      }
    }

    # j. Assign unlabeled nodes (Label=0) to nearest labelled node
    for (id in names(neurons.flow)) {
      neuron <- neurons.flow[[id]]
      d <- neuron$d
      labeled_nodes <- which(d$Label != 0)
      unlabeled_nodes <- which(d$Label == 0)

      if (length(unlabeled_nodes) > 0 && length(labeled_nodes) > 0) {
        g <- nat::as.ngraph(neuron)
        dm <- igraph::distances(g, v = unlabeled_nodes, to = labeled_nodes)
        closest_idx <- apply(dm, 1, which.min)
        d$Label[unlabeled_nodes] <- d$Label[labeled_nodes[closest_idx]]
        neuron$d <- d

        # Also update connector labels via nearest node
        syns.df <- neuron$connectors
        if (!is.null(syns.df) && nrow(syns.df) > 0) {
          node_coords <- as.matrix(neuron$d[, c("X", "Y", "Z")])
          syn_coords <- as.matrix(syns.df[, c("x", "y", "z")])
          if (ncol(syn_coords) == 0) {
            syn_coords <- as.matrix(syns.df[, c("X", "Y", "Z")])
          }
          nearest_node_idx <- RANN::nn2(node_coords, syn_coords, k = 1)$nn.idx[, 1]
          syns.df$Label <- neuron$d$Label[nearest_node_idx]
          neuron$connectors <- syns.df
        }
        neurons.flow[[id]] <- neuron
      }
    }

    # k. Assign Strahler order + carryover labels to connectors
    neurons.split <- assign_strahler(neurons.flow,
                                     OmitFailures = TRUE)
    neurons.split <- nat::nlapply(neurons.split,
                                  hemibrainr:::carryover_labels,
                                  .parallel = FALSE,
                                  OmitFailures = TRUE)
    neurons.split[, "root_id"] <- names(neurons.split)

    # l. Save split SWC files
    message("  Saving split SWC files...")
    capture.output(
      nat::write.neurons(nl = neurons.split,
                         dir = split.folder,
                         files = names(neurons.split),
                         Force = TRUE,
                         format = "swc",
                         include.data.frame = FALSE)
    )

    # m. Save synapse CSVs
    message("  Saving synapse CSVs...")
    for (n in names(neurons.split)) {
      csv <- neurons.split[n][[1]]$connectors
      if (!is.null(csv) && nrow(csv) > 0) {
        write.csv(x = csv,
                  file = file.path(synapses.folder, paste0(n, ".csv")),
                  row.names = FALSE)
      }
    }

    # n. Compute segregation index
    neurons.batch <- hemibrainr:::segregation_index.neuronlist(neurons.split)

    # o. Compute compartment metrics
    neurons.microns <- hemibrainr::scale_neurons(neurons.batch,
                                                  scaling = 1 / 1000,
                                                  .parallel = FALSE,
                                                  OmitFailures = TRUE)
    neurons.microns[, ] <- neurons.microns[, "root_id", drop = FALSE]

    mets <- tryCatch(
      hemibrainr::hemibrain_compartment_metrics(neurons.microns,
                                                 OmitFailures = TRUE,
                                                 .parallel = FALSE,
                                                 delta = 5,
                                                 resample = NULL,
                                                 locality = FALSE),
      error = function(e) {
        message(sprintf("  compartment_metrics failed: %s", e$message))
        NULL
      }
    )

    if (!is.null(mets)) {
      colnames(mets) <- snakecase::to_snake_case(colnames(mets))
      mets$root_id <- mets$id
      mets$id <- NULL
      mets <- mets[!duplicated(mets$root_id), ]
      mets <- round_dataframe(mets, digits = 4)

      # p. Save metrics CSV
      nmets <- length(list.files(metrics.folder)) + 1
      readr::write_csv(mets,
                       file = file.path(metrics.folder,
                                        sprintf("crant_metrics_%d.csv", nmets)),
                       col_names = TRUE)
      message(sprintf("  Metrics saved for %d neurons", nrow(mets)))
    }

    message(sprintf("  Batch %d complete: %d neurons split", batch_idx, length(neurons.split)))

  }, error = function(e) {
    message(sprintf("  Batch %d failed: %s", batch_idx, e$message))
  })
}

###############################################
### Update root_id_processed in seatable    ###
###############################################

# Re-check which neurons now have split output
done.swc <- gsub("\\.swc$", "", list.files(split.folder))
done.syn <- gsub("\\.csv$", "", list.files(synapses.folder))
newly_done <- intersect(done.swc, done.syn)

# Update root_id_processed for all neurons that have been split
split.update <- ac %>%
  dplyr::filter(root_id %in% newly_done) %>%
  dplyr::select(`_id`, root_id, root_id_processed)

if (nrow(split.update) > 0) {
  split.update$root_id_processed <- sapply(seq_len(nrow(split.update)), function(i) {
    set_processed_rootid(split.update$root_id_processed[i], "split", split.update$root_id[i])
  })

  split.update[is.na(split.update)] <- ""

  message(sprintf("Updating root_id_processed for %d split neurons", nrow(split.update)))
  crant_table_update_rows(
    df = split.update %>% dplyr::select(`_id`, root_id_processed),
    table = "CRANTb_meta",
    base = "CRANTb",
    append_allowed = FALSE,
    chunksize = 1000
  )
}

message("### crantb: neuron splitting complete ###")

})
