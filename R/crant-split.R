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
  sql = "SELECT _id, root_id, status, position, super_class, cell_class, flow, root_id_processed FROM CRANTb_meta"
)
ac[ac == ""] <- NA
ac[ac == "0"] <- NA

# Filter non-neurons
ac <- ac %>%
  dplyr::filter(!is_non_neuron(status)) %>%
  dplyr::filter(!is.na(root_id)) %>%
  dplyr::distinct(root_id, .keep_all = TRUE)
ac$rule <- compartment_rule(ac$flow, ac$super_class)

###############################################
### Afferent / efferent neurons by rule     ###
###############################################

# Afferent neurons are wholly axon and efferent neurons wholly dendrite,
# whether or not they are proofread. Their arbors are truncated at the
# edge of the brain, so flow centrality cannot split them. Label every
# synapse from the raw synapse CSVs (crant-synapses.R) with the same
# filters as crant_add_synapses, without a skeleton.
ac.rule <- ac %>% dplyr::filter(!is.na(rule))
message(sprintf("Found %d afferent/efferent neurons to label by rule", nrow(ac.rule)))

cleanup_stale_files(ac.rule$root_id, ac.rule$root_id_processed, "split",
                    dir = split.folder, ext = ".swc")
cleanup_stale_files(ac.rule$root_id, ac.rule$root_id_processed, "split",
                    dir = synapses.folder, ext = ".csv")

rule.done <- character(0)
rule.rewritten <- character(0)
for (i in seq_len(nrow(ac.rule))) {
  rid <- ac.rule$root_id[i]
  label <- if (ac.rule$rule[i] == "afferent") 2L else 3L
  out_file <- file.path(synapses.folder, paste0(rid, ".csv"))
  swc_file <- file.path(split.folder, paste0(rid, ".swc"))
  tryCatch({
    # Up to date if already labelled by this rule for this root_id
    if (file.exists(out_file) && !file.exists(swc_file) &&
        identical(get_processed_rootid(ac.rule$root_id_processed[i], "split"), rid)) {
      old <- readr::read_csv(out_file, show_col_types = FALSE, progress = FALSE,
                             col_select = "Label")
      if (all(old$Label == label)) {
        rule.done <- c(rule.done, rid)
        next
      }
    }
    raw_file <- file.path(crant.synapses.save.path, paste0(rid, ".csv"))
    if (!file.exists(raw_file)) next
    raw <- readr::read_csv(raw_file, show_col_types = FALSE, progress = FALSE,
                           col_types = readr::cols(.default = readr::col_character()))
    # crant-synapses.R only writes this file when there are synapses, so the
    # columns are always present
    syns <- raw %>%
      dplyr::filter(as.numeric(size) > 5, pre_pt_root_id != post_pt_root_id) %>%
      dplyr::transmute(connector_id = id,
                       pre_id = pre_pt_root_id,
                       post_id = post_pt_root_id,
                       prepost = as.integer(prepost),
                       size = as.numeric(size),
                       status = "good",
                       Label = label)
    readr::write_csv(syns, out_file)
    # A split skeleton from an earlier flow-centrality split no longer applies
    if (file.exists(swc_file)) file.remove(swc_file)
    rule.done <- c(rule.done, rid)
    rule.rewritten <- c(rule.rewritten, rid)
  }, error = function(e) {
    message(sprintf("  Rule labelling failed for %s: %s", rid, e$message))
  })
}
message(sprintf("Labelled %d neurons by rule (%d rewritten)",
                length(rule.done), length(rule.rewritten)))

# Mark rule neurons as split, and have split metrics recomputed where the
# labels were rewritten
rule.update <- ac.rule %>%
  dplyr::filter(root_id %in% rule.done) %>%
  dplyr::select(`_id`, root_id, root_id_processed)
if (nrow(rule.update) > 0) {
  new_processed <- sapply(seq_len(nrow(rule.update)), function(i) {
    p <- set_processed_rootid(rule.update$root_id_processed[i], "split", rule.update$root_id[i])
    if (rule.update$root_id[i] %in% rule.rewritten) p <- clear_processed_rootid(p, "split_metrics")
    p
  })
  changed <- !mapply(identical, new_processed, rule.update$root_id_processed, USE.NAMES = FALSE)
  rule.update$root_id_processed <- new_processed
  rule.update <- rule.update[changed, ]
  if (nrow(rule.update) > 0) {
    message(sprintf("Updating root_id_processed for %d rule-labelled neurons", nrow(rule.update)))
    crant_table_update_rows(
      df = rule.update %>% dplyr::select(`_id`, root_id_processed),
      table = "CRANTb_meta",
      base = "CRANTb",
      append_allowed = FALSE,
      chunksize = 1000
    )
  }
}

###############################################
### Proofread neurons split by flow         ###
###############################################

# Filter to BACKBONE_PROOFREAD neurons not labelled by rule
ac <- ac %>%
  dplyr::filter(is.na(rule), is_backbone_proofread(status), !is.na(position))

message(sprintf("Found %d backbone-proofread neurons with positions", nrow(ac)))

if (nrow(ac) == 0) {
  message("No proofread neurons to split. Done.")
  return(invisible())
}

# Clean up stale files for neurons whose root_id has changed
cleanup_stale_files(ac$root_id, ac$root_id_processed, "split",
                    dir = split.folder, ext = ".swc")
cleanup_stale_files(ac$root_id, ac$root_id_processed, "split",
                    dir = synapses.folder, ext = ".csv")

###############################################
### Determine which neurons to process      ###
###############################################

# Check which neurons already have both split SWC and synapse CSV
# (stale files for old root_ids have been cleaned up above)
done.swc <- gsub("\\.swc$", "", list.files(split.folder))
done.syn <- gsub("\\.csv$", "", list.files(synapses.folder))
done.ids <- intersect(done.swc, done.syn)

# Need processing: output files missing for current root_id
to_process <- ac %>%
  dplyr::filter(!(root_id %in% done.ids))

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

    # Set id field on each neuron (after resampling, which drops custom fields)
    neurons <- crantr:::add_field_seq(neurons, entries = names(neurons), field = "id")

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
