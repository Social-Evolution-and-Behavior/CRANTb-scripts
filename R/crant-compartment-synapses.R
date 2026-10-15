###########################################################
### Compile compartment labels for synapses between
### labelled neurons (local table)
###
### For each synapses_v2 synapse whose presynaptic AND
### postsynaptic neurons are both labelled, records the
### compartment it sits on in each neuron. Labelled neurons:
###   - afferent neurons: every synapse "axon" (by rule)
###   - efferent neurons: every synapse "dendrite" (by rule)
###   - other BACKBONE_PROOFREAD neurons: flow-centrality split
### Labels come from l2split/synapses/{root_id}.csv, written
### by crant-split.R. Synapses flagged as bad are skipped.
###
### Writes, under crant.save.path/compartments/:
###   compartment_synapses_v2.csv  one row per synapse:
###     synapse_id      synapses_v2 id
###     pre_root_id,  pre_compartment,  pre_method
###     post_root_id, post_compartment, post_method
###   compartment = axon, dendrite, primary_dendrite,
###                 primary_neurite or soma
###   method      = flow_centrality, afferent or efferent
###   changes.csv  one row per run: synapses added, removed
###                and relabelled since the previous run
###
### Rebuilt in full each run, so labels follow edits to
### either neuron, demotions and flow class changes.
### Kept local for now; R/crant-compartment-tables.R creates
### the matching CAVE tables once synapses_v2 can be
### referenced there.
###########################################################
source("R/crant-startup.R")

local({

compartment_tags <- c("axon", "dendrite", "primary_dendrite",
                      "primary_neurite", "soma")
synapses.folder <- file.path(crant.l2split.save.path, "synapses")
compartments.folder <- file.path(crant.save.path, "compartments")
out.file <- file.path(compartments.folder, "compartment_synapses_v2.csv")
changes.file <- file.path(compartments.folder, "changes.csv")
chr_cols <- readr::cols(.default = readr::col_character())
dir.create(compartments.folder, recursive = TRUE, showWarnings = FALSE)

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
labelled$method <- ifelse(is.na(labelled$rule), "flow_centrality", labelled$rule)
message(sprintf("Found %d labelled neurons", nrow(labelled)))

###############################################
### Labels per synapse side                 ###
###############################################

message("### crantb: reading compartment labels ###")
syns <- dplyr::bind_rows(lapply(seq_len(nrow(labelled)), function(i) {
  x <- readr::read_csv(labelled$csv[i], col_types = chr_cols, progress = FALSE)
  if (!nrow(x) || !all(c("connector_id", "prepost", "status", "Label") %in% colnames(x))) return(NULL)
  x <- x[, c("connector_id", "prepost", "status", "Label")]
  x$root_id <- labelled$root_id[i]
  x$method <- labelled$method[i]
  x$mtime <- file.mtime(labelled$csv[i])
  x
}))
sides <- syns %>%
  dplyr::filter(status %in% "good", !is.na(connector_id)) %>%
  dplyr::transmute(synapse_id = connector_id,
                   side = ifelse(prepost == "0", "pre", "post"),
                   root_id, method, mtime,
                   compartment = gsub("\\.", "_", hemibrainr::standard_compartments(as.integer(Label)))) %>%
  dplyr::filter(compartment %in% compartment_tags) %>%
  # If two neurons' labels claim the same side of a synapse, one is stale:
  # keep the most recently written
  dplyr::arrange(dplyr::desc(mtime)) %>%
  dplyr::distinct(synapse_id, side, .keep_all = TRUE) %>%
  dplyr::select(-mtime)
rm(syns)

# Only synapses labelled on both sides, i.e. between two labelled neurons
pre <- sides %>% dplyr::filter(side == "pre") %>% dplyr::select(-side) %>%
  dplyr::rename(pre_root_id = root_id, pre_compartment = compartment, pre_method = method)
post <- sides %>% dplyr::filter(side == "post") %>% dplyr::select(-side) %>%
  dplyr::rename(post_root_id = root_id, post_compartment = compartment, post_method = method)
comp <- dplyr::inner_join(pre, post, by = "synapse_id") %>%
  dplyr::select(synapse_id, pre_root_id, pre_compartment, pre_method,
                post_root_id, post_compartment, post_method) %>%
  dplyr::arrange(as.numeric(synapse_id))
message(sprintf("%d synapses between labelled neurons", nrow(comp)))

###########################
### Save                ###
###########################

# Changes since the previous run
old <- if (file.exists(out.file)) {
  readr::read_csv(out.file, col_types = chr_cols, progress = FALSE)
} else {
  comp[0, ]
}
key <- function(x) paste(x$synapse_id, x$pre_compartment, x$post_compartment)
changes <- data.frame(
  date = format(Sys.time(), "%Y-%m-%d %H:%M"),
  synapses = nrow(comp),
  neurons = length(unique(c(comp$pre_root_id, comp$post_root_id))),
  added = sum(!(comp$synapse_id %in% old$synapse_id)),
  removed = sum(!(old$synapse_id %in% comp$synapse_id)),
  relabelled = sum(comp$synapse_id %in% old$synapse_id & !(key(comp) %in% key(old)))
)
message(sprintf("Since last run: %d added, %d removed, %d relabelled",
                changes$added, changes$removed, changes$relabelled))

# Write to a temporary file first, so a failed run never leaves a partial table
tmp <- paste0(out.file, ".tmp")
readr::write_csv(comp, tmp)
file.rename(tmp, out.file)
readr::write_csv(changes, changes.file, append = file.exists(changes.file))
message(sprintf("Saved %s", out.file))
message("### crantb: compartment synapse table complete ###")

})
