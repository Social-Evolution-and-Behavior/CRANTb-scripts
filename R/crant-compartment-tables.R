###########################################################
### Create the CAVE compartment_synapses_pre_v2 and
### compartment_synapses_post_v2 tables (run once)
###
### Both are reference_tag tables referencing synapses_v2.
### They are populated nightly by
### crant-compartment-synapses.R
###########################################################
source("R/crant-startup.R")

local({

fcc <- crant_cave_client()
existing_tables <- unlist(reticulate::py_to_r(fcc$annotation$get_tables()))
meta <- reticulate::py_to_r(fcc$annotation$get_table_metadata("synapses_v2"))
voxel_resolution <- as.numeric(meta$voxel_resolution)

description <- paste(
  "Compartment of the %s neuron at each synapses_v2 synapse.",
  "target_id is the synapses_v2 id; tag is one of axon, dendrite,",
  "primary_dendrite, primary_neurite or soma.",
  "Afferent neurons are labelled wholly axon and efferent neurons wholly dendrite;",
  "for efferent neurons this includes their output synapses in the brain.",
  "Other BACKBONE_PROOFREAD neurons are split into axon and dendrite by flow centrality",
  "on L2 skeletons (hemibrainr). Synapses flagged as bad on the skeleton,",
  "and synapses with size <= 5 or autapses, are not labelled.",
  "Updated nightly by the CRANTb-R pipeline: labels for a neuron are replaced",
  "when its root_id changes, and removed if it loses proofread status or its",
  "flow class changes, so labels can lag edits by up to a day."
)

for (side in c("pre", "post")) {
  table_name <- sprintf("compartment_synapses_%s_v2", side)
  if (table_name %in% existing_tables) {
    message(sprintf("%s already exists", table_name))
    next
  }
  neuron <- if (side == "pre") "presynaptic" else "postsynaptic"
  fcc$annotation$create_table(
    table_name = table_name,
    schema_name = "reference_tag",
    description = sprintf(description, neuron),
    voxel_resolution = as.list(voxel_resolution),
    reference_table = "synapses_v2",
    track_target_id_updates = TRUE,
    write_permission = "PRIVATE",
    read_permission = "PUBLIC"
  )
  message(sprintf("Created %s", table_name))
}

})
