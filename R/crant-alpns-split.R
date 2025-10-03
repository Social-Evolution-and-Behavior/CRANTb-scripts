library(crantr)
library(ggplot2)
library(dplyr)
library(hemibrainr)
library(pheatmap)
library(elmr)

# Look at CRANT data only
choose_crant()

# Get CRANTb meta data
crant.meta <- crant_table_query()

# Subset CRANTb meta data
crant.meta.alpns <- crant.meta %>%
  dplyr::filter(cell_class == "olfactory_projection_neuron", 
                side == "right",
                proofread) %>%
  dplyr::distinct(root_id, .keep_all = TRUE)
crant.meta.alpns.ids <- na.omit(crant.meta.alpns$root_id)

# get meshes 
crant.meshes <- crant_read_neuron_meshes(crant.meta.alpns.ids)
crant.skels <- skeletor(segment = crant.meta.alpns.ids, 
                   clean = TRUE,
                   method = "wavefront",
                   save.obj = NULL, 
                   mesh3d = FALSE,
                   waves = 1,
                   k.soma.search = 100,
                   radius.soma.search = 10000,
                   heal = TRUE,
                   reroot = TRUE,
                   heal.threshold = 1000000,
                   heal.k = 10L,
                   reroot_method = "density",
                   brain = bancr::banc_neuropil.surf,
                   elapsed = 10000,
                   resample = 1000)

# # Get the L2 skeletons
# crant.skels <- crant_read_l2skel(crant.meta.alpns.ids)

# Add synapse information, stored at n.syn[[1]]$connectors
crant.skels.syns <- crant_add_synapses(crant.skels)

# Split neuron
crant.neurons.flow <- hemibrainr::flow_centrality(crant.skels.syns)

# Get synapses
crant.synapses <- lapply(names(crant.neurons.flow), function(id){
  x <- crant.neurons.flow[[id]]
  syns <- x$connectors
  syns$root_id <- x
  syns
})
crant.synapses <- do.call(rbind, crant.synapses.list)
crant.synapses <- as.data.frame(crant.synapses)

# Get look up
lookup <- crant.synapses %>%
  dplyr::filter(prepost==0) %>%
  dplyr::distinct(connector_id, 
                  pre_label = label)
  
# Assemble edgelist
crant.el <- crant.synapses %>%
  dplyr::filter(pre %in% crant.meta.alpns.ids,
                post %in% crant.meta.alpns.ids)
  dplyr::left_join(lookup, 
                   by = "connector_id") %>%
  dplyr::mutate(pre_label )
  dplyr::group_by(pre, post) %>%
  dplyr::mutate(total_input = sum(prepost==1)) %>%
  dplyr::ungroup() %>%
  dplyr::group_by(pre, post, label) %>%
  dplyr::mutate(count = dplyr::n) %>%
  dplyr::mutate(norm = count/total_input) %>%
  dplyr::ungroup()

# Axo-axonic connections
crant.el.aa <- crant.el %>%
  dplyr::filter(pre_label == 2, post_label == 2)

# Axo-dendritic connections
crant.el.ad <- crant.el %>%
  dplyr::filter(pre_label == 2, post_label == 3)








