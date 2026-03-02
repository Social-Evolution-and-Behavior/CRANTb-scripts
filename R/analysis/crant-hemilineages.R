library(crantr)
library(ggplot2)
library(dplyr)
library(hemibrainr)
library(bancr)

crant.meta <- crant_table_query()
fafb.meta <- franken_meta() %>%
  dplyr::filter(!is.na(fafb_id))

hemilineages <- c("EBa1","LALv1_dorsal","DM","DL1_dorsal","WEDd1","AOTUv4_ventral")
for(hl in hemilineages){
  try({
    ### CRANTb ###
    
    # Get meta
    # choose_crant()
    crant.meta.hl <- crant.meta %>%
      dplyr::filter(hemilineage == hl) %>%
      dplyr::distinct(root_id, .keep_all = TRUE)
    crant.meta.hl.ids <- na.omit(crant.meta.hl$root_id)
    
    # Plot neurons
    crant.meshes <- crant_read_neuron_meshes(crant.meta.hl.ids)
    g <- crant_ggneuron(x = crant.meshes,
                        cols1 = c("gold","darkorange"), 
                        volume = crantb.surf)
    
    # Save
    ggsave(g,
           filename = sprintf("images/crantb_%s.png",hl),
           width = 6, height = 6, dpi = 300, bg = "transparent")
    
    ### FAFB ###
    
    # Get meta
    fafb.meta.hl <- fafb.meta %>%
      dplyr::filter(hemilineage == hl) %>%
      dplyr::distinct(fafb_id, .keep_all = TRUE)
    fafb.meta.hl.ids <- na.omit(fafb.meta.hl$fafb_id)
    
    # Plot neurons
    # choose_segmentation("flywire31")
    fafb.meshes <- read_cloudvolume_meshes(fafb.meta.hl.ids)
    g <- nat.ggplot::ggneuron(x = fafb.meshes, 
                              volume = FAFB14.surf, 
                              info = NULL, 
                              rotation_matrix =  structure(c(0.994118511676788, 0.0208597891032696, 
                                                             -0.106269955635071, 0, 0.0318574905395508, -0.994184970855713, 
                                                             0.102867424488068, 0, -0.103506252169609, -0.105647884309292, 
                                                             -0.989001929759979, 0, 0, 0, 0, 1), dim = c(4L, 4L)), 
                              cols1 = c("red","darkred"), 
                              cols2 = c("grey75", "grey50"), 
                              alpha = 0.5, 
                              title.col = "darkgrey")
    
    # Save
    ggsave(g,
           filename = sprintf("images/fafb_%s.png",hl))
    
  })
}

crant.meta.nt <- subset(crant.meta, !is.na(known_nt))
crant.meta.nt.ids <- na.omit(crant.meta.nt$root_id)
table(crant.meta.nt$known_nt)

# Plot neurons
crant.meshes <- crant_read_neuron_meshes(crant.meta.nt.ids)
crant.meta.nt <- crant.meta.nt %>%
  dplyr::filter(root_id %in% names(crant.meshes))
nt.cols <- c(acetylcholine = "orange", dopamine = "brown", gaba = "blue", glutamate = "green", octopamine = "purple", serotonin = "gold")
cols <- nt.cols[crant.meta.nt$known_nt]
cols[is.na(cols)] <- "grey"
cols <- unname(cols)
g <- crant_ggneuron(x = crant.meshes,
                    cols1 = cols, 
                    volume = crantb.surf)

# Save
ggsave(g,
       filename = sprintf("images/crantb_known_nt.png"),
       width = 6, height = 6, dpi = 300, bg = "transparent")

