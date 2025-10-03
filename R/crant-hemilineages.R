library(crantr)
library(ggplot2)
library(dplyr)
library(hemibrainr)
library(bancr)

crant.meta <- crant_table_query()
fafb.meta <- franken_meta() %>%
  dplyr::filter(!is.na(fafb_id))

hemilineages <- c("DL1_dorsal","WEDd1","DL1_dorsal","AOTUv4_ventral")
for(hl in hemilineages){
  
  ### CRANTb ###
  
  # Get meta
  # choose_crant()
  crant.meta.hl <- crant.meta %>%
    dplyr::filter(hemilineage == hl,
                  side == "right") %>%
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
    dplyr::filter(hemilineage == hl,
                  side == "right") %>%
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

}



