library(crantr)
library(ggplot2)
library(dplyr)
library(hemibrainr)
library(pheatmap)

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
crant.synapses.list <- lapply(names(crant.neurons.flow), function(id){
  x <- crant.neurons.flow[[id]]
  syns <- x$connectors
  syns$root_id <- id
  syns
})
crant.synapses <- do.call(rbind, crant.synapses.list)
crant.synapses <- as.data.frame(crant.synapses) %>%
  dplyr::filter(pre_id %in% crant.meta.alpns.ids,
                post_id %in% crant.meta.alpns.ids)

# Get look up
pre.lookup <- crant.synapses %>%
  dplyr::filter(prepost==0) %>%
  dplyr::distinct(connector_id, 
                  pre_id, post_id,
                  pre_label = Label)
post.lookup <- crant.synapses %>%
  dplyr::filter(prepost==1) %>%
  dplyr::distinct(connector_id, 
                  pre_id, post_id,
                  post_label = Label)
  
# Assemble edgelist
crant.el <- crant.synapses %>%
  dplyr::left_join(pre.lookup,
                   by = c("connector_id","pre_id","post_id")) %>%
  dplyr::left_join(post.lookup,
                     by = c("connector_id","pre_id","post_id")) %>%
  dplyr::group_by(post_id) %>%
  dplyr::mutate(total_input = sum(prepost==1)) %>%
  dplyr::ungroup() %>%
  dplyr::group_by(pre_id, post_id, pre_label, post_label) %>%
  dplyr::mutate(count = dplyr::n()) %>%
  dplyr::mutate(norm = count/total_input) %>%
  dplyr::ungroup()

# Axo-axonic connections
crant.el.aa <- crant.el %>%
  dplyr::filter(pre_label == 2, post_label == 2)

# Axo-dendritic connections
crant.el.ad <- crant.el %>%
  dplyr::filter(pre_label == 2, post_label == 3)

# plot
g <- nat.ggplot::gganat +
  nat.ggplot::geom_neuron(
    crantb.surf,
    rotation_matrix = crantr:::crant_rotation_matrices[["front"]],
    cols = c("grey95", "grey85"),
    alpha = 0.3
  ) +
  # nat.ggplot::geom_neuron(
  #   crant.meshes[1],
  #   rotation_matrix = crantr:::crant_rotation_matrices[["front"]],
  #   cols = c("grey60", "grey40"),
  #   alpha = 0.8
  # ) +
  nat.ggplot::geom_neuron(
    crant.neurons.flow,
    threshold = 15000,
    root = 2,
    size = 0.1,
    rotation_matrix = crantr:::crant_rotation_matrices[["front"]]
  ) +
  ggplot2::labs(
    title = "antennal lobe projection neurons",
    subtitle = "proofread, axon-dendrite split, CRANTb data"
  )

# Show
print(g)

# Save
ggsave(g,
       filename = "images/crantb_antennal_lobe_projection_neurons.png")


# Safe character IDs from integer64 to avoid precision issues in row/col names
safe_id <- function(x) base::as.character(bit64::as.integer64(x))

# 1) Collapse to one value per (pre_id, post_id) using COUNT
#    (your data has `count` assigned within the grouped edgelist)
summarise_pairwise <- function(df) {
  df %>%
    dplyr::group_by(pre_id, post_id) %>%
    dplyr::summarise(count = base::max(count, na.rm = TRUE), .groups = "drop")
}

aa_df <- summarise_pairwise(crant.el.aa)
ad_df <- summarise_pairwise(crant.el.ad)

# 2) Determine row/column universe and ordering source from AXO-AXONIC
row_ids <- base::sort(base::unique(safe_id(aa_df$pre_id)))
col_ids <- base::sort(base::unique(safe_id(aa_df$post_id)))

# 3) Build dense matrices with the same shape; fill missing pairs with 0
to_mat <- function(df, row_levels, col_levels, fill = 0) {
  df2 <- df %>%
    dplyr::transmute(pre = safe_id(pre_id), post = safe_id(post_id), value = count)
  
  grid <- tidyr::expand_grid(pre = row_levels, post = col_levels)
  
  df_full <- dplyr::left_join(grid, df2, by = c("pre", "post")) %>%
    dplyr::mutate(value = dplyr::if_else(base::is.na(value), fill, value))
  
  wide <- tidyr::pivot_wider(df_full, names_from = post, values_from = value)
  m <- base::as.matrix(dplyr::select(wide, -pre))
  base::rownames(m) <- wide$pre
  m
}

aa_mat <- to_mat(aa_df, row_levels = row_ids, col_levels = col_ids, fill = 0)
ad_mat <- to_mat(ad_df, row_levels = row_ids, col_levels = col_ids, fill = 0)

# 4) Orientation annotations (and consistent factor levels)
ann_row <- data.frame(Role = base::rep("Source", length(row_ids)))
base::rownames(ann_row) <- row_ids
ann_col <- data.frame(Role = base::rep("Target", length(col_ids)))
base::rownames(ann_col) <- col_ids

ann_row$Role <- base::factor(ann_row$Role, levels = c("Source", "Target"))
ann_col$Role <- base::factor(ann_col$Role, levels = c("Source", "Target"))

ann_colors <- list(
  Role = stats::setNames(viridisLite::viridis(2), c("Source", "Target"))
)

# 5) Shared viridis palette/breaks across BOTH heatmaps (for comparable colors)
max_val <- base::max(c(aa_df$count, ad_df$count), na.rm = TRUE)
if (!base::is.finite(max_val) || max_val <= 0) max_val <- 1
cols <- viridisLite::viridis(100)
brks <- base::seq(0, max_val, length.out = 101)

# 6) Cluster ONLY the axo-axonic matrix to get an order, then lock it in
aa_tmp <- pheatmap::pheatmap(
  aa_mat,
  color = cols, breaks = brks, na_col = "grey90", border_color = NA,
  cluster_rows = TRUE, cluster_cols = TRUE,
  silent = TRUE
)

row_order <- if (!base::is.null(aa_tmp$tree_row)) base::rownames(aa_mat)[aa_tmp$tree_row$order] else base::rownames(aa_mat)
col_order <- if (!base::is.null(aa_tmp$tree_col)) base::colnames(aa_mat)[aa_tmp$tree_col$order] else base::colnames(aa_mat)

# Reorder both matrices + annotations using the axo-axonic order
aa_mat_ord  <- aa_mat[row_order, col_order, drop = FALSE]
ad_mat_ord  <- ad_mat[row_order, col_order, drop = FALSE]
ann_row_ord <- ann_row[row_order, , drop = FALSE]
ann_col_ord <- ann_col[col_order, , drop = FALSE]

# 7) Final plots (fixed order, same scale), using COUNT
pheatmap::pheatmap(
  aa_mat_ord,
  color = cols, breaks = brks, na_col = "grey90", border_color = NA,
  cluster_rows = FALSE, cluster_cols = FALSE,
  annotation_row = ann_row_ord, annotation_col = ann_col_ord,
  annotation_colors = ann_colors,
  main = "Axo-axonic (source axon \u2192 target axon): synapse count",
  fontsize_row = 6, fontsize_col = 6
)

pheatmap::pheatmap(
  ad_mat_ord,
  color = cols, breaks = brks, na_col = "grey90", border_color = NA,
  cluster_rows = FALSE, cluster_cols = FALSE,
  annotation_row = ann_row_ord, annotation_col = ann_col_ord,
  annotation_colors = ann_colors,
  main = "Axo-dendritic (source axon \u2192 target dendrite): synapse count",
  fontsize_row = 6, fontsize_col = 6
)


# ---- File paths for PNG output ----
file_aa <- base::file.path("images", "heatmap_axo_axonic_count.png")
file_ad <- base::file.path("images", "heatmap_axo_dendritic_count.png")

# 7) SAVE to PNG (and optionally also display on-screen)
# Saving
pheatmap::pheatmap(
  aa_mat_ord,
  color = cols, breaks = brks, na_col = "grey90", border_color = NA,
  cluster_rows = FALSE, cluster_cols = FALSE,
  annotation_row = ann_row_ord, annotation_col = ann_col_ord,
  annotation_colors = ann_colors,
  main = "Axo-axonic (source axon \u2192 target axon): synapse count",
  fontsize_row = 6, fontsize_col = 6,
  filename = file_aa, width = 10, height = 8
)

pheatmap::pheatmap(
  ad_mat_ord,
  color = cols, breaks = brks, na_col = "grey90", border_color = NA,
  cluster_rows = FALSE, cluster_cols = FALSE,
  annotation_row = ann_row_ord, annotation_col = ann_col_ord,
  annotation_colors = ann_colors,
  main = "Axo-dendritic (source axon \u2192 target dendrite): synapse count",
  fontsize_row = 6, fontsize_col = 6,
  filename = file_ad, width = 10, height = 8
)





