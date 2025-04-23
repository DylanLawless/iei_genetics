
# v4 ----
library(ggplot2)
library(ggraph)
library(dplyr)
library(tidyr)
library(igraph)
library(ggrepel)
library(scales)
set.seed(666)

df <- readRDS("../output/df_processed.Rds")

# Load STRING data and protein info
string_db <- readRDS(file = "../data/string_data_700.rds")
string_id_df <- data.table::fread('../data/9606.protein.info.v11.5.txt.gz') %>%
  dplyr::select(STRING_ID = "#string_protein_id", Gene_ID = preferred_name)

# select the unique genes from df
selected_genes <- unique(df$`Genetic defect`) 

# join df score into protein info
score_df <- df %>% dplyr::select(`Genetic defect`, score_positive_total) %>% distinct()
string_id_df <- left_join(string_id_df, score_df, by = c("Gene_ID" = "Genetic defect"))
string_id_df <- string_id_df %>% filter(Gene_ID %in% selected_genes)

# get the STRING interactions for the selected proteins
selected_interactions <- string_db$get_interactions(string_id_df$STRING_ID)

# create an igraph object from the interactions
graph <- graph_from_data_frame(selected_interactions[, c("from", "to")])
V(graph)$group <- "groupX"

# assign protein names and gene symbols
protein_names <- unique(c(selected_interactions$from, selected_interactions$to))
V(graph)$name <- protein_names[match(V(graph)$name, protein_names)]
V(graph)$gene_symbol <- string_id_df$Gene_ID[match(V(graph)$name, string_id_df$STRING_ID)]

# set edge weights using the combined score
E(graph)$weight <- selected_interactions$combined_score
weights <- E(graph)$weight

# compute degrees and normalise for node sizes
V(graph)$degree <- degree(graph, mode = "all")
normalise <- function(x, to = c(3, 15)) {
  if(max(x) == min(x)) return(rep(1, length(x)))
  (x - min(x)) / (max(x) - min(x)) * (to[2] - to[1]) + to[1]
}
V(graph)$size <- normalise(V(graph)$degree)

# adjust text size based on graph density
graph_density <- ecount(graph) / ((vcount(graph) * (vcount(graph) - 1)) / 2)
text_size <- (1 / (4 - graph_density)) * 24

# map df$score_positive_total to a colour gradient (yellow to red) with log transformation
# map df$score_positive_total to a colour gradient (lightblue to red) with log transformation
vertex_scores <- string_id_df$score_positive_total[match(V(graph)$gene_symbol, string_id_df$Gene_ID)]
V(graph)$score <- vertex_scores
vertex_scores_log <- log(V(graph)$score + 1)
max_log <- max(vertex_scores_log, na.rm = TRUE)
min_log <- min(vertex_scores_log, na.rm = TRUE)
V(graph)$score_norm <- (vertex_scores_log - min_log) / (max_log - min_log)
color_palette <- colorRampPalette(c("lightyellow", "orange", "red", "darkred"))(100)
V(graph)$color <- color_palette[as.integer(round(V(graph)$score_norm * 99)) + 1]

# compute the Kamada-Kawai layout and expand it by a scaling factor
layout_coords <- layout_with_kk(graph)
scaling_factor <- 5  # adjust this factor to expand the layout
layout_expanded <- layout_coords * scaling_factor

# layout_df ----
# create layout tibble and order by score_norm so that highest come last (on top)
layout_df <- create_layout(graph, layout = "manual", x = layout_expanded[,1], y = layout_expanded[,2]) %>%
  arrange(score_norm) %>%
  mutate(size_color = score_norm * (15 - 3) + 3)

layout_df <- layout_df %>% 
  mutate(label = if_else(rank(-score_norm) <= 15, gene_symbol, ""))

library(ggrepel)

p_net <- ggraph(layout_df) +
  geom_edge_link(aes(width = weights), alpha = 0.3, colour = "grey50") +
  scale_edge_width(range = c(0.1, 0.4), guide = "none") +
  # Add artificial outline behind nodes with a slightly larger size
  geom_node_point(aes(size = size_color * 1.2), colour = "black", alpha = 0.8) +
  # Main node layer coloured by score
  geom_node_point(aes(size = size_color, colour = color), alpha = 0.8) +
  scale_size_continuous(range = c(3, 15), guide = "none") +
  scale_y_continuous(expand = expansion(mult = c(0.1, 0.1)), limits = c(NA, NA)) +
  scale_color_identity(guide = "none") +
  # Use geom_text_repel from ggrepel to label top 15 nodes
 geom_label_repel(data = layout_df, aes(x = x, y = y, label = label, fill = alpha("white", 0.50)),
                   size = text_size,
                   colour = "black",
                   max.overlaps = Inf,
                   force = 20,
                   min.segment.length = 0) +
  scale_fill_identity() +
  theme_minimal()+
  theme(
    base_size = 20,
    text = element_text(family = "sans"),
    plot.title = element_text(size = 20),
    plot.background = element_rect(fill = "white", colour = "white"),
    panel.background = element_rect(fill = "white", colour = "white"),
    legend.background = element_rect(fill = "white", colour = "white")
  ) +
  theme(panel.grid = element_blank(),
        axis.title = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank())

# publication version with a legend ----
# prepare breakpoints
sz    <- layout_df$size_color
sz_br <- c(min(sz), median(sz), max(sz))
sz_lb <- round(sz_br, 1)

sn    <- layout_df$score_norm
sn_br <- c(0, median(sn), 1)
sn_lb <- round(sn_br, 2)

p_net <- ggraph(layout_df) +
  geom_edge_link(aes(width = weights), alpha = .3, colour = "grey50") +
  scale_edge_width(range = c(.1, .4), guide = "none") +
  
  # black outline
  geom_node_point(aes(size = size_color * 1.2), colour = "black", alpha = .8) +
  # main nodes: size → size_color, colour → score_norm
  geom_node_point(aes(size = size_color, colour = score_norm), alpha = .8) +
  
  # size legend
  scale_size_continuous(
    range  = c(3, 15),
    breaks = sz_br,
    labels = sz_lb,
    name   = "Node size\n(scaled variants)"
  ) +
  
  # colour legend (using your four‐step palette)
  scale_colour_gradientn(
    colours = c("lightyellow", "orange", "red", "darkred"),
    limits  = c(0, 1),
    breaks  = sn_br,
    labels  = sn_lb,
    name    = "Normalised\nscore- \npositive-total"
  ) +
  
  # highlight labels on top 15 by score_norm
  {
    top15 <- layout_df %>%
      arrange(desc(score_norm)) %>%
      slice_head(n = 15)
    geom_label_repel(
      data               = top15,
      aes(x = x, y = y, label = label),
      fill               = alpha("white", .5),
      size               = text_size,
      colour             = "black",
      max.overlaps       = Inf,
      force              = 20,
      min.segment.length = 0
    )
  } +
  
  theme_minimal(base_size = 20) +
  theme(
    text              = element_text(family = "sans"),
    panel.grid        = element_blank(),
    axis.title        = element_blank(),
    axis.text         = element_blank(),
    axis.ticks        = element_blank(),
    legend.background = element_rect(fill = "white", colour = "white")
  )

print(p_net)

ggsave("../output/untangleR_ppi_network.pdf", plot = p_net, height = 7, width = 9)

ggsave("../output/untangleR_ppi_network.png", plot = p_net, height = 7, width = 9)


# Associtions -----
# Rename the factor to avoid issues with spaces in the column name
df_clean <- df
df_clean$Major_category <- df_clean$`Major category`

# Run ANOVA using the new column name
anova_res <- aov(score_positive_total ~ Major_category, data = df_clean)
summary(anova_res)

# Run Tukey's HSD post hoc test
tukey_res <- TukeyHSD(anova_res)
print(tukey_res)


library(ggplot2)
library(dplyr)
library(tibble)

# Convert Tukey results to a data frame
tukey_df <- as.data.frame(tukey_res$Major_category) %>%
  rownames_to_column("comparison")

# Plot Tukey HSD comparisons with ggplot2
p2 <- ggplot(tukey_df, aes(x = reorder(comparison, diff), y = diff)) +
  geom_point() +
  geom_errorbar(aes(ymin = lwr, ymax = upr), width = 0.2) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  coord_flip() +
  labs(x = "Comparison",
       y = "Difference in Score\n(Positive Total)") +
  theme_bw(base_size = 20) +
  scale_x_discrete(breaks = function(x) x[seq(1, length(x), by = 5)])

print(p2)

# The plot shows each pairwise comparison between categories as a point (the mean difference) with error bars representing the 95% confidence interval. If an error bar crosses zero, it indicates that there is no statistically significant difference between those two groups. Conversely, if the entire error bar is either above or below zero, it suggests a significant difference in the score between those categories. The direction of the difference (positive or negative) indicates which group has the higher mean score.

# For non-parametric post hoc, use Dunn's test (requires the 'dunn.test' package)
# install.packages("dunn.test")  # Uncomment if needed
library(dunn.test)
dunn_res <- dunn.test(df$score_positive_total, df$`Major category`, method = "bh")
print(dunn_res)

# Visualise differences using ggplot2 boxplot with jittered points

p1 <- ggplot(df, aes(x = `Major category`, y = score_positive_total, fill = score_positive_total)) +
  geom_boxplot(outlier.shape = NA, fill = "lightblue") +
  geom_jitter(width = 0.2, alpha = 0.7, shape = 21, colour = "black") +
  scale_fill_gradientn(colours = c("orange", "red", "darkred")) +
  labs(x = "Major Category",
       y = "Score Positive Total") +
  theme_bw(base_size = 20) +
  guides(fill = "none") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

print(p1)

 # Even if one group appears to have a higher median in the boxplot, if the Tukey HSD post hoc test shows that none of the pairwise differences are statistically significant (i.e. the confidence intervals cross zero), then you cannot confidently say that one group is truly higher than another based on this data. The apparent difference could be due to chance.

library(patchwork)

# patch1 ----


  
# Use the & operator to apply a theme to all patchwork annotations
# patch1 <- p_net + (p1 + p2) + plot_annotation(tag_levels = 'A') &
  # theme(plot.tag = element_text(size = 24))

design <- 
  "1#
   1#
   23"

patch1 <- p_net / (p1 + p2) + plot_annotation(tag_levels = 'A') &
  theme(plot.tag = element_text(size = 30))

# patch1 <- (p_net) / (p1 + p2) + plot_annotation(tag_levels = 'A') &
#   theme(plot.tag = element_text(size = 30)) &
#   plot_layout(  widths = c(1, 1, 1),
#                 heights = c(2.5, 2, 2))

# patch1

# ggsave("../output/untangleR_ppi_network_assoc_patch1.pdf", plot = patch1, height = 10, width = 12)
ggsave("../output/untangleR_ppi_network_assoc_patch1.jpg",
       plot = patch1,
       height = 11, width = 15,
       dpi = 110)

# ggsave("../output/untangleR_ppi_network_assoc_patch1.jpg",
# plot = patch1,
# height = 8, width = 11,
# dpi = 110)

# For JPEG: reduce the dpi to lower the file size (adjust dpi as needed)
ggsave("~/web/var_risk_est/images/untangleR_ppi_network_assoc_patch1.jpg",
       plot = patch1,
       height = 11, width = 15,
       dpi = 110)

# PPI Netowrk plot ----
library(igraph)
library(uwot)
library(ggplot2)
library(ggrepel)
set.seed(666)

# Convert graph to undirected for community detection
graph_undirected <- as.undirected(graph, mode = "collapse")

# Detect communities using the Louvain algorithm
clusters <- cluster_louvain(graph_undirected)
V(graph_undirected)$cluster <- clusters$membership

# Create a weighted adjacency matrix from the undirected graph
A <- as.matrix(as_adjacency_matrix(graph_undirected, attr = "weight"))

# Run UMAP on the adjacency matrix
umap_embedding <- umap(A, n_neighbors = 15, min_dist = 0.1, metric = "euclidean")

# Prepare a data frame for plotting
embedding_df <- data.frame(
  UMAP1 = umap_embedding[,1],
  UMAP2 = umap_embedding[,2],
  gene_symbol = V(graph_undirected)$gene_symbol,
  degree = degree(graph_undirected, mode = "all"),
  cluster = factor(V(graph_undirected)$cluster)
)

saveRDS(embedding_df, "../output/embedding_df_with_clusters.Rds")

# Determine top nodes (e.g. degree above the 95th percentile) for labelling
degree_thresh <- quantile(embedding_df$degree, 0.95)
embedding_df$label <- ifelse(embedding_df$degree >= degree_thresh, embedding_df$gene_symbol, "")

# score_df <- df |> dplyr::select(gene_symbol, score_positive_total)

# Join score_positive_total to embedding_df based on gene_symbol
embedding_df <- embedding_df %>%
  left_join(score_df, by = c("gene_symbol" = "Genetic defect"))

# Ensure score_positive_total is numeric
embedding_df$score_positive_total <- as.numeric(embedding_df$score_positive_total)

# Create second set of labels for genes with top 15 score_positive_total
embedding_df$label_score <- ifelse(rank(-embedding_df$score_positive_total) <= 15,
                                   embedding_df$gene_symbol, "")

# Plot UMAP embedding with two sets of labels:
# - "label": genes with degree above the 95th percentile (existing column)


# p_umap ----
# - "label_score": genes with top 15 score_positive_total
# p_umap <- ggplot(embedding_df, aes(x = UMAP1, y = UMAP2, color = cluster)) +
#   geom_point(aes(size = degree), alpha = 0.8) +
#   # First set of labels (e.g. hub genes, already in embedding_df$label)
#   geom_label_repel(aes(label = label),
#                    fill = I(alpha("blue", 0.5)),
#                    size = 5,
#                    color = "black",
#                    max.overlaps = Inf,
#                    force = 20,
#                    min.segment.length = 0) +
#   # Second set of labels (top 15 score_positive_total)
#   geom_label_repel(aes(label = label_score),
#                    fill = I(alpha("yellow", 0.5)),
#                    size = 5,
#                    color = "black",
#                    max.overlaps = Inf,
#                    force = 20,
#                    min.segment.length = 0,
#                    nudge_y = 0.5) +
#   scale_size_continuous(range = c(3, 10)) +
#   # scale_y_continuous(expand = expansion(mult = c(0.4, 0.9)), limits = c(NA, NA)) + 
#     # scale_y_continuous(limits = c(-5, 8)) +
#   theme_bw(base_size = 20) +
#   labs(title = "UMAP Embedding of PPI Network",
#        color = "Cluster", size = "Degree") +
#   guides(size = "none") +
#   # Add colored annotations for the metrics
#   annotate("label", x = 4.25, y = -3.5, label = "Interaction\nDegree",
#            fill = alpha("blue", 0.5), color = "black", size = 6) +
#   annotate("label", x = 4.25, y = -2, label = "Variant Score\nPositive Total",
#            fill = alpha("yellow", 0.5), color = "black", size = 6)
# 
# print(p_umap)




# Define a Wes style block of 25 colors and subset the first 17
color_block <- c(
  "#00A08A", "#F2AD00", "#F98400", "#5BBCD6", 
  "#ECCBAE", "#046C9A", "#D69C4E", "#ABDDDE", 
  "#DD8D29", "#E2D200", "#46ACC8", "#E58601", 
  "#B40F20", "#F1BB7B", "#FD6467", "#5B1A18", 
  "#D67236", "#E6A0C4", "#C6CDF7", "#D8A499", 
  "#7294D4", "#446455", "#FDD262", "#D3DDDC", 
  "#C7B19C"
)

# Use the first 17 colors for the 17 groups
custom_colors <- color_block[1:17]

p_umap <- ggplot(embedding_df, aes(x = UMAP1, y = UMAP2, fill = cluster)) +
  # Use shape = 21 for points with fill and black outline
  geom_point(aes(size = degree), shape = 21, color = "black", alpha = 0.9) +
  geom_label_repel(aes(label = label),
                   fill = I(alpha("#60b2f0", 0.4)),
                   size = 5,
                   color = "black",
                   max.overlaps = Inf,
                   force = 20,
                   min.segment.length = 0) +
  geom_label_repel(aes(label = label_score),
                   fill = I(alpha("yellow", 0.5)),
                   size = 5,
                   color = "black",
                   max.overlaps = Inf,
                   force = 20,
                   min.segment.length = 0,
                   nudge_y = 0.5) +
  scale_fill_manual(values = custom_colors) +
  scale_size_continuous(range = c(3, 10)) +
  scale_y_continuous(limits = c(min(embedding_df$UMAP2, na.rm = TRUE), 
                                max(embedding_df$UMAP2, na.rm = TRUE) * 1.0)) +
  theme_bw(base_size = 20) +
  labs(#title = "UMAP Embedding of PPI Network",
       fill = "Cluster", size = "Degree") +
  guides(size = "none") +
  # Add colored annotations for the metrics
  annotate("label", x = 4.25, y = -3.5, label = "Interaction  \nDegree         ",
           fill = alpha("#60b2f0", 0.8), color = "black", size = 6) +
  annotate("label", x = 4.28, y = -2.5,   label = "Variant Score\nPositive Total",
           fill = alpha("yellow", 0.6), color = "black", size = 6)

print(p_umap)

ggsave("../output/untangleR_ppi_network_umap.pdf", plot = p_umap, height = 7, width = 12)
# ggsave("../output/untangleR_ppi_network_umap.png", plot = p_umap, height = 6, width = 9)

# Create a lookup table for gene to Major Category mapping
lookup_df <- df %>% 
  dplyr::select(`Genetic defect`, `Major category`) %>% 
  distinct()

# Merge the lookup info into the UMAP embedding data frame based on gene_symbol matching Genetic defect
embedding_df <- embedding_df %>%
  left_join(lookup_df, by = c("gene_symbol" = "Genetic defect"))

# View a contingency table of clusters by Major Category
cluster_table <- table(embedding_df$cluster, embedding_df$`Major category`)
print(cluster_table)

# p_cat ----
# Plot: Distribution of Major Categories within each cluster
# p_cat <- ggplot(embedding_df, aes(x = `Major category`, fill = `Major category`)) +
#   geom_bar() +
#   facet_wrap(~ cluster, scales = "free_y") +
#   theme_minimal(base_size = 20) +
#   theme(axis.text.x = element_text(angle = 45, hjust = 1),
#         legend.position = "none") +
#   labs(title = "Distribution of Major Category by Cluster",
#        x = "Major Category", y = "Count")
# 
# print(p_cat)


# p_map (UMAP embedding) is already defined above using custom_colors for clusters.
# Now, update the bar plot (p_cat) so that the fill colors for Major Category match the custom colors,
# and add a black outline to the bars.

p_cat <- ggplot(embedding_df, aes(x = `Major category`, fill = `Major category`)) +
  geom_bar(color = "black") +
  facet_wrap(~ cluster, scales = "free_y") +
  scale_fill_manual(values = custom_colors) +
  theme_minimal(base_size = 20) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "none") +
  labs(# title = "Distribution of Major Category by Cluster",
       x = "Major Category", y = "Count")

print(p_cat)

# assoc  ----

# Create a contingency table for clusters and Major Category
contingency <- table(embedding_df$cluster, embedding_df$`Major category`)
print(contingency)

# Perform a chi-square test for independence
chisq_res <- chisq.test(contingency)
print(chisq_res)

# Calculate standardized residuals to identify enrichments
std_res <- round(chisq_res$stdres, 2)
print(std_res)

# Mark cells with |residual| > 2 as enriched (or depleted)
enriched <- ifelse(abs(std_res) > 2, "Enriched/Depleted", "")
print(enriched)

# Convert standardized residuals from chi-square test to a data frame for plotting
stdres_df <- as.data.frame(as.table(chisq_res$stdres))
names(stdres_df) <- c("Cluster", "Major_category", "stdres")
# 
# # Create a heatmap of standardized residuals to show enrichment/depletion
# p_enrich <- ggplot(stdres_df, aes(x = Major_category, y = as.factor(Cluster), fill = stdres)) +
#   geom_tile(color = "white") +
#   geom_text(aes(label = round(stdres, 2)), color = "black", size = 6) +
#   scale_fill_gradient2(low = "blue", mid = "white", high = "red", midpoint = 0) +
#   theme_minimal(base_size = 20) +
#   theme(axis.text.x = element_text(angle = 45, hjust = 1),
#         legend.position = "right") +
#   labs(title = "Enrichment of Major Category by Cluster",
#        x = "Major Category",
#        y = "Cluster",
#        fill = "Std Residual")
# 
# print(p_enrich)
library(tidyr)
library(ggdendro)


# Convert stdres_df to a matrix with rows = clusters and columns = Major_category
res_matrix <- stdres_df %>%
  pivot_wider(names_from = Major_category, values_from = stdres) %>%
  column_to_rownames("Cluster") %>%
  as.matrix()

# Hierarchical clustering on columns (Major Category)
hc_cols <- hclust(dist(t(res_matrix)), method = "complete")
ordered_major_cat <- hc_cols$labels[hc_cols$order]

# Hierarchical clustering on rows (Clusters)
hc_rows <- hclust(dist(res_matrix), method = "complete")
ordered_clusters <- hc_rows$labels[hc_rows$order]

# Update factor levels in stdres_df based on hierarchical clustering
stdres_df <- stdres_df %>%
  mutate(Major_category = factor(Major_category, levels = ordered_major_cat),
         Cluster = factor(Cluster, levels = ordered_clusters))

# p_dendro ----
# Generate dendrogram data for columns using ggdendro
dendro_data <- dendro_data(hc_cols, type = "rectangle")
p_dendro <- ggplot(segment(dendro_data)) +
  geom_segment(aes(x = x, y = y, xend = xend, yend = yend), linewidth = 1) +
  scale_x_continuous(expand = c(0,0)) +
  scale_y_continuous(expand = c(0,0)) +
  theme_minimal(base_size = 20) +
  theme(axis.line = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        axis.title = element_blank(),
        panel.grid = element_blank())

# p_enrich ----
# Create the heatmap of standardized residuals with clustered axes
p_enrich <- ggplot(stdres_df, aes(x = Major_category, y = Cluster, fill = stdres)) +
  geom_tile(color = "white") +
  geom_text(aes(label = round(stdres, 1)), color = "black", size = 6) +
  scale_fill_gradient2(low = "blue", mid = "white", high = "red", midpoint = 0) +
  theme_bw(base_size = 25) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "right") +
  labs(#title = "Enrichment of Major Category by Cluster (Hierarchically Clustered)",
       x = "Major Category",
       y = "Cluster",
       fill = "Std\nResidual")

# Combine dendrogram and heatmap using patchwork
# Adjust relative heights so the dendrogram appears on top
combined_plot <- p_dendro / p_enrich + 
  plot_layout(heights = c(0.3, 1))

print(combined_plot)

# Aggregate standardized residuals by taking the maximum absolute value per Major Category
agg_df <- stdres_df %>%
  group_by(Major_category) %>%
  summarize(enrichment = max(abs(stdres)))

# p_bar ----
# Bar plot of enrichment score (max standardized residual) per Major Category
p_bar <- ggplot(agg_df, aes(x = Major_category, y = enrichment)) +
  geom_col(fill = "#046C9A", color = "black") +
  theme_bw(base_size = 25) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "none") +
  labs(# title = "Maximum Enrichment Score\nby Major Category",
       x = "Major Category",
       y = "Maximum\n| Std Residual |")

print(p_bar)

design <- "
  13
  23
  23
  23
  23
"
# patch1 <- 
  # p_score + p1 + p2 + p3 + plot_layout(design = design)  +
  # 
patch2 <- (p_dendro + p_enrich +  p_bar) + plot_layout(design = design) + plot_annotation(tag_levels = 'A') &
  theme(plot.tag = element_text(size = 24))

patch2

ggsave("../output/untangleR_ppi_network_patch2_cator.pdf", plot = patch2, height = 7, width = 16)

# "~/web/var_risk_est/images/


# Save p_net
# Save patch1
# Save patch2
# Save p_umap

# gnomad constraint -----
# cite: Karczewski, K. J., Francioli, L. C., Tiao, G., Cummings, B. B., Alföldi, J., Wang, Q., Collins, R. L., Laricchia, K. M., Ganna, A., Birnbaum, D. P., Gauthier, L. D., Brand, H., Solomonson, M., Watts, N. A., Rhodes, D., Singer-Berk, M., England, E. M., Seaby, E. G., Kosmicki, J. A., … MacArthur, D. G. The mutational constraint spectrum quantified from variation in 141,456 humans. Nature 581, 434–443 (2020). https://doi.org/10.1038/s41586-020-2308-7
gnmd_const <- read.table(file = "~/Desktop/gnomad.v4.1.constraint_metrics/gnomad.v4.1.constraint_metrics.tsv", header = TRUE)

# Assuming you have a data frame 'constraint_df' with gnomAD constraint metrics 
# and a data frame 'network_df' that includes gene names (matching constraint_df$gene) 
# and network metrics (e.g. degree from your PPI network)

# Merge network metrics with constraint data based on gene name

# Select relevant columns from the constraint data frame
gnmd_const_short <- gnmd_const %>%
  filter(mane_select == "true") %>%
  dplyr::select(gene, lof.oe_ci.upper_rank, lof.oe_ci.lower, lof.oe_ci.upper, lof.pLI) %>% na.omit() %>%
  unique()

# Merge the embedding data with constraint metrics based on gene name
# Here, 'embedding_df$gene_symbol' corresponds to 'gnmd_const_short$gene'
embedding_df_const <- merge(embedding_df, gnmd_const_short, by.x = "gene_symbol", by.y = "gene")

# Inspect the merged data frame
head(embedding_df_const)

# Convert columns to numeric if needed
embedding_df_const$lof.oe_ci.upper <- as.numeric(embedding_df_const$lof.oe_ci.upper)
embedding_df_const$lof.pLI <- as.numeric(embedding_df_const$lof.pLI)
embedding_df_const$degree <- as.numeric(embedding_df_const$degree)
embedding_df_const$lof.oe_ci.upper_rank <- as.numeric(embedding_df_const$lof.oe_ci.upper_rank)

embedding_df_const$cluster 
embedding_df_const$score_positive_total
embedding_df_const$lof.oe_ci.upper_rank
embedding_df_const$lof.oe_ci.upper_rank

# p_umap_const ----
p_umap_const <- ggplot(embedding_df_const, aes(x = UMAP1, y = UMAP2)) +
  geom_point(aes(size = degree, fill = lof.oe_ci.upper_rank), shape = 21, alpha = 0.9) +
  scale_fill_gradient(low = "lightyellow", high = "darkred") +
  scale_size_continuous(range = c(3, 10)) +
  scale_y_continuous(limits = c(min(embedding_df$UMAP2, na.rm = TRUE), 
                                max(embedding_df$UMAP2, na.rm = TRUE) * 1.0)) +
  theme_bw(base_size = 20) +
  labs(fill = "LoF OE CI\nupper rank", size = "PPI Degree")

p_umap_const

ggsave("../output/untangleR_ppi_network_p_umap_const.pdf", plot = p_umap_const, height = 7, width = 12)












p_const_hist <-  ggplot(embedding_df_const, aes(x = lof.oe_ci.upper_rank)) +
  geom_histogram(fill = "darkred", color = "black", bins = 30, alpha = 0.8) +
  facet_wrap(~ cluster) +
  theme_minimal(base_size = 20) +
  labs(x = "LOEUF Upper Rank", y = "Count", title = "Histogram of LOEUF Upper Rank by Cluster")

p_const_hist


ggplot(embedding_df_const, aes(x = gene_symbol)) +
  geom_linerange(aes(ymin = lof.oe_ci.lower, ymax = lof.oe_ci.upper), color = "black") +
  geom_point(aes(y = (lof.oe_ci.lower + lof.oe_ci.upper) / 2), color = "red", size = 2) +
  facet_wrap(~ cluster, scales = "free_x") +
  theme_minimal(base_size = 20) +
  labs(x = "Gene", y = "LOEUF Constraint Measure", 
       title = "Per-Gene LOEUF Constraint Intervals by Cluster") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# First, summarize the LOEUF constraint measures per cluster.
# Here we calculate the median lower and upper bounds for each cluster,
# and then compute the median midpoint.

cluster_summary <- embedding_df_const %>%
  group_by(cluster) %>%
  summarize(
    med_lower = median(lof.oe_ci.lower, na.rm = TRUE),
    med_upper = median(lof.oe_ci.upper, na.rm = TRUE)
  ) %>%
  mutate(med_midpoint = (med_lower + med_upper) / 2)

# Now, plot the summary as an error bar (representing the median interval)
# with a point at the median midpoint, for each cluster.

p_clust_const <- ggplot(cluster_summary, aes(x = cluster)) +
  geom_errorbar(aes(ymin = med_lower, ymax = med_upper), width = 0.2, color = "black") +
  geom_point(aes(y = med_midpoint), color = "red", size = 4) +
  labs(x = "Cluster", 
       y = "LOEUF Constraint Measure",
       title = "Median LOEUF Constraint Interval by Cluster") +
  theme_minimal(base_size = 20)

p_clust_const




# correlations ----

# Histogram for lof.oe_ci.upper_rank
p_hist_lof <- ggplot(embedding_df_const, aes(x = lof.oe_ci.upper_rank)) +
  geom_histogram(bins = 30, fill = "darkred", color = "black", alpha = 0.8) +
  labs(title = "Histogram of LOEUF Upper Rank", x = "LOEUF Upper Rank", y = "Count") +
  theme_minimal(base_size = 20)

print(p_hist_lof)

# Histogram for degree
p_hist_degree <- ggplot(embedding_df_const, aes(x = degree)) +
  geom_histogram(bins = 30, fill = "darkblue", color = "black", alpha = 0.8) +
  labs(title = "Histogram of PPI Degree", x = "PPI Degree", y = "Count") +
  theme_minimal(base_size = 20)

print(p_hist_degree)

# constraint with PPI degree ----

# Perform the Spearman correlation test
cor_res <- cor.test(embedding_df_const$degree, embedding_df_const$lof.oe_ci.upper_rank, method = "spearman")
rho_val <- round(cor_res$estimate, 3)
p_val <- signif(cor_res$p.value, 3)
print(cor_res)

# Create scatter plot with annotation
p_scatter <- ggplot(embedding_df_const, aes(x = degree, y = lof.oe_ci.upper_rank)) +
  geom_point(alpha = 0.6, color = "#046C9A") +
  geom_smooth(method = "lm", color = "red", se = TRUE) +
  labs(# title = "Correlation between PPI Degree and LOEUF Upper Rank",
       x = "PPI Degree",
       y = "LOEUF\nUpper Rank") +
  theme_minimal(base_size = 20) +
  annotate("label", 
           fill = alpha("white", 0.50), 
           x = max(embedding_df_const$degree, na.rm = TRUE)*0.4, 
           y = max(embedding_df_const$lof.oe_ci.upper_rank, na.rm = TRUE)*0.6,
           label = paste("Spearman's\nrho =", rho_val, "\n", "p-value =", p_val),
           hjust = 0,
           size = 8,
           color = "black") +
    scale_x_continuous(expand = expansion(mult = c(0, 0.15)), limits = c(-5, NA)) 


print(p_scatter)

ggsave("../output/untangleR_ppi_network_p_scatter_spear_rho.pdf", plot = p_scatter, height = 4, width = 7.5)

# constraint with clusters PPI degree ----
# Group by cluster and run correlation test per group
cor_results <- embedding_df_const %>%
  group_by(cluster) %>%
  summarise(
    n = n(),
    rho = cor(degree, lof.oe_ci.upper_rank, method = "spearman", use = "complete.obs"),
    p_value = cor.test(degree, lof.oe_ci.upper_rank, method = "spearman")$p.value
  ) %>%
  mutate(
    rho = round(rho, 3),
    p_value = signif(p_value, 3)
  )

print(cor_results)

# Filter out clusters with NA (i.e. groups with insufficient variation)
cor_plot_data <- cor_results %>%
  filter(!is.na(rho))

# Create a bar plot of Spearman's rho per cluster and annotate with the p-value
ggplot(cor_plot_data, aes(x = cluster, y = rho)) +
  geom_col(aes(fill = -(p_value)), color = "black", width = 0.7) +
  geom_text(aes(label = paste0("p=", p_value)), vjust = -0.5, size = 6) +
  labs(title = "Spearman Correlation between PPI Degree and LOEUF Upper Rank by Cluster",
       x = "Cluster",
       y = "Spearman's rho") +
  theme_minimal(base_size = 20)

# Overall, the full-network analysis shows a weak but statistically significant negative Spearman correlation between PPI degree and LOEUF upper rank (ρ = –0.181, p = 0.00024). This indicates that genes with higher connectivity tend to have lower LOEUF values (i.e. they are more constrained), suggesting that highly interactive genes may be less tolerant of loss‐of‐function variants.

# When the data are broken down by cluster, the correlation coefficients vary considerably. For example, clusters 2 (ρ = –0.375, p = 0.000994), 3 (ρ = –0.318, p = 0.0112), 4 (ρ = –0.800, p < 0.000001), 5 (ρ = –0.497, p = 0.0423), and 13 (ρ = 0.671, p = 0.0237; note the positive correlation here is unexpected and may reflect small sample size or heterogeneity) show moderate to strong correlations. Other clusters (e.g., clusters 1, 6, 7, and 8) display weak or non-significant relationships, and several clusters have very few genes (n < 5) rendering their estimates unstable. 

# These results suggest that while the overall trend supports an association between higher connectivity and increased constraint, the relationship can differ markedly depending on the functional grouping (cluster) of genes. Further investigation, particularly in clusters with larger sample sizes, is warranted to better understand the biological underpinnings of these differences.
# 
# library(ggplot2)
library(dplyr)

# Assume cor_plot_data is your data frame with columns:
# cluster, n, rho, p_value.
# Create a new column with the absolute correlation values.
cor_plot_data <- cor_plot_data %>%
  mutate(abs_rho = abs(rho))

# Option: Plot p_value (x-axis) vs. absolute Spearman's rho (y-axis) with cluster labels.
p_cor_spear_rho <- ggplot(cor_plot_data, aes(y = -log10(p_value), x = abs_rho)) +
  geom_point(size = 4, color = "#046C9A") +
  geom_text_repel(aes(label = cluster), vjust = -1, size = 10) +
  geom_hline(yintercept = -log10(0.05/17), linetype = "dashed", color = "red") +
  labs(# title = "Spearman Correlation by Cluster",
       y = "-log(p-value)",
       x = "Absolute Spearman's rho") +
  theme_minimal(base_size = 20)

p_cor_spear_rho

p_cor_spear_rho_scatter_patch <- p_scatter + p_cor_spear_rho + plot_annotation(tag_levels = 'A') 
# & theme(plot.tag = element_text(size = 24))

ggsave("../output/untangleR_ppi_network_p_cor_spear_rho_scatter_patch.pdf", plot = p_cor_spear_rho_scatter_patch, height = 4.5, width = 10)

# replot the PPI networks for the enriched signigicant clusters (2, and 4) after fiding them due to the gnomAD constraint level ----

# Merge the cluster information from the UMAP embedding into the layout tibble by gene_symbol
layout_df <- left_join(layout_df, embedding_df %>% dplyr::select(gene_symbol, cluster), by = "gene_symbol") |> unique()

# Check that the cluster column is present
head(layout_df$cluster)

# Assume sig_clusters is a vector of significant cluster IDs from your correlation results
cor_sig_clust <- cor_plot_data |> filter(p_value < 0.05/17)
sig_clusters <- cor_sig_clust$cluster

# Create an empty list to store plot objects
plot_list <- list()


# Loop over each significant cluster
for(cl in sig_clusters) {
  layout_sub <- layout_df %>% filter(cluster == cl)
  gene_string <- paste(unique(layout_sub$gene_symbol), collapse = ", ")
  cat("Cluster", cl, "genes:\n", gene_string, "\n\n")
}

# Loop over each significant cluster
for(cl in sig_clusters) {
  
  layout_sub <- layout_df %>% filter(cluster == cl)

  gene_string <- paste(unique(layout_sub$gene_symbol), collapse = ", ")
  gene_string <- str_wrap(gene_string, width = 50)
  cat("Cluster", cl, "genes:\n", gene_string, "\n\n")
  
  # save any NA size nodes
  layout_sub <- layout_sub %>%
    mutate(size_color = ifelse(is.na(size_color), min(size_color, na.rm = TRUE), size_color))
  
  
  p_net_sub <- ggraph(layout_sub) +
    geom_edge_link(aes(width = weights), alpha = 0.3, colour = "grey50") +
    scale_edge_width(range = c(0.1, 0.4), guide = "none") +
    
    # geom_label_repel( data = layout_sub,
    #                   nudge_y = 1,
    #                  aes(x = x, y = y,
    #                      label = unique(gene_symbol), 
    #                      fill = alpha("white", 0.10)
    #                                         ),
    #                  size = 4,
    #                  max.overlaps = Inf,
    #                  force = 10,
    #                  min.segment.length = 0) +
    
    geom_node_point(aes(size = size_color * 1.2), 
                    colour = "black", alpha = 0.8, show.legend = FALSE) +
    geom_node_point(aes(size = size_color, colour = color), show.legend = FALSE) +
    scale_color_identity(guide = "none") +
    scale_fill_identity() +
    # theme_minimal(base_size = 12) +
    theme_minimal(base_size = 20) +
    theme(panel.grid = element_blank(),
          axis.title = element_blank(),
          axis.text = element_blank(),
          axis.ticks = element_blank()) +
  
    labs(title = paste("Cluster", cl),
         subtitle = gene_string) 
  
  plot_list[[as.character(cl)]] <- p_net_sub
}

# Now, each significant cluster network plot is stored in plot_list.
p_clust1 <- plot_list[[1]]
p_clust2 <- plot_list[[2]]

cor_sig_clust_patch <- p_clust1 + p_clust2 + plot_annotation(tag_levels = 'A') 
# & theme(plot.tag = element_text(size = 24))
cor_sig_clust_patch

ggsave("../output/untangleR_ppi_network_cor_sig_clust_patch.pdf", plot = cor_sig_clust_patch, height = 5, width = 10)


design <- "
  1
  1
  2
"

patch3 <-  ((p_scatter + p_cor_spear_rho) /
              (p_clust1 + p_clust2)) +
  plot_annotation(tag_levels = 'A') + 
  theme_minimal(base_size = 20) +
  plot_layout(design = design)

patch3

ggsave("../output/untangleR_ppi_network_p_cor_spear_rho_sig_clust_patch3.pdf", plot = patch3, height = 12, width = 18)



