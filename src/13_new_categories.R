# 1. Load required libraries ----
library(dplyr)
library(tidyr)
library(ggplot2); theme_set(theme_bw())
library(caret)
library(forcats)
library(igraph)
library(uwot)
library(ggraph)
library(ggrepel)
library(rpart)
library(rpart.plot)  # For plotting the complete decision tree
library(patchwork)

set.seed(666)

# 2. Read the processed IEI data ----
df <- readRDS("../output/df_processed.Rds")
head(df %>% select(`T cell`, `B cell`, Ig, Neutrophil, `Genetic defect`))

# 3. Prepare immunophenotypic data ----
immu_df <- df %>%
  select(`Genetic defect`, `T cell`, `B cell`, Ig, Neutrophil) %>%
  mutate(across(c(`T cell`, `B cell`, Ig, Neutrophil), as.factor))

cat("T cell distribution:\n")
print(table(immu_df$`T cell`))
cat("B cell distribution:\n")
print(table(immu_df$`B cell`))
cat("Ig distribution:\n")
print(table(immu_df$Ig))
cat("Neutrophil distribution:\n")
print(table(immu_df$Neutrophil))


# Convert the data to long format for easier plotting
immu_long <- immu_df %>%
  pivot_longer(cols = c(`T cell`, `B cell`, Ig, Neutrophil),
               names_to = "Feature", 
               values_to = "Status")

# Create a faceted bar plot using ggplot2 and patchwork
p <- ggplot(immu_long, aes(x = Status, fill = Status)) +
  geom_bar(color = "black") +
  facet_wrap(~ Feature, scales = "free") +
  theme_minimal() +
  labs(title = "Distribution of Immunophenotypic Features",
       x = "Status",
       y = "Count") +
  scale_fill_brewer(palette = "RdYlBu") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

print(p)

ggsave("../output/plot_patch_1_v2.pdf", plot = p, height = 4, width = 7)
ggsave("~/web/var_risk_est/images/plot_patch_1_v2.pdf", plot = p, height = 4, width = 7)


#  4. Load PPI embedding and cluster assignments ----
embedding_df <- readRDS("../output/embedding_df_with_clusters.Rds")

# 5. Merge immunophenotypic data with PPI clusters ----
immu_merged <- immu_df %>%
  inner_join(embedding_df %>% select(gene_symbol, cluster),
             by = c("Genetic defect" = "gene_symbol"))
immu_merged$cluster <- as.factor(immu_merged$cluster)
head(immu_merged)
cat("Cluster frequency:\n")
print(table(immu_merged$cluster))

# 6. Prepare data for classification ----
cat("Missing values (per column):\n")
print(sapply(immu_merged[, c("T cell", "B cell", "Ig", "Neutrophil")], function(x) sum(is.na(x))))

immu_merged_clean <- immu_merged %>%
  mutate(
    `T cell`   = replace_na(`T cell`, "normal"),
    `B cell`   = replace_na(`B cell`, "normal"),
    Ig         = replace_na(Ig, "normal"),
    Neutrophil = replace_na(Neutrophil, "normal")
  ) %>%
  mutate(
    Tcell = `T cell`,
    Bcell = `B cell`
  ) %>%
  select(-`T cell`, -`B cell`) %>%
  mutate(
    Tcell   = factor(if_else(Tcell == "normal", "normal", "not_normal"),
                     levels = c("normal", "not_normal")),
    Bcell   = factor(if_else(Bcell == "normal", "normal", "not_normal"),
                     levels = c("normal", "not_normal")),
    Ig      = factor(if_else(Ig == "normal", "normal", "not_normal"),
                     levels = c("normal", "not_normal")),
    Neutrophil = factor(if_else(Neutrophil == "normal", "normal", "not_normal"),
                        levels = c("normal", "not_normal"))
  )

# 7. Statistical Association (Chi-square tests) ----
chisq_t <- chisq.test(table(immu_merged_clean$Tcell, immu_merged_clean$cluster))
chisq_b <- chisq.test(table(immu_merged_clean$Bcell, immu_merged_clean$cluster))
chisq_ig <- chisq.test(table(immu_merged_clean$Ig, immu_merged_clean$cluster))
chisq_neut <- chisq.test(table(immu_merged_clean$Neutrophil, immu_merged_clean$cluster))

cat("Chi-square test results:\n")
print(chisq_t)
print(chisq_b)
print(chisq_ig)
print(chisq_neut)

# Define a helper function to create a heatmap plot with p-value annotation
plot_heatmap <- function(df, feature_name, p_value) {
  # Create the contingency table for the given clinical feature and cluster
  ct <- table(df[[feature_name]], df$cluster)
  # Convert to a data frame for ggplot2
  ct_df <- as.data.frame(ct)
  names(ct_df) <- c("Status", "Cluster", "Count")
  
  # Format the p-value text
  pval_text <- paste("p =", signif(p_value, 3))
  
  ggplot(ct_df, aes(x = Cluster, y = Status, fill = Count)) +
    geom_tile(color = "black") +
    geom_text(aes(label = Count), color = "white", size = 4) +
    scale_fill_gradientn(colours = c("#FDD262", "#FD6467", "#5B1A18"),
                         limits = c(0, 100)) +
    labs(title = paste("Distribution of", feature_name, "by Cluster\n", pval_text),
         x = "Cluster",
         y = feature_name) 
    # theme_minimal()
}

# Assuming the chi-square test results are stored in chisq_t, chisq_b, chisq_ig, chisq_neut
# Create heatmaps for each clinical feature with their corresponding p-values
p1 <- plot_heatmap(immu_merged_clean, "Tcell", chisq_t$p.value)
p2 <- plot_heatmap(immu_merged_clean, "Bcell", chisq_b$p.value)
p3 <- plot_heatmap(immu_merged_clean, "Ig", chisq_ig$p.value)
p4 <- plot_heatmap(immu_merged_clean, "Neutrophil", chisq_neut$p.value)

# Combine the plots using patchwork
p_combined <- (p1 | p2) / (p3 | p4)  + 
  plot_layout(guides = 'collect', axis = "collect")  + 
  plot_annotation(tag_levels = 'A')
print(p_combined)

ggsave("../output/plot_patch_3_clust_chi.pdf", plot = p_combined, height = 4, width = 9)
ggsave("~/web/var_risk_est/images/plot_patch_3_clust_chi.pdf", plot = p_combined, height = 4, width = 9)


# What It Means
# Significance Confirmation: The chi-square tests indicate that there’s a highly significant association between the clinical features (Tcell, Bcell, Ig, Neutrophil) and the clusters. The plots help you see which clusters have higher frequencies of abnormal values.
# Visual Patterns:  For example, if one heatmap shows that Cluster 3 has predominantly “not_normal” values for Tcell while Cluster 1 shows mostly “normal”, this could be a sign that the immunophenotypic profile is different between these clusters. Such visual cues can guide further investigation or refinement of classification schemes.
# Cluster Characteristics: Combining these plots helps to build a narrative about how each cluster behaves in terms of the immune features, which is particularly valuable when comparing these new classifications with traditional PID (IEI) categories.


# 8: Model Granular PID Classifications ----
# We started with a rough non-fine tuned version which is not shown here and it had 3 groups.
# Fine-tune rpart parameters to allow more splits and deeper tree
rpart_ctrl <- rpart.control(minsplit = 10,    # Lower minsplit for earlier splits
                            minbucket = 5,    # Smaller terminal node size
                            cp = 0.001,       # Lower cp to allow more splits (less pruning)
                            maxdepth = 30)    # Allow a deeper tree

# Train the rpart model with the new control parameters to derive finer groups
train_control_fine <- trainControl(method = "cv", number = 5, savePredictions = "final")
model_immuno_fine <- train(cluster ~ Tcell + Bcell + Ig + Neutrophil,
                           data = immu_merged_clean,
                           method = "rpart",
                           trControl = train_control_fine,
                           tuneGrid = data.frame(cp = 0.001),
                           control = rpart_ctrl)

print(model_immuno_fine)
print(varImp(model_immuno_fine))

## 0. Evaluate model predictions for fine-tuned model ----
if (!is.null(model_immuno_fine$pred) && nrow(model_immuno_fine$pred) > 0) {
  all_levels <- levels(immu_merged_clean$cluster)
  pred_fixed <- factor(model_immuno_fine$pred$pred, levels = all_levels)
  obs_fixed  <- factor(model_immuno_fine$pred$obs, levels = all_levels)
  confusion_fine <- confusionMatrix(pred_fixed, obs_fixed)
  print(confusion_fine)
} else {
  pred <- predict(model_immuno_fine, newdata = immu_merged_clean)
  confusion_fine <- confusionMatrix(pred, immu_merged_clean$cluster)
  print(confusion_fine)
}

## 1. Confusion Matrix Heatmap ----
cm_table_fine <- confusion_fine$table
cm_df_fine <- as.data.frame(cm_table_fine)
names(cm_df_fine) <- c("Prediction", "Reference", "Frequency")

p_mod_1_fine <- ggplot(cm_df_fine, aes(x = Reference, y = Prediction, fill = Frequency)) +
  geom_tile() +
  geom_text(aes(label = Frequency), colour = "black", size = 3) +
  scale_fill_gradientn(colours = c("#FDD262", "#FD6467", "#5B1A18"),
                       limits = c(0, 100)) +
  labs(title = "Confusion Matrix Heatmap",
       x = "Observed Cluster",
       y = "Predicted Cluster")

p_mod_1_fine

## 2. Variable Importance Plot ----
varimp_df_fine <- as.data.frame(varImp(model_immuno_fine)$importance)
varimp_df_fine$Feature <- rownames(varimp_df_fine)

p_mod_2_fine <- ggplot(varimp_df_fine, aes(x = reorder(Feature, Overall), y = Overall)) +
  geom_bar(stat = "identity", fill = "#FD6467", colour = "black") +
  coord_flip() +
  theme_minimal() +
  labs(title = "Decision Tree Variable Importance",
       x = "Features",
       y = "Importance")

p_mod_2_fine

## 3. Per-Class Performance (Sensitivity and Specificity) ----
byClass_df_fine <- as.data.frame(confusion_fine$byClass)
if(nrow(byClass_df_fine) > 1){
  byClass_df_fine$Class <- rownames(confusion_fine$byClass)
  
  p_mod_3_fine <- ggplot(byClass_df_fine, aes(x = reorder(Class, Sensitivity), y = Sensitivity)) +
    geom_bar(stat = "identity", fill = "#FDD262", color = "black") +
    coord_flip() +
    theme_minimal() +
    labs(title = "Sensitivity by Cluster",
         x = "Cluster",
         y = "Sensitivity")
  
  p_mod_4_fine <- ggplot(byClass_df_fine, aes(x = reorder(Class, Specificity), y = Specificity)) +
    geom_bar(stat = "identity", fill = "#5BBCD6", colour = "black") +
    coord_flip() +
    theme_minimal() +
    labs(title = "Specificity by Cluster",
         x = "Cluster",
         y = "Specificity")
} else {
  message("By-class performance not available as separate rows; multi-class metrics may be averaged.")
}

## 4. Combine Plots ----
library(patchwork)
p_combined_mod_fine <- (p_mod_1_fine + p_mod_2_fine) / (p_mod_3_fine + p_mod_4_fine) +
  plot_annotation(tag_levels = 'A')
print(p_combined_mod_fine)

ggsave("../output/plot_combined_model_interpret_fine.pdf", plot = p_combined_mod_fine, height = 7, width = 14)
ggsave("~/web/var_risk_est/images/plot_combined_model_interpret_fine.pdf", plot = p_combined_mod_fine, height = 7, width = 14)

# 9. Model tree with colors -----
# Define a Wes-style color block and select the first 17 colors
color_block <- c(
  "#00A08A", "#F2AD00", "#F98400", "#5BBCD6", 
  "#ECCBAE", "#046C9A", "#D69C4E", "#ABDDDE", 
  "#DD8D29", "#E2D200", "#46ACC8", "#E58601", 
  "#B40F20", "#F1BB7B", "#FD6467", "#5B1A18", 
  "#D67236", "#E6A0C4", "#C6CDF7", "#D8A499", 
  "#7294D4", "#446455", "#FDD262", "#D3DDDC", 
  "#C7B19C"
)
custom_colors <- color_block[1:17]

# Extract the rpart tree frame from the fine-tuned model
tree_frame <- model_immuno_fine$finalModel$frame

# Identify terminal nodes (leaf nodes)
terminal_nodes <- as.numeric(rownames(tree_frame)[tree_frame$var == "<leaf>"])
sorted_terminals <- sort(terminal_nodes)

# Create a vector to hold the colors for each row in the tree frame.
# For non-terminal (internal) nodes, we'll use a default color ("lightgrey")
box_colors <- rep("lightgrey", nrow(tree_frame))
box_colors <- custom_colors

# Assign a custom color from custom_colors to each terminal node (in sorted order)
for(i in seq_along(sorted_terminals)){
  idx <- which(as.numeric(rownames(tree_frame)) == sorted_terminals[i])
  box_colors[idx] <- custom_colors[i]
}

# Save the decision tree plot as a PDF with the custom colors on the leaf nodes
# pdf("~/web/var_risk_est/images/p_new_classification_finetune_tree.pdf", width = 8, height = 5)
pdf("../output/p_new_classification_finetune_tree.pdf", width = 8, height = 4)
library(rpart.plot)
rpart.plot(model_immuno_fine$finalModel,
           type = 4,
           extra = 104,
           tweak=1.1,
           fallen.leaves = TRUE,
           box.col = box_colors
           # main = "A  Fine-Tuned Decision Tree for PID Classification"
           # main = "A"
)
dev.off()

pdf("~/web/var_risk_est/images/p_new_classification_finetune_tree.pdf", width = 8, height = 4)
library(rpart.plot)
rpart.plot(model_immuno_fine$finalModel,
           type = 4,
           extra = 104,
           tweak=1.1,
           fallen.leaves = TRUE,
           box.col = box_colors
           # main = "A  Fine-Tuned Decision Tree for PID Classification"
           # main = "A  "
)
dev.off()

# 13: Deriving New PID Classifications Based on Abnormal Clinical Features ----

# Extract terminal node numbers from the fine-tuned tree and assign new PID classifications
immu_leaf_nodes_fine <- model_immuno_fine$finalModel$where
new_classifications_fine <- factor(immu_leaf_nodes_fine,
                                   levels = sort(unique(immu_leaf_nodes_fine)),
                                   labels = paste("Group", sort(unique(immu_leaf_nodes_fine)), sep = "_"))

# Merge the fine-grained classifications into the final dataframe
model_df_fine <- immu_merged_clean %>%
  select(`Genetic defect`, cluster, Tcell, Bcell, Ig, Neutrophil) %>%
  mutate(new_PID_classification = new_classifications_fine)

# Function to compute the mode (most frequent value)
mode_value <- function(x) {
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}

# Update the mapping table to create descriptive group names (if not already done)
mapping <- model_df_fine %>%
  group_by(new_PID_classification) %>%
  summarise(
    Tcell_mode      = mode_value(Tcell),
    Bcell_mode      = mode_value(Bcell),
    Ig_mode         = mode_value(Ig),
    Neutrophil_mode = mode_value(Neutrophil)
  ) %>%
  ungroup() %>%
  mutate(new_PID_classification_descriptive = if_else(
    Tcell_mode == "normal" & Bcell_mode == "normal" & Ig_mode == "normal" & Neutrophil_mode == "normal",
    paste0(new_PID_classification, "_all_normal"),
    paste0(new_PID_classification,
           if_else(Tcell_mode != "normal", "_Tcell", ""),
           if_else(Bcell_mode != "normal", "_Bcell", ""),
           if_else(Ig_mode != "normal", "_Ig", ""),
           if_else(Neutrophil_mode != "normal", "_Neutrophil", "")
    )
  ))

# Merge the descriptive names back into the final dataframe
model_df_fine <- model_df_fine %>%
  left_join(mapping %>% select(new_PID_classification, new_PID_classification_descriptive),
            by = "new_PID_classification")

# Plot the distribution of new descriptive PID classifications with the same 17 custom colors
p_class <- ggplot(model_df_fine, aes(x = new_PID_classification_descriptive, fill = new_PID_classification_descriptive)) +
  geom_bar(colour = "black") +
  scale_fill_manual(values = custom_colors) +
  scale_x_discrete(labels = function(x) str_wrap(gsub("_", " ", x), width = 15)) +
  labs(#subtitle = "B  Distribution of New PID Classifications",
    # subtitle = "B",
       x = "New PID Classification",
       y = "Gene Count") +
  guides(fill = "none") +
  theme(text=element_text(size=14)) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

p_class
ggsave("../output/plot_new_pid_classifications_genetic.pdf", plot = p_class, height = 4, width = 8)
ggsave("~/web/var_risk_est/images/plot_new_pid_classifications_genetic.pdf", plot = p_class, height = 4, width = 8)

# Inspect the updated dataframe with the descriptive group names.
head(model_df_fine)

# Write the final dataframe with descriptive classifications to a CSV file.
write.csv(model_df_fine, file = "../output/df_PID_classification_descriptive.csv", row.names = FALSE)

