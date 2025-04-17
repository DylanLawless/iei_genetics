# 1. load libraries ----
library(conflicted)
library(ggplot2);theme_set(theme_bw())
library(dplyr)
library(tidyr)
library(caret)
library(rpart)
library(rpart.plot)
library(scales)
library(forcats)
library(RColorBrewer)

conflict_prefer("select", "dplyr")
conflict_prefer("filter", "dplyr")
conflict_prefer("rename", "dplyr")
conflict_prefer("lag", "dplyr")
conflict_prefer("slice", "dplyr")

set.seed(666)

# 2. read and prepare data ----
df        <- readRDS("../output/df_processed.Rds")
ppi_embed <- readRDS("../output/embedding_df_with_clusters.Rds")

immu <- df %>%
  select(`Genetic defect`, `T cell`, `B cell`, Ig, Neutrophil,
         `Major category`, Subcategory) %>%
  inner_join(ppi_embed %>% select(gene_symbol, cluster),
             by = c("Genetic defect" = "gene_symbol")) %>%
  replace_na(list(`T cell`          = "normal",
                  `B cell`          = "normal",
                  Ig                 = "normal",
                  Neutrophil         = "normal",
                  `Major category` = "NA",
                  Subcategory       = "NA")) %>%
  mutate(
    Tcell      = factor(if_else(`T cell`   == "normal", "normal", "not_normal"),
                        levels = c("normal","not_normal")),
    Bcell      = factor(if_else(`B cell`   == "normal", "normal", "not_normal"),
                        levels = c("normal","not_normal")),
    Ig         = factor(if_else(Ig          == "normal", "normal", "not_normal"),
                        levels = c("normal","not_normal")),
    Neutrophil = factor(if_else(Neutrophil  == "normal", "normal", "not_normal"),
                        levels = c("normal","not_normal")),
    major      = factor(`Major category`),
    subcat     = factor(Subcategory),
    cluster    = factor(cluster)
  )

# 3. define train control and rpart control ----
train_ctrl <- trainControl(method = "cv", number = 5, savePredictions = "final")
rpart_ctrl <- rpart.control(minsplit = 10, minbucket = 5, cp = 0.001, maxdepth = 30)

# (a)  
model_pheno <- train(cluster ~ Tcell + Bcell + Ig + Neutrophil,
                        data      = immu,
                        method    = "rpart",
                        trControl = train_ctrl,
                        tuneGrid  = data.frame(cp = 0.001),
                        control   = rpart_ctrl)

# (b)  
model_pheno_maj <- train(cluster ~ major + Tcell + Bcell + Ig + Neutrophil,
                               data      = immu,
                               method    = "rpart",
                               trControl = train_ctrl,
                               tuneGrid  = data.frame(cp = 0.001),
                               control   = rpart_ctrl)

# (c)  
model_pheno_maj_sub <- train(cluster ~ major + subcat + Tcell + Bcell + Ig + Neutrophil,
                         data      = immu,
                         method    = "rpart",
                         trControl = train_ctrl,
                         tuneGrid  = data.frame(cp = 0.001),
                         control   = rpart_ctrl)

# (d)
model_maj_sub <- train(cluster ~ major + subcat,
                             data      = immu,
                             method    = "rpart",
                             trControl = train_ctrl,
                             tuneGrid  = data.frame(cp = 0.001),
                             control   = rpart_ctrl)

# 5. compute and print accuracies ----
acc_pheno           <- confusionMatrix(predict(model_pheno, immu),        immu$cluster)$overall["Accuracy"]
acc_pheno_maj       <- confusionMatrix(predict(model_pheno_maj, immu), immu$cluster)$overall["Accuracy"]
acc_pheno_maj_sub   <- confusionMatrix(predict(model_pheno_maj_sub, immu),       immu$cluster)$overall["Accuracy"]
acc_maj_sub   <- confusionMatrix(predict(model_maj_sub, immu),       immu$cluster)$overall["Accuracy"]

acc_pheno       
acc_pheno_maj
acc_maj_sub
acc_pheno_maj_sub  

# 3. Build a tidy accuracy table with pretty labels ----
acc_df <- tibble(
  model_key = c("pheno",       # (a) phenotypes only
                "pheno_maj",   # (b) phenotypes + major
                "pheno_maj_sub", # (c) phenotypes + major + subcat
                "maj_sub"      # (d) major + subcat only
  ),
  accuracy  = c(acc_pheno,
                acc_pheno_maj,
                acc_pheno_maj_sub,
                acc_maj_sub)
) %>%
  mutate(
    model_label = recode(model_key,
                         pheno          = "Phenotypes\nonly",
                         pheno_maj      = "Phenotypes +\nIUIS Major",
                         pheno_maj_sub  = "Phenotypes +\nIUIS Major +\nSubcat",
                         maj_sub        = "IUIS Major +\nSubcat\nonly"
    ),
    # force the plotting order
    model_label = factor(model_label,
                         levels = c(
                           "Phenotypes\nonly",
                           "Phenotypes +\nIUIS Major",
                           "IUIS Major +\nSubcat\nonly",
                           "Phenotypes +\nIUIS Major +\nSubcat"
                         )
    )
  )

# 4. Plot the bar chart ----

p_acc <- ggplot(acc_df, aes(x = model_label, y = accuracy, fill = model_label)) +
  geom_col(colour = "black") +
  geom_text(aes(label = percent(accuracy, accuracy = 1)), 
            vjust = -0.5, size = 4) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.2))) +
  scale_fill_brewer(palette = "YlOrRd") +
  labs(
    title = "Model Accuracy Comparison",
    x     = NULL,
    y     = "Accuracy"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    axis.text.x     = element_text(angle = 45, hjust = 1),
    legend.position = "none"
  )

print(p_acc)

# # prepare accuracy data with explicit factor levels
# acc_df <- tibble(
#   model    = c(
#     "PPI + \nBiomarkers",
#     "IUIS Major + \nBiomarkers",
#     "PPI +\nBiomarkers +\nIUIS Major/subcat"
#   ),
#   accuracy = c(acc_pheno, acc_pheno_maj, acc_pheno_maj_sub)
# ) %>%
#   mutate(model = factor(
#     model,
#     levels = c(
#       "PPI + \nBiomarkers",
#       "IUIS Major + \nBiomarkers",
#       "PPI +\nBiomarkers +\nIUIS Major/subcat"
#     )
#   ))
# 
# p_acc <- ggplot(acc_df, aes(x = model, y = accuracy, fill = model)) +
#   geom_col(colour = "black") +
#   geom_text(aes(label = round(accuracy, 2)), 
#             vjust = -0.5, size = 4) +
#   scale_x_discrete(limits = levels(acc_df$model)) +
#   scale_y_continuous(labels = percent_format(accuracy = 1),
#                      expand = expansion(mult = c(0, 0.2))) +
#   scale_fill_brewer(palette = "YlOrRd") +
#   labs(
#     title = "Model accuracy",
#     x     = NULL,
#     y     = "Accuracy"
#   ) +
#   theme(
#     axis.text.x     = element_text(angle = 45, hjust = 1),
#     legend.position = "none"
#   )
# 
# print(p_acc)
ggsave("../output/plot_model_accuracy_comparison.pdf", plot = p_acc, width = 4, height = 3)

# 6. plot all four trees vertically ----
pdf("../output/plot_multicat_new_classification_compare_trees.pdf", width = 8, height = 16)
par(mfrow = c(4, 1), mar = c(2, 2, 3, 1))
# par(mfrow = c(4, 1), mar = c(2, 2, 1))
rpart.plot(model_pheno$finalModel,
           type = 4, extra = 104, tweak = 1.1, fallen.leaves = TRUE,
           main = "PPI clusters ← immunophenotype")

rpart.plot(model_pheno_maj$finalModel,
           type = 4, extra = 104, tweak = 1.1, fallen.leaves = TRUE,
           main = "IUIS major ← immunophenotype")

# rpart.plot(model_trad_only$finalModel,
#            type = 4, extra = 104, tweak = 1.1, fallen.leaves = TRUE,
#            main = "Traditional pid ← major + subcategory")

rpart.plot(model_pheno_maj_sub$finalModel,
           type = 4, extra = 104, tweak = 1.1, fallen.leaves = TRUE,
           main = "Combined ← immunophenotype + IUIS")

dev.off()

# Plot the final model results for main figures ----


# 1. load libraries ----

# 2. read and prepare data ----
df        <- readRDS("../output/df_processed.Rds")
embed_df  <- readRDS("../output/embedding_df_with_clusters.Rds")

immu <- df %>%
  select(`Genetic defect`, `T cell`, `B cell`, Ig, Neutrophil,
         `Major category`, Subcategory) %>%
  inner_join(embed_df %>% select(gene_symbol, cluster),
             by = c("Genetic defect" = "gene_symbol")) %>%
  replace_na(list(`T cell`          = "normal",
                  `B cell`          = "normal",
                  Ig                 = "normal",
                  Neutrophil         = "normal",
                  `Major category` = "NA",
                  Subcategory       = "NA")) %>%
  mutate(
    Tcell      = factor(if_else(`T cell`   == "normal", "normal", "not_normal"),
                        levels = c("normal","not_normal")),
    Bcell      = factor(if_else(`B cell`   == "normal", "normal", "not_normal"),
                        levels = c("normal","not_normal")),
    Ig         = factor(if_else(Ig          == "normal", "normal", "not_normal"),
                        levels = c("normal","not_normal")),
    Neutrophil = factor(if_else(Neutrophil  == "normal", "normal", "not_normal"),
                        levels = c("normal","not_normal")),
    major      = factor(`Major category`),
    subcat     = factor(Subcategory),
    cluster    = factor(cluster)
  )

# 3. chi‑square heatmaps ----
chisq_t <- chisq.test(table(immu$Tcell,      immu$cluster))
chisq_b <- chisq.test(table(immu$Bcell,      immu$cluster))
chisq_ig <- chisq.test(table(immu$Ig,         immu$cluster))
chisq_neut <- chisq.test(table(immu$Neutrophil,immu$cluster))

plot_heatmap <- function(df, feat, pval) {
  ct_df <- as.data.frame(table(df[[feat]], df$cluster))
  names(ct_df) <- c("Status","Cluster","Count")
  ggplot(ct_df, aes(Cluster, Status, fill = Count)) +
    geom_tile(colour = "black") +
    geom_text(aes(label = Count), colour = "white", size = 4) +
    scale_fill_gradientn(colours = c("#FDD262", "#FD6467", "#5B1A18"),
                         limits = c(0, 100)) +
    labs(title = paste0("Distribution of ", feat, " by cluster\np=", signif(pval,3)))
}

p1 <- plot_heatmap(immu, "Tcell",      chisq_t$p.value)
p2 <- plot_heatmap(immu, "Bcell",      chisq_b$p.value)
p3 <- plot_heatmap(immu, "Ig",         chisq_ig$p.value)
p4 <- plot_heatmap(immu, "Neutrophil", chisq_neut$p.value)

library(patchwork)
p_chi <- (p1 | p2) / (p3 | p4) + 
  plot_layout(guides = 'collect', axis = "collect")  + 
  plot_annotation(tag_levels = 'A')
p_chi
ggsave("../output/plot_multicat_patch_3_clust_chi.pdf", plot = p_chi, height = 4, width = 9)
ggsave("~/web/var_risk_est/images/plot_multicat_patch_3_clust_chi.pdf", plot = p_chi, height = 4, width = 9)

# 4. fit final combined model ----
train_ctrl <- trainControl(method = "cv", number = 5, savePredictions = "final")
rpart_ctrl <- rpart.control(minsplit = 10, minbucket = 5, cp = 0.001, maxdepth = 30)

model_pheno_maj_sub <- train(cluster ~ major + subcat + Tcell + Bcell + Ig + Neutrophil,
                         data      = immu,
                         method    = "rpart",
                         trControl = train_ctrl,
                         tuneGrid  = data.frame(cp = 0.001),
                         control   = rpart_ctrl)

# 5. confusion matrix heatmap ----
cm <- confusionMatrix(predict(model_pheno_maj_sub, immu), immu$cluster)$table
cm_df <- as.data.frame(cm)
names(cm_df) <- c("Predicted","Observed","Freq")

p_cm <- ggplot(cm_df, aes(x = Observed, y = Predicted, fill = Freq)) +
  geom_tile(colour = "black") +
  geom_text(aes(label = Freq), size = 3) +
  scale_fill_gradientn(colours = c("#FDD262","#FD6467","#5B1A18")) +
  labs(title = "confusion matrix", x = "observed", y = "predicted")

# 6. variable importance ----

varimp <- varImp(model_pheno_maj_sub)$importance
varimp$Feature <- rownames(varimp)


varimp <- varImp(model_pheno_maj_sub)$importance %>%
  as_tibble(rownames = "Feature") %>%
  rename(Importance = Overall) %>%
  mutate(
    Feature = recode(Feature,
                     Tcell      = "T cell",
                     Bcell      = "B cell",
                     Ig         = "Immunoglobulin",
                     Neutrophil = "Neutrophil",
                     major      = "IUIS Major category",
                     subcat     = "IUIS"))

p_vi <- varimp |> filter(Importance > 10) |> 
  ggplot(aes(x = reorder(Feature, Importance), y = Importance)) +
  geom_col(fill = "#FD6467", colour = "black") +
  coord_flip() +
  scale_x_discrete(labels = function(x) str_trunc(x, 15, ellipsis = "...")) +
  labs(title = "variable importance (>10)", x = NULL, y = "importance")

p_vi

# 7. sensitivity & specificity ----
byClass <- as.data.frame(confusionMatrix(
  predict(model_pheno_maj_sub, immu), immu$cluster
)$byClass)
byClass$Class <- rownames(byClass)

p_sen <- ggplot(byClass, aes(x = reorder(Class, Sensitivity), y = Sensitivity)) +
  geom_col(fill = "#FDD262", colour = "black") +
  coord_flip() +
  labs(x = "class")

p_spec <- ggplot(byClass, aes(x = reorder(Class, Specificity), y = Specificity)) +
  geom_col(fill = "#5BBCD6", colour = "black") +
  coord_flip() +
  labs(x = "class")

p_perf <- (p_acc / p_cm) |  ( p_vi / (p_sen + p_spec)) 
# p_perf <- (p_cm + p_vi) / (p_sen + p_spec) + plot_annotation(tag_levels = "A")
p_perf <- p_perf + plot_annotation(tag_levels = "A")
ggsave("../output/plot_multicat_performance_combined.pdf", plot = p_perf, height = 6, width = 10)
ggsave("~/web/var_risk_est/images/plot_multicat_performance_combined.pdf", plot = p_perf, height = 6, width = 10)

# 8. coloured decision tree ----
color_block <- c("#00A08A","#F2AD00","#F98400","#5BBCD6","#ECCBAE",
                 "#046C9A","#D69C4E","#ABDDDE","#DD8D29","#E2D200",
                 "#46ACC8","#E58601","#B40F20","#F1BB7B","#FD6467",
                 "#5B1A18","#D67236")
box_cols <- rep("lightgrey", nrow(model_pheno_maj_sub$finalModel$frame))
term <- as.numeric(rownames(model_pheno_maj_sub$finalModel$frame)[
  model_pheno_maj_sub$finalModel$frame$var=="<leaf>"
])
for(i in seq_along(term)) {
  idx <- which(as.numeric(rownames(model_pheno_maj_sub$finalModel$frame))==term[i])
  box_cols[idx] <- color_block[i]
}
pdf("../output/plot_multicat_combined_classification_tree.pdf", width = 8, height = 4)
rpart.plot(model_pheno_maj_sub$finalModel, type = 4, extra = 104,
           tweak = 1.1, fallen.leaves = TRUE, box.col = box_cols)
dev.off()

# 9. new pid classification bar plot ----
leaf_nodes <- model_pheno_maj_sub$finalModel$where
groups     <- factor(leaf_nodes,
                     labels = paste0("Grp. ", sort(unique(leaf_nodes))))
df_groups  <- immu %>% mutate(new_group = groups)

mode_val <- function(x) { ux<-unique(x); ux[which.max(tabulate(match(x,ux)))] }

mapping <- df_groups %>%
  group_by(new_group) %>%
  summarise(
    Tm = mode_val(Tcell), Bm = mode_val(Bcell),
    Im = mode_val(Ig), Nm = mode_val(Neutrophil)
  ) %>%
  mutate(desc = paste0(new_group,
                       if_else(Tm!="normal"," Tcell",""),
                       if_else(Bm!="normal"," Bcell",""),
                       if_else(Im!="normal"," Ig",""),
                       if_else(Nm!="normal"," Neut","")))

df_plot <- df_groups %>%
  left_join(mapping %>% select(new_group, desc), by = "new_group")

# color number of PID classes
n_classes <- length(unique(df_plot$desc))

# clolor generate a distinct colour for each class
palette <- colorRampPalette(brewer.pal(9, "YlGnBu"))(n_classes)

p_bar <- ggplot(df_plot, aes(x = desc, fill = desc)) +
  geom_bar(colour = "black") +
  scale_fill_manual(values = palette) +
  labs(
    x = "PID classification model:\nPPI ~ Phenotypes + IUIS IEI Major/sub categories",
    y = "Gene count"
  ) +
  # theme_minimal() +
  theme(
    axis.text.x   = element_text(angle = 45, hjust = 1),
    legend.position = "none"
  )

print(p_bar)


ggsave("../output/plot_multicat_new_pid_classes_combined.pdf", plot = p_bar, height = 3, width = 8)
ggsave("~/web/var_risk_est/images/plot_multicat_new_pid_classes_combined.pdf", plot = p_bar, height = 3, width = 8)

# information about strategy ----


library(caret)

set.seed(666)

# 1. create 5 stratified folds
folds <- createFolds(immu_merged_clean$cluster, k = 5, returnTrain = TRUE)

# 2. inspect each fold
lapply(seq_along(folds), function(i) {
  train_idx <- folds[[i]]
  data.frame(
    fold         = i,
    n_train      = length(train_idx),
    n_test       = nrow(immu_merged_clean) - length(train_idx),
    balance_train = prop.table(table(immu_merged_clean$cluster[ train_idx ])),
    balance_test  = prop.table(table(immu_merged_clean$cluster[-train_idx]))
  )
})

# 3. plug these exact splits into trainControl
train_ctrl  <- trainControl(
  method      = "cv",
  number      = 5,
  index       = folds,
  savePredictions = "final"
)

# now your train() call will use *these* folds
model_pheno <- train(
  cluster ~ Tcell + Bcell + Ig + Neutrophil,
  data      = immu_merged_clean,
  method    = "rpart",
  trControl = train_ctrl,
  tuneGrid  = data.frame(cp = 0.001),
  control   = rpart.control(minsplit = 10, minbucket = 5, cp = 0.001, maxdepth = 30)
)

# 4. if you’d rather do LOOCV:
train_ctrl_loocv <- trainControl(method = "LOOCV", savePredictions = "final")
model_ppi_loocv <- update(model_pheno, trControl = train_ctrl_loocv)

# check out-of-fold performance
confusionMatrix(model_pheno$pred$pred, model_pheno$pred$obs)

