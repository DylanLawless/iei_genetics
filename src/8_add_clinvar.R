# This script pivots the gene_variant_summary_clean data to wide format (one row per gene symbol with separate columns for "Pathogenic" and "Other" variant counts), then merges it with df (after splitting "Genetic defect" into individual GeneSymbol rows).

library(dplyr)
library(stringr)
library(tidyr)
library(reactable)
library(htmltools)
library(sparkline)
library(purrr)

gene_variant_summary_wide <- gene_variant_summary_clean %>%
  pivot_wider(names_from = variant_class, values_from = variant_count, values_fill = list(variant_count = 0))

df <- df %>%
  separate_rows(`Genetic defect`, sep = ";") %>%
  mutate(GeneSymbol = str_trim(`Genetic defect`)) %>%
  left_join(gene_variant_summary_wide, by = "GeneSymbol") %>%
  dplyr::select(-"GeneSymbol")

df <- df %>% mutate(VariantCounts = paste(Pathogenic, Other, sep = " / "))

max_pathogenic <- max(df$Pathogenic, na.rm = TRUE)

# Calculate the maximum total count (for scaling if needed)
max_total <- max(df$Pathogenic + df$Other, na.rm = TRUE)


# 
# reactable(
#   df_summary,
#   columns = list(
#     "Genetic defect" = colDef(name = "Gene"),
#     Pathogenic = colDef(
#       name = "Known Papathogenic variants",
#       align = "left",
#       cell = function(value) {
#         width <- paste0(value / max_pathogenic * 100, "%")
#         bar_chart(value, width = width)
#       }
#     )
#   )
# )
# 



# 
# 
# 
# 
# 
# 
# 
# 
# library(reactable)
# library(htmltools)
# library(tippy)
# 
# # Define a function to create a stacked bar chart
# bar_chart_stacked <- function(label, path_width, other_width, height = "16px") {
#   div(
#     style = list(
#       position = "relative",
#       display = "flex",
#       width = paste0(path_width + other_width, "%"),
#       height = height,
#       border = "1px solid #ccc",
#       borderRadius = "4px",
#       overflow = "hidden"
#     ),
#     div(
#       style = list(
#         width = paste0(path_width, "%"),
#         height = "100%",
#         background = "red"
#       )
#     ),
#     div(
#       style = list(
#         width = paste0(other_width, "%"),
#         height = "100%",
#         background = "blue"
#       )
#     ),
#     div(
#       style = list(
#         position = "absolute",
#         top = 0,
#         left = "50%",
#         transform = "translateX(-50%)",
#         color = "white",
#         fontSize = "12px",
#         lineHeight = height
#       ),
#       label
#     )
#   )
# }
# 
# # Calculate the maximum total count (for scaling if needed)
# max_total <- max(df$Pathogenic + df$Other, na.rm = TRUE)
# 
# # Create a VariantCounts column that displays "Pathogenic / Other" counts as text
# df <- df %>% mutate(VariantCounts = paste(Pathogenic, Other, sep = " / "))
# 
# 
# reactable(
#   df,
#   columns = list(
#     `Genetic defect` = colDef(name = "Gene"),
#     VariantCounts = colDef(
#       name = "Known pathogenic variants",
#       header = function() {
#         tippy("Known pathogenic variants &#9432;",
#               tooltip = "Displays the count of pathogenic and other variants reported on ClinVar for this gene (Pathogenic / Other).",
#               delay = 0, theme = "custom")
#       },
#       align = "left",
#       cell = function(value, index) {
#         path_count <- df$Pathogenic[index]
#         other_count <- df$Other[index]
#         total <- path_count + other_count
#         path_width <- ifelse(!is.na(total) & total > 0, (path_count / total) * 100, 0)
#         other_width <- ifelse(!is.na(total) & total > 0, (other_count / total) * 100, 0)
#         label_text <- paste0(path_count, " / ", other_count)
#         
#         div(
#           style = list(display = "flex", alignItems = "center"),
#           div(
#             style = list(marginRight = "8px", fontSize = "12px", color = "black"),
#             label_text
#           ),
#           div(
#             style = list(
#               position = "relative",
#               display = "flex",
#               width = "100px",
#               height = "16px",
#               border = "1px solid #ccc",
#               borderRadius = "4px",
#               overflow = "hidden"
#             ),
#             div(
#               style = list(
#                 width = paste0(path_width, "%"),
#                 height = "100%",
#                 background = "red"
#               )
#             ),
#             div(
#               style = list(
#                 width = paste0(other_width, "%"),
#                 height = "100%",
#                 background = "blue"
#               )
#             )
#           )
#         )
#       }
#     )
#   )
# )
