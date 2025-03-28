# This script pivots the gene_variant_summary_clean data to wide format (one row per gene symbol with separate columns for "Pathogenic" and "Other" variant counts), then merges it with df (after splitting "Genetic defect" into individual GeneSymbol rows).

library(dplyr)
library(stringr)
library(tidyr)
library(reactable)
library(htmltools)
library(sparkline)
library(purrr)

# gene_variant_summary_wide <- gene_variant_summary_clean %>%
#   pivot_wider(names_from = variant_class, values_from = variant_count, values_fill = list(variant_count = 0))
# 
# df <- df %>%
#   separate_rows(`Genetic defect`, sep = ";") %>%
#   mutate(GeneSymbol = str_trim(`Genetic defect`)) %>%
#   left_join(gene_variant_summary_wide, by = "GeneSymbol") %>%
#   dplyr::select(-"GeneSymbol")
# 
# df <- df %>% mutate(VariantCounts = paste(Pathogenic, Other, sep = " / "))
# 
# max_pathogenic <- max(df$Pathogenic, na.rm = TRUE)
# 
# # Calculate the maximum total count (for scaling if needed)
# max_total <- max(df$Pathogenic + df$Other, na.rm = TRUE)


# version 2 ----

head(clinvar_summary)

df <- df %>%
  separate_rows(`Genetic defect`, sep = ";") %>%
  mutate(GeneSymbol = str_trim(`Genetic defect`)) %>%
  left_join(clinvar_summary, by = "Genetic defect")#%>%
  # dplyr::select(-"GeneSymbol")

# df <- df %>% mutate(VariantCounts = paste(Pathogenic, Other, sep = " / "))

# max_pathogenic <- max(df$Pathogenic, na.rm = TRUE)

# Calculate the maximum total count (for scaling if needed)
# max_total <- max(df$Pathogenic + df$Other, na.rm = TRUE)

