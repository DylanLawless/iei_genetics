# if (!requireNamespace("BiocManager", quietly=TRUE))
    # install.packages("BiocManager")
# BiocManager::install("HPO.db")

# Run once ----
# doterm <- toTable(HPOTERM)
# colnames(doterm)[colnames(doterm) == 'hpoid'] <- 'HPO combined'
# colnames(doterm)[colnames(doterm) == 'term'] <- 'HPO term'
# saveRDS(doterm, file = paste0(data_dir, "hpoterms.Rds"))

# import ----
doterm <- readRDS(file = paste0(data_dir, "hpoterms.Rds"))

df <- df %>%
  mutate(row_id = row_number()) %>%
  separate_rows(`HPO combined`, sep = ";\\s*") %>%
  merge(doterm, by.x = "HPO combined", by.y = "HPO combined") %>%
  group_by(row_id) %>%
  summarise(
    `HPO combined` = paste(unique(`HPO combined`), collapse = "; "),
    `HPO term` = paste(`HPO term`, collapse = "; ")
  ) %>%
  right_join(df %>% mutate(row_id = row_number()) %>% 
               dplyr::select(-`HPO combined`), by = "row_id") %>%
  dplyr::select(-row_id)
