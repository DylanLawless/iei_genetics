# # install.packages("remotes")
# # remotes::install_github('davetang/romim')
# library(romim)
# help(package = "romim")
# omim_result <- get_omim(147920, geneMap = TRUE)
# # not a real key
# set_key('AAAAAAAAAAAAAAAAAAAAAA')
# print("awaiting API key")
# # https://github.com/davetang/romim?tab=readme-ov-file

# mygene ----
# if (!require("BiocManager", quietly = TRUE))
  # install.packages("BiocManager")
# 
# BiocManager::install("mygene")
library(mygene)
library(dplyr)

print("note request >1000 require a special command")

gene_symbols <- unique(df$`Genetic defect`)
# Use returnall=TRUE to access the response data
res <- queryMany(gene_symbols, scopes = "symbol", fields = "uniprot", species = "human", returnall = TRUE)
response_df <- as.data.frame(res$response, stringsAsFactors = FALSE)

response_df <- response_df %>%
  filter(
    (is.na(notfound) | notfound != TRUE) &
      !is.na(`uniprot.Swiss.Prot`) &
      str_detect(`uniprot.Swiss.Prot`, "[0-9]") # weird empty cells
  )

map_df <- data.frame(query = response_df$query, stringsAsFactors = FALSE)
map_df$Uniprot <- sapply(seq_len(nrow(response_df)), function(i) {
  sp <- as.character(response_df$`uniprot.Swiss.Prot`[i])
  if (!is.na(sp) && sp != "" && sp != "NA") {
    # If multiple IDs are present, take the first one
    return(trimws(strsplit(sp, ",")[[1]][1]))
  } else {
    return(NA_character_)
  }
})

# Join the mapping back to the original data frame by matching 'Genetic defect' with 'query'
df <- df %>% left_join(map_df, by = c("Genetic defect" = "query"))
df$AlphaFold_URL <- paste0("https://alphafold.ebi.ac.uk/entry/", df$Uniprot)