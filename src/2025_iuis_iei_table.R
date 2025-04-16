# https://github.com/davetang/romim

library(dplyr)
library(stringr)
library(tidyr)
library(digest)
library(reactable)
library(htmlwidgets)
library(webshot2)
library(HPO.db)
library(AnnotationDbi)
library(ggplot2)
library(sparkline)

# source: https://wp-iuis.s3.eu-west-1.amazonaws.com/app/uploads/2024/10/30094653/IUIS-IEI-list-for-web-site-July-2024V2.xlsx
source <- "../data/IUIS-IEI-list-for-web-site-July-2024V2.xlsx"
md5sum <- digest(file = source, algo = "md5")
md5sum
df <-  readxl::read_xlsx(source)
data_dir <- "~/web/iei_genetics/data/"
gene_variant_summary_clean <- readRDS(file = paste0(data_dir, "gene_variant_summary.Rds"))
clinvar_summary <- readRDS(file = paste0(data_dir, "clinvar_summary.Rds"))

source("1_clean.R")
source("2_restring_B.R")
source("2_restring_T.R")
source("2_restring_Ig.R")
source("2_restring_N.R")
source("3_tally_clean.R")
source("4_hpo.R")
source("5_rename.R")
source("6_alpha.R")
# source("7_clinvar.R") # run once then skip - import data_dir
source("8_add_clinvar.R")
source("9_add_var_risk_est.R")
source("10_reorder.R")
source("11_reactable.R")
df_t

saveRDS(df, "../output/df_processed.Rds")
html_file <-  "../output/iusis_iei_table_2025.html"
img_file <-  "../output/iusis_iei_table_2025.png"
# reactablefmtr::save_reactable(df_t, html_file)
df <- df |> dplyr::select(-probabilities)
write.table(df, file = "../output/iusis_iei_table_2025.tsv", sep = "\t", row.names = FALSE)
saveWidget(widget = df_t, file = html_file, selfcontained = TRUE)
webshot(url = html_file, file = img_file, delay = 0.1,
        expand = c(10, 50, 0, 50))

# new insights from the improvements
source("12_untangleR.R")
source("13_new_categories.R")