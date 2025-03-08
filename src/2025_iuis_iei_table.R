# https://github.com/davetang/romim

library(dplyr)
library(stringr)
library(tidyr)
library(digest)



# source: https://wp-iuis.s3.eu-west-1.amazonaws.com/app/uploads/2024/10/30094653/IUIS-IEI-list-for-web-site-July-2024V2.xlsx
source <- "../data/IUIS-IEI-list-for-web-site-July-2024V2.xlsx"
md5sum <- digest(file = source, algo = "md5")
md5sum
df <-  readxl::read_xlsx(source)

source("1_clean.R")
source("2_restring_B.R")
source("2_restring_T.R")
source("2_restring_Ig.R")
source("2_restring_N.R")
source("3_tally_clean.R")

library(HPO.db)
library(AnnotationDbi)

source("4_hpo.R")
source("5_rename.R")
source("6_reorder.R")
# source("urls.R")
# 
library("reactable")
library("htmlwidgets")
library("webshot2")
source("7_reactable.R")
df_t

html_file <-  "../output/iusis_iei_table_2025.html"
img_file <-  "../output/iusis_iei_table_2025.png"
# reactablefmtr::save_reactable(df_t, html_file)
write.table(df, file = "../output/iusis_iei_table_2025.tsv", sep = "\t")
saveWidget(widget = df_t, file = html_file, selfcontained = TRUE)
webshot(url = html_file, file = img_file, delay = 0.1,
        expand = c(10, 50, 0, 50))
