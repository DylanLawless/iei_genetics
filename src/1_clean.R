
df <- df %>%
  unite("HPO combined", c("HPO (table)", "HPO (subtable)", "HPO...18", "HPO...19"), 
        sep = "; ", na.rm = TRUE, remove = TRUE)

# clean the data ----
colnames(df)[colnames(df) == 'Genetic defect'] <- 'Genetic_defect'

df$`Major category` <- str_replace(df$`Major category`, "^Table\\s*(\\d+)\\s*", "\\1. ")
df$Subcategory <- str_replace(df$Subcategory, "^Subtable\\s*(\\d+)\\s*", "\\1. ")


# Fix gene names ----
# unique_gene_symbol <- unique(df[["Gene symbol"]])
# unique_gene_symbol[str_detect(unique_gene_symbol, " ")]

df$Genetic_defect <- str_replace_all(df$Genetic_defect, "Unknown / environment", NA_character_)
df$Genetic_defect <- str_replace_all(df$Genetic_defect, "Unknown", NA_character_)

df$Genetic_defect <- str_replace_all(df$Genetic_defect, "CD40 \\(TNFRSF5\\)", "CD40")
df$Genetic_defect <- str_replace_all(df$Genetic_defect, "CD40LG \\(TNFSF5\\)", "CD40LG")
df$Genetic_defect <- str_replace_all(df$Genetic_defect, "KMT2D \\(MLL2\\)", "KMT2D")
df$Genetic_defect <- str_replace_all(df$Genetic_defect, "MOGS \\(GCS1\\)", "MOGS")
df$Genetic_defect <- str_replace_all(df$Genetic_defect, "PIK3CD GOF", "PIK3CD")
df$Genetic_defect <- str_replace_all(df$Genetic_defect, "CFHR1 CFHR2\\. CFHR3 CFHR4 CFHR5", "CFHR1;CFHR2;CFHR3;CFHR4;CFHR5")
df <- df %>% separate_rows(Genetic_defect, sep = ";") # note we must split genes to their own rows. It is not reasonable to merge genes even if closely related. 
df1 <- df$Inheritance 
df1 <- str_replace_all(df1," ","") # stupid white spaces
df1 <- str_replace_all(df1,"\\?",NA_character_) # question mark is not appropriate, if not sufficient evidence keep as NA
df1 <- str_replace_all(df1,"AR or AD" ,"AD or AR" ) # make consistent
df1 <- str_replace_all(df1,"AD or AR" ,"AD/AR" ) # make consistent
df1 <- str_replace_all(df1,"AD \\(1 AR\\)" ,"AD/AR" ) # make consistent 
df1 <- str_replace_all(df1,"AD \\(1 AR\\)" ,"AD/AR" )
df1 <- str_replace_all(df1,"AD halploinsufficiency" ,"AD haploinsufficiency" ) 
df1 <- str_replace_all(df1,"XL \\(females may be affected\\)" ,"XL females_affected" )
df1 <- str_replace_all(df1,"XL females can be affected" ,"XL females_affected" )
df1 <- str_replace_all(df1," " ,":" ) # make 2 columns
df1 <- df1 %>% replace_na(NA_character_) # I want the character string NA_character_

df$Inheritance <- df1
rm(df1)
df <- separate(df, Inheritance, into = c("Inheritance", "Inheritance detail"), sep = ":")
df$Inheritance %>% unique()

df$OMIM_ID <- df$OMIM
df <- df %>% dplyr::select(-OMIM)

df$OMIM_ID <- str_replace_all(df$OMIM_ID," ","") # stupid white spaces
df$OMIM_ID <- str_replace_all(df$OMIM_ID,"Not yet attributed",NA_character_) # (-_-) 
df$OMIM_ID <- str_replace_all(df$OMIM_ID,"614602 \r\n" ,"614602") # (-_-) 
df$OMIM_ID <- str_replace_all(df$OMIM_ID,"· 612411" ,"612411") # (-_-) 
