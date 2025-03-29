
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
df <- df %>%
  separate_rows(`Genetic_defect`, sep = "[+;]") %>%
  mutate(`Genetic_defect` = str_trim(`Genetic_defect`))
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
df$Inheritance <- ifelse(df$Inheritance == "Sporadic/toxin", "Sporadic", df$Inheritance)
# Delete the "variable" inheritance for CVID phenotype - only appears once and is not clear 
df$Inheritance <- ifelse(df$Inheritance == "Variable" & str_detect(df$Disease, "CVID"), NA_character_, df$Inheritance)
df$Inheritance

df$OMIM_ID <- df$OMIM
df <- df %>% dplyr::select(-OMIM)

df$OMIM_ID <- str_replace_all(df$OMIM_ID," ","") # stupid white spaces
df$OMIM_ID <- str_replace_all(df$OMIM_ID,"Not yet attributed",NA_character_) # (-_-) 
df$OMIM_ID <- str_replace_all(df$OMIM_ID,"614602 \r\n" ,"614602") # (-_-) 
df$OMIM_ID <- str_replace_all(df$OMIM_ID,"· 612411" ,"612411") # (-_-) 

pattern <- "[\\u0391-\\u03A9\\u03B1-\\u03C9]"
cols_with_greek <- names(df)[sapply(df, function(col) {
  is.character(col) && any(str_detect(col, pattern), na.rm = TRUE)
})]

print(cols_with_greek)
for (col in cols_with_greek) {
  cat("Entries in", col, "with Greek letters:\n")
  print(df %>% filter(str_detect(get(col), pattern)) %>% dplyr::select(all_of(col)))
  cat("\n")
}

greek_map <- c(
  "α" = "a", "β" = "b", "γ" = "g", "δ" = "d", "ε" = "e", "ζ" = "z", "η" = "h", "θ" = "th",
  "ι" = "i", "κ" = "k", "λ" = "l", "μ" = "m", "ν" = "n", "ξ" = "x", "ο" = "o", "π" = "p",
  "ρ" = "r", "σ" = "s", "ς" = "s", "τ" = "t", "υ" = "u", "φ" = "ph", "χ" = "ch", "ψ" = "ps", "ω" = "o",
  "Α" = "A", "Β" = "B", "Γ" = "G", "Δ" = "D", "Ε" = "E", "Ζ" = "Z", "Η" = "H", "Θ" = "Th",
  "Ι" = "I", "Κ" = "K", "Λ" = "L", "Μ" = "M", "Ν" = "N", "Ξ" = "X", "Ο" = "O", "Π" = "P",
  "Ρ" = "R", "Σ" = "S", "Τ" = "T", "Υ" = "U", "Φ" = "Ph", "Χ" = "Ch", "Ψ" = "Ps", "Ω" = "O"
)

for (col in cols_with_greek) {
  for (greek in names(greek_map)) {
    df[[col]] <- str_replace_all(df[[col]], fixed(greek), greek_map[greek])
  }
}

df$Disease <- df$Disease %>%
  str_replace_all("Widemann-Steiner", "Wiedemann-Steiner") %>%
  str_replace_all("ICOSLG deficiency", "ICOSL deficiency") %>%
  str_replace_all("IKZF2 deficiency", "HELIOS deficiency") %>%
  str_replace_all("C8bdeficiency", "C8b deficiency") %>%
  str_replace_all("STING--associated", "STING-associated") %>%
  str_replace_all("STAT3 GOF mutation", "STAT3 GOF") %>%
  str_replace_all("IRF4 multimorphic R95T", "IRF4 multimorphic")

df$Major_category_original <- df$`Major category`

# Create a mapping from the abbreviated Major category to its full definition
major_map <- c(
  "1. CID"  = "1. Immunodeficiencies affecting cellular and humoral immunity",
  "2. CID+" = "2. Combined immunodeficiencies with associated or syndromic features",
  "3. PAD"  = "3. Predominantly Antibody Deficiencies",
  "4. PIRD" = "4. Diseases of Immune Dysregulation",
  "5. PD"   = "5. Congenital defects of phagocyte number or function",
  "6. IID"  = "6. Defects in intrinsic and innate immunity",
  "7. AID"  = "7. Autoinflammatory Disorders",
  "8. CD"   = "8. Complement Deficiencies",
  "9. BMF"  = "9. Bone marrow failure"
)

# Replace full category names with abbreviated ones (including their number)
df$`Major category` <- dplyr::case_when(
  df$`Major category` == "1. Immunodeficiencies affecting cellular and humoral immunity" ~ "1. CID",
  df$`Major category` == "2. Combined immunodeficiencies with associated or syndromic features" ~ "2. CID+",
  df$`Major category` == "3. Predominantly Antibody Deficiencies" ~ "3. PAD",
  df$`Major category` == "4. Diseases of Immune Dysregulation" ~ "4. PIRD",
  df$`Major category` == "5. Congenital defects of phagocyte number or function" ~ "5. PD",
  df$`Major category` == "6. Defects in intrinsic and innate immunity" ~ "6. IID",
  df$`Major category` == "7. Autoinflammatory Disorders" ~ "7. AID",
  df$`Major category` == "8. Complement Deficiencies" ~ "8. CD",
  df$`Major category` == "9. Bone marrow failure" ~ "9. BMF",
  TRUE ~ df$`Major category`
)

df$Subcategory <- dplyr::case_when(
  df$Subcategory == "3. Combined Immunodeficiencies Generally Less Profound than Severe Combined Immunodeficiency" ~ "3. CID Generally Less Profound than SCID",
  TRUE ~ df$Subcategory
)



# 
# df$`Major category` <- dplyr::case_when(
#   df$`Major category` == "1. Immunodeficiencies affecting cellular and humoral immunity" ~ "1. CID",
#   df$`Major category` == "2. Combined immunodeficiencies with associated or syndromic features" ~ "2. CID+",
#   df$`Major category` == "3. Predominantly Antibody Deficiencies" ~ "3. PAD",
#   df$`Major category` == "4. Diseases of Immune Dysregulation" ~ "4. PIRD",
#   df$`Major category` == "5. Congenital defects of phagocyte number or function" ~ "5. PD",
#   df$`Major category` == "6. Defects in intrinsic and innate immunity" ~ "6. IID",
#   df$`Major category` == "7. Autoinflammatory Disorders" ~ "7. AID",
#   df$`Major category` == "8. Complement Deficiencies" ~ "8. CD",
#   df$`Major category` == "9. Bone marrow failure" ~ "9. BMF",
#   TRUE ~ df$`Major category`
# )
# 
# tooltip_text <- paste(
#   "1. CID: Immunodeficiencies affecting cellular and humoral immunity;",
#   "2. CID+: Combined immunodeficiencies with associated or syndromic features;",
#   "3. PAD: Predominantly Antibody Deficiencies;",
#   "4. PIRD: Diseases of Immune Dysregulation;",
#   "5. PD: Congenital defects of phagocyte number or function;",
#   "6. IID: Defects in intrinsic and innate immunity;",
#   "7. AID: Autoinflammatory Disorders;",
#   "8. CD: Complement Deficiencies;",
#   "9. BMF: Bone marrow failure.",
#   sep = "\n"
# )
# 
