cat(paste(shQuote(names(df)), collapse = ", "))

df <- df |>
  dplyr::select(
    -"GeneSymbol",
    'Major category', 
    'Subcategory', 
    'Disease', 
    'Genetic defect', 
    'Inheritance',
    'OMIM ID',
    Uniprot, 
    VariantCounts.VRE,
    score_positive_total,
    VariantCounts.ClnVar,
    probabilities,
    'T cell', 'B cell', 'Ig',  'Neutrophil',  
    'T cell details', 'B cell details', 'Ig details', 'Neutrophil details',
    'Associated features', 
    'ICD9', 'ICD10', 
    'HPO IDs', 'HPO term',
    "Uniprot",
    'Other affected cells', 
    'Inheritance detail', 
    'GOF/DN details', everything())

df <- df[order(df[[1]], df[[2]]), ]

