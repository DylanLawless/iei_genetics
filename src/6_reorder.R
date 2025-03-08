cat(paste(shQuote(names(df)), collapse = ", "))

df <- df |>
  dplyr::select(
    'Major category', 
    'Subcategory', 
    'Disease', 
    'Genetic defect', 
    'Inheritance',
    'T cell count', 'B cell count', 'Ig count',  'Neutrophil count',  
    'T cell details', 'B cell details', 'Ig details', 'Neutrophil details',
    'Associated features', 
    'ICD9', 'ICD10', 
    'HPO combined', 'HPO term',
    'OMIM ID', 
    'Other affected cells', 
    'Inheritance detail', 
    'GOF/DN details', everything())

df <- df[order(df[[1]], df[[2]]), ]

