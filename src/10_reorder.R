cat(paste(shQuote(names(df)), collapse = ", "))

df <- df |>
  dplyr::select(-GeneSymbol)

df <- df |>
  dplyr::select(
    `Major category`,
    Subcategory,
    Disease,
    `Genetic defect`,
    Inheritance,
    score_positive_total,
    probabilities,
    VariantCounts.VRE,
    VariantCounts.ClnVar,
    `OMIM ID`,
    AlphaFold_URL,
    Uniprot,
    `HPO IDs`,
    `HPO term`,
    ICD9,
    ICD10,
    `Associated features`,

    
    `B cell`, `T cell`, Ig, Neutrophil,
  
    `Inheritance detail`,
    Major_category_original,
    `GOF/DN details`,
    `T cell details`,
    `B cell details`,
    `Ig details`,
    `Neutrophil details`,
    `Other affected cells`,
    
    score5.ClnVar,
    score4.ClnVar,
    score2.ClnVar,
    score0.ClnVar,
    score5.VRE,
    score4.VRE,
    score2.VRE,
    score0.VRE,
    min_prob,
    median_prob,
    max_prob,
    everything())

df <- df[order(df[[1]], df[[2]]), ]

