neutro_lookup <- c(
  "normal" = "normal",
  "low" = "low",
  "variable" = "variable",
  "severe neutropenia" = "severe",
  "normal to low" = "mixed",
  "high" = "high",
  "variable neutropenia" = "variable",
  "normal but dysfunctional" = "defective",
  "pancytopenia" = "pancytopenia",
  "neutropenia with active hlh" = "defective",
  "mild pancytopenia" = "pancytopenia",
  "neutropenia" = "low"
)

df <- df %>%
  mutate(Neutrophil_count_clean = `Neutrophil count` %>%
           tolower() %>%
           str_trim() %>%
           recode(!!!neutro_lookup))
