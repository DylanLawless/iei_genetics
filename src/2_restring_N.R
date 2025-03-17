neutro_lookup <- c(
  "normal" = "normal",
  "low" = "low",
  "variable" = "variable",
  "severe neutropenia" = "low",
  "normal to low" = "mixed",
  "high" = "high",
  "variable neutropenia" = "variable",
  "normal but dysfunctional" = "defective",
  "low" = "low",
  "neutropenia with active hlh" = "defective",
  "mild low" = "low",
  "neutropenia" = "low"
)

df <- df %>%
  mutate(Neutrophil_count_clean = `Neutrophil count` %>%
           tolower() %>%
           str_trim() %>%
           recode(!!!neutro_lookup))
