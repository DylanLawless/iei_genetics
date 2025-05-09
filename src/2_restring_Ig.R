ig_lookup <- c(
  "variably low" = "low",
  "low" = "low",
  "normal" = "normal",
  "low igg and iga with normal or high igm" = "mixed",
  "high ige" = "high",
  "variable" = "variable",
  "very low" = "low",
  "high ige, poor specific antibody production" = "defective",
  "normal serum igm, g, a; • intact levels of pathogen-spec igg" = "normal",
  "nl" = "normal",
  "igm nl or high low igg, iga" = "mixed",
  "variably low igg, iga" = "low",
  "normal or decreased" = "mixed",
  "pan-hypogammaglobulinemia" = "low",
  "normal to low" = "mixed",
  "near-normal levels of serum igm, g, a\r\n• poor/absent ig responses to vaccines" = "defective",
  "panhypogamma (igg, undetectable; igm, iga low).  intact b cell function in vitro" = "low",
  "progressive hypogammaglobulinemia, impaired csr, shm" = "low",
  "high serum igg, iga, igm" = "high",
  "low igm, normal igg, variable iga.\r\nnear normal vaccine resp" = "mixed",
  "low igm, normal to high igg and iga, high ige" = "mixed",
  "normal levels of serum igm, g, a  but reduced responses to live viral vaccines" = "defective",
  "normal to increased ige" = "high",
  "decreased to normal" = "mixed",
  "increased" = "high",
  "high ige and iga" = "high",
  "high igm" = "high",
  "high iga, low igm and igg" = "mixed",
  "normal or increased" = "mixed",
  "decreased, some with elevated iga, igm, poor specific antibody responses, absent antibody to polysaccharide antigens" = "defective",
  "mild decrease" = "low",
  "hypogammaglobulinemia with low/normal igg vaccine responses" = "defective",
  "normal or low" = "mixed",
  "severe hypogamma" = "low",
  "high" = "high",
  "most patients died early, no data" = NA_character_,
  "normal/low igg, a, normal igm. hyper ige. vaccine igg normal" = "mixed",
  "agammaglobulinemia" = "low",
  "low iga and occasionally low igg" = "low",
  "low, poor function" = "defective",
  "normal igm, low iga, high ige" = "mixed",
  "slightly low igg" = "low",
  "normal to high" = "mixed",
  "hypogammaglobuliemia" = "low",
  "decreased igg and iga, elevated igm, poor specific antibody responses, absent antibody to polysaccharide antigens" = "defective",
  "normal igg, iga, normal to elevated igm; decreased antibody responses to pps" = "defective",
  "decreased" = "low",
  "high serum igg, iga; normal igm, ige. anti-insulin autoantibody" = "high",
  "normal or elevated igg and iga, most high ige," = "mixed",
  "normal/increased igm, reduced igg and iga" = "defective",
  "low with preserved vaccine resposnes" = "low",
  "low igg and iga, high igm, abnormal antibody responses" = "defective",
  "low igg or iga" = "low",
  "hypogammaglobulinemia, variably decreased specific antibodies" = "defective",
  "low serum igg, iga \r\nreduced specific ab responses" = "defective",
  "low maybe due to nephrotic syndrome" = "low",
  "low immunogobulins" = "low",
  "undetectable" = "low",
  "low to normal" = "mixed",
  "increased ige" = "high",
  "low igg, igm" = "low",
  "increased igee" = "high",
  "normal to increased" = "mixed",
  "low igm and a, lack of anti-pneumococcal  antibody" = "defective",
  "low igg" = "low",
  "high igg" = "high",
  "low igg and iga and/or igm" = "low",
  "variable total igg with low iga" = "low",
  "low iga" = "low",
  "low igm" = "low",
  "high ige, low igm" = "mixed",
  "high ige and igg" = "high"
)

df <- df %>%
  mutate(Immunoglobulin_levels_clean = `Immunoglobulin levels` %>%
           tolower() %>%
           str_trim() %>%
           recode(!!!ig_lookup))
