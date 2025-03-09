# for value coloring, there can be no NAs (but character string "NA")
library(reactable)
library(htmltools)
library(tippy)


# reactable ----

options(reactable.theme = reactableTheme(
  borderColor = "#dfe2e5",
  stripedColor = "#E5E5E5",
  highlightColor = "#fcf0e6",
  cellPadding = "8px 12px",
  style = list(fontFamily = "-apple-system, Arial, BlinkMacSystemFont, Segoe UI, Helvetica,  sans-serif",
               fontSize = "1.0rem"),
  searchInputStyle = list(width = "50%")
))

df_t <- reactable(
  df,
  # height = 800,
  bordered = TRUE,
  elementId = "df-select",
  compact = TRUE,
  searchable = TRUE,
  resizable = TRUE, 
  defaultPageSize = 10,
  pageSizeOptions = c(10, 50, nrow(df)),
  filterable = TRUE,
  showSortable = TRUE,
  showPageSizeOptions = TRUE,
  striped = TRUE,
  highlight = TRUE,
  defaultColDef = colDef(minWidth = 140),

  columns = list(
    
    "Major category" = colDef(
      header = function() {
        tippy("Major category &#9432;", tooltip = "Definition: Major category of primary immunodeficiencies.", delay = 0, theme = "custom")
      }
    ),
    
    "Subcategory" = colDef(
      header = function() {
        tippy("Subcategory &#9432;", tooltip = "Definition: Subcategory or more detailed classification of the PID.", delay = 0, theme = "custom")
      }
    ),
    
    Disease = colDef(
      minWidth = 200,
      header = function() {
        tippy("Disease &#9432;", tooltip = "Definition: The type or classification of disease.", delay = 0, theme = "custom")
      }
    ),
    
    "Genetic defect" = colDef(
      header = function() {
        tippy("Genetic defect &#9432;", tooltip = "Definition: The gene or mutation responsible for the condition.", delay = 0, theme = "custom")
      }
    ),
    

    
    
    
    "Inheritance" = colDef(
        filterInput = function(values, name) {htmltools::tags$select(onchange = sprintf("Reactable.setFilter('df-select', '%s', event.target.value || undefined)", name), htmltools::tags$option(value = "", "All"), lapply(sort(unique(values)), function(x) htmltools::tags$option(value = x, x)))},
        
        header = function() {
          tippy(
            "Inheritance &#9432;",
            tooltip = paste0(
              "Definition: Mode of inheritance. ",
              "AD: Autosomal dominant; AR: Autosomal recessive; XL: X-linked; ",
              "Sporadic/toxin: Sporadic or toxin-induced; AD/AR: Combination; Variable: Variable expression."
            ),
            delay = 0,
            theme = "custom"
          )
        }
        
        ),
    
    
    
    "T cell count" = colDef(
      header = function() {
        tippy(
          "T cell count &#9432;",
          tooltip = paste0(
            "Definition: T cell count status. ",
            "Possible values: low, normal, high, defective, decreased, variable, mixed."
          ),
          delay = 0,
          theme = "custom"
        )
      },
      filterInput = function(values, name) {htmltools::tags$select(onchange = sprintf("Reactable.setFilter('df-select', '%s', event.target.value || undefined)", name), htmltools::tags$option(value = "", "All"), lapply(sort(unique(values)), function(x) htmltools::tags$option(value = x, x)))}),
    
    
    
    

    
    
    
    "B cell count" = colDef(
      header = function() {
        tippy(
          "B cell count &#9432;",
          tooltip = paste0(
            "Definition: B cell count status. ",
            "Possible values: normal, borderline, low, decreased, immature, defective, high, abnormal, variable."
          ),
          delay = 0,
          theme = "custom"
        )
      },
      filterInput = function(values, name) {htmltools::tags$select(onchange = sprintf("Reactable.setFilter('df-select', '%s', event.target.value || undefined)", name), htmltools::tags$option(value = "", "All"), lapply(sort(unique(values)), function(x) htmltools::tags$option(value = x, x)))}),
    
    
    
 
    
    
    "Neutrophil count" = colDef(
      header = function() {
        tippy("Neutrophil count &#9432;", tooltip = "Definition: Neutrophil count status.", delay = 0, theme = "custom")
      },
      filterInput = function(values, name) {htmltools::tags$select(onchange = sprintf("Reactable.setFilter('df-select', '%s', event.target.value || undefined)", name), htmltools::tags$option(value = "", "All"), lapply(sort(unique(values)), function(x) htmltools::tags$option(value = x, x)))}),
    
    
    
    
    "Ig count" = colDef(
      header = function() {
        tippy("Ig count &#9432;", tooltip = "Definition: Immunoglobulin count status.", delay = 0, theme = "custom")
      },
      filterInput = function(values, name) {htmltools::tags$select(onchange = sprintf("Reactable.setFilter('df-select', '%s', event.target.value || undefined)", name), htmltools::tags$option(value = "", "All"), lapply(sort(unique(values)), function(x) htmltools::tags$option(value = x, x)))}),
    
    
    
    
    "ICD9" = colDef(
      header = function() {
        tippy("ICD9 &#9432;", tooltip = "Definition: International Classification of Diseases, Ninth Revision code.", delay = 0, theme = "custom")
      }
    ),
    "ICD10" = colDef(
      header = function() {
        tippy("ICD10 &#9432;", tooltip = "Definition: International Classification of Diseases, Tenth Revision code.", delay = 0, theme = "custom")
      }
    ),
    "HPO combined" = colDef(
      header = function() {
        tippy("HPO combined &#9432;", tooltip = "Definition: Combined Human Phenotype Ontology (HPO) terms.", delay = 0, theme = "custom")
      }
    ),
    "HPO term" = colDef(
      header = function() {
        tippy("HPO term &#9432;", tooltip = "Definition: Individual Human Phenotype Ontology (HPO) term.", delay = 0, theme = "custom")
      }
    ),
    
    
    "Inheritance detail" = colDef(minWidth = 140), 
    "Major category" = colDef(minWidth = 200), 
    "Subcategory" = colDef(minWidth = 200), 
    "Associated features" = colDef(minWidth = 280), 
    "ICD9" = colDef(minWidth = 100),
    "ICD10" = colDef(minWidth = 100),
    "HPO combined" = colDef(minWidth = 120),
    "HPO term" = colDef(minWidth = 270),
    # "OMIM_ID" = colDef(
    #   minWidth = 140,
    #   cell = function(value, index) {
    #     url <- sprintf("https://www.omim.org/entry/%s", df[index, "OMIM_ID"])
    #     htmltools::tags$a(href = url, target = "_blank", as.character(value))
    #   }
    # ),
    
    "OMIM ID" = colDef(
      minWidth = 140,
      cell = function(value, index) {
        if (is.na(value) || value == "") {
          query <- df[index, "Genetic defect"]
          url <- sprintf("https://www.omim.org/search?index=geneMap&search=%s", query)
          display_text <- "query"
        } else {
          url <- sprintf("https://www.omim.org/entry/%s", value)
          display_text <- as.character(value)
        }
        htmltools::tags$a(href = url, target = "_blank", display_text)
      }
    ),

    "T cell details" = colDef(
      header = function() {
        tippy("T cell details &#9432;", tooltip = "Definition: Additional details regarding T cell status.", delay = 0, theme = "custom")
      }
    ),
    "B cell details" = colDef(
      header = function() {
        tippy("B cell details &#9432;", tooltip = "Definition: Additional details regarding B cell status.", delay = 0, theme = "custom")
      }
    ),
    "Ig details" = colDef(
      header = function() {
        tippy("Ig details &#9432;", tooltip = "Definition: Additional details regarding immunoglobulin levels.", delay = 0, theme = "custom")
      }
    ),
    "Neutrophil details" = colDef(
      header = function() {
        tippy("Neutrophil details &#9432;", tooltip = "Definition: Additional details regarding neutrophil counts.", delay = 0, theme = "custom")
      }
    ),
    
    "Associated features" = colDef(
      header = function() {
        tippy("Associated features &#9432;", tooltip = "Definition: Other clinical features associated with the disease.", delay = 0, theme = "custom")
      }
    ),
    
    
    "OMIM ID" = colDef(
      header = function() {
        tippy("OMIM ID &#9432;", tooltip = "Definition: Online Mendelian Inheritance in Man (OMIM) identifier.", delay = 0, theme = "custom")
      }
    ),
    "Other affected cells" = colDef(
      header = function() {
        tippy("Other affected cells &#9432;", tooltip = "Definition: Other immune or non-immune cells affected.", delay = 0, theme = "custom")
      }
    ),
    "Inheritance detail" = colDef(
      header = function() {
        tippy("Inheritance detail &#9432;", tooltip = "Definition: Further details regarding the mode of inheritance.", delay = 0, theme = "custom")
      }
    ),
    "GOF/DN details" = colDef(
      header = function() {
        tippy("GOF/DN details &#9432;", tooltip = "Definition: Details on gain-of-function or dominant-negative effects.", delay = 0, theme = "custom")
      }
    ),
    
    "Inheritance" = colDef(
      minWidth = 100,
      style = function(value) {
        if (value == "AD/AR") {
          color <- "#962fbf"
        } else if (value == "Variable") {
          color <- "#962fbf"
        } else if (value == "AR") {
          color <- "#4f5bd5"
        } else if (value == "AD") {
          color <- "#d62976"
        } else if (value == "Sporadic/toxin") {
          color <- "#e5ab00"
        } else if (value == "XL") {
          color <- "#fa7e1e"
        } else if (value == "XLR") {
          color <- "#fa7e1e"
        } else if (value == "NA") {
          color <- "#339900"
        } else {
          color <- "black"
        }
        list(color = color)
      }
    )
  )
)

df_t
  










# "UniProt" = colDef(cell = function(value, index) {
#   url <- sprintf("https://www.uniprot.org/uniprot/?query=gene:%s&sort=score", df[index, "Gene symbol"], value)
#   htmltools::tags$a(href = url, target = "_blank", as.character(value))
# }),
# 
# "Ensembl" = colDef(cell = function(value, index) {
#   url <- sprintf("https://www.ensembl.org/Human/Search/Results?q=%s;site=ensembl;facet_species=Human", df[index, "Gene symbol"], value)
#   htmltools::tags$a(href = url, target = "_blank", as.character(value))
# }),
# 
# "OMIM" = colDef(cell = function(value, index) {
#   url <- sprintf("https://www.omim.org/search?index=entry&sort=score+desc%%%%2C+prefix_sort+desc&start=1&limit=10&search=%s", df[index, "Gene symbol"], value)
#   htmltools::tags$a(href = url, target = "_blank", as.character(value))
# }),
# 
# "gnomAD r2 GRCh37" = colDef(cell = function(value, index) {
#   url <- sprintf("https://gnomad.broadinstitute.org/gene/%s?dataset=gnomad_r2_1", df[index, "Gene symbol"], value)
#   htmltools::tags$a(href = url, target = "_blank", as.character(value))
# }),
# 
# "gnomAD r3 GRCh38" = colDef(cell = function(value, index) {
#   url <- sprintf("https://gnomad.broadinstitute.org/gene/%s?dataset=gnomad_r3", df[index, "Gene symbol"], value)
#   htmltools::tags$a(href = url, target = "_blank", as.character(value))
# }),
# 
# "omni" = colDef(cell = function(value, index) {
#   url <- sprintf("https://omni.institutimagine.org/search=%s/page=1", df[index, "Gene symbol"], value)
#   htmltools::tags$a(href = url, target = "_blank", as.character(value))
# }),
# "pdb" = colDef(cell = function(value, index) {
#   url <- sprintf("https://www.rcsb.org/search?request=%%7B%%22query%%22%%3A%%7B%%22type%%22%%3A%%22group%%22%%2C%%22nodes%%22%%3A%%5B%%7B%%22type%%22%%3A%%22group%%22%%2C%%22nodes%%22%%3A%%5B%%7B%%22type%%22%%3A%%22group%%22%%2C%%22nodes%%22%%3A%%5B%%7B%%22type%%22%%3A%%22terminal%%22%%2C%%22service%%22%%3A%%22full_text%%22%%2C%%22parameters%%22%%3A%%7B%%22value%%22%%3A%%22%s%%22%%7D%%7D%%5D%%2C%%22logical_operator%%22%%3A%%22and%%22%%7D%%5D%%2C%%22logical_operator%%22%%3A%%22and%%22%%2C%%22label%%22%%3A%%22full_text%%22%%7D%%5D%%2C%%22logical_operator%%22%%3A%%22and%%22%%7D%%2C%%22return_type%%22%%3A%%22entry%%22%%2C%%22request_options%%22%%3A%%7B%%22paginate%%22%%3A%%7B%%22start%%22%%3A0%%2C%%22rows%%22%%3A25%%7D%%2C%%22scoring_strategy%%22%%3A%%22combined%%22%%2C%%22sort%%22%%3A%%5B%%7B%%22sort_by%%22%%3A%%22score%%22%%2C%%22direction%%22%%3A%%22desc%%22%%7D%%5D%%7D%%2C%%22request_info%%22%%3A%%7B%%22query_id%%22%%3A%%2203e25d3f3eddbc329e8a3c568558431b%%22%%7D%%7D", df[index, "Gene symbol"], value)
#   htmltools::tags$a(href = url, target = "_blank", as.character(value))
# }),
# "ORPHA" = colDef(cell = function(value, index) {
#   url <- sprintf(#"https://hpo.jax.org/app/browse/disease/ORPHA:%s"
#     "https://hpo.jax.org/app/browse/search?q=%s&navFilter=all", df[index, "Gene symbol"], value)
#   htmltools::tags$a(href = url, target = "_blank", as.character(value))
# }),
# "HPO" = colDef(cell = function(value, index) {
#   url <- sprintf("https://hpo.jax.org/app/browse/search?q=%s&navFilter=gene", df[index, "Gene symbol"], value)
#   htmltools::tags$a(href = url, target = "_blank", as.character(value))
# }),
# "HGNC" = colDef(cell = function(value, index) {
#   url <- sprintf("https://hpo.jax.org/app/browse/search?q=%s&navFilter=disease", df[index, "Gene symbol"], value)
#   htmltools::tags$a(href = url, target = "_blank", as.character(value))
# }),
# "AmiGo" = colDef(cell = function(value, index) {
#   url <- sprintf("http://amigo.geneontology.org/amigo/search/bioentity?q=%s&searchtype=geneproduct", df[index, "Gene symbol"], value)
#   htmltools::tags$a(href = url, target = "_blank", as.character(value))
# }),
# "Alpha Fold" = colDef(cell = function(value, index) {
#   url <- sprintf("https://www.alphafold.ebi.ac.uk/search/text/%s", df[index, "Gene symbol"], value)
#   htmltools::tags$a(href = url, target = "_blank", as.character(value))
# }),
# 
# "ClinGen" = colDef(cell = function(value, index) {
#   url <- sprintf("https://search.clinicalgenome.org/kb/genes?page=1&size=50&search=%s", df[index, "Gene symbol"], value)
#   htmltools::tags$a(href = url, target = "_blank", as.character(value))
# }),
# 
# "Gemma" = colDef(cell = function(value, index) {
#   url <- sprintf("https://gemma.msl.ubc.ca/searcher.html?query=%s&scope=G", df[index, "Gene symbol"], value)
#   htmltools::tags$a(href = url, target = "_blank", as.character(value))
# }),
