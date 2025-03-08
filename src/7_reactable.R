# for value coloring, there can be no NAs (but character string "NA")
library(reactable)
library(htmltools)


# reactable ----
library(reactable)
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
    "Disease" = colDef(minWidth = 200), 
    "Inheritance" = colDef(
        filterInput = function(values, name) {htmltools::tags$select(onchange = sprintf("Reactable.setFilter('df-select', '%s', event.target.value || undefined)", name), htmltools::tags$option(value = "", "All"), lapply(sort(unique(values)), function(x) htmltools::tags$option(value = x, x)))}),
    "T cell count" = colDef(
      filterInput = function(values, name) {htmltools::tags$select(onchange = sprintf("Reactable.setFilter('df-select', '%s', event.target.value || undefined)", name), htmltools::tags$option(value = "", "All"), lapply(sort(unique(values)), function(x) htmltools::tags$option(value = x, x)))}),
    
    "B cell count" = colDef(
      filterInput = function(values, name) {htmltools::tags$select(onchange = sprintf("Reactable.setFilter('df-select', '%s', event.target.value || undefined)", name), htmltools::tags$option(value = "", "All"), lapply(sort(unique(values)), function(x) htmltools::tags$option(value = x, x)))}),
    
    "Neutrophil count" = colDef(
      filterInput = function(values, name) {htmltools::tags$select(onchange = sprintf("Reactable.setFilter('df-select', '%s', event.target.value || undefined)", name), htmltools::tags$option(value = "", "All"), lapply(sort(unique(values)), function(x) htmltools::tags$option(value = x, x)))}),
    
    "Ig count" = colDef(
      filterInput = function(values, name) {htmltools::tags$select(onchange = sprintf("Reactable.setFilter('df-select', '%s', event.target.value || undefined)", name), htmltools::tags$option(value = "", "All"), lapply(sort(unique(values)), function(x) htmltools::tags$option(value = x, x)))}),
    
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


    # "Disease" = colDef(minWidth = 200), 
    # 
    # "Inheritance" = colDef(
    #   minWidth = 140,
    #   filterInput = selectFilter
    # ),
    # "T cell count" = colDef(
    #   minWidth = 140,
    #   filterInput = selectFilter
    # ),
    # "B cell count" = colDef(
    #   minWidth = 140,
    #   filterInput = selectFilter
    # ),
    # "Ig count" = colDef(
    #   minWidth = 140,
    #   filterInput = selectFilter
    # ),
    # "Neutrophil count" = colDef(
    #   minWidth = 140,
    #   filterInput = selectFilter
    # ),








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
