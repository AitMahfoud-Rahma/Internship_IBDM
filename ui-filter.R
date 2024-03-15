# ui-filter.R



fluidPage(useSweetAlert(), fluidPage(fluidRow(
  column(
3,
box(                   # parameter box
  title = tagList(icon("cogs"), "Parameters"),
  width = NULL,
  solidHeader = TRUE,
  status = "primary",
  uiOutput('condFilter')

)),
column(9,
       box(    # group assignement box
         title = tagList(icon("tags"), "Normalized Table Check"),
         solidHeader = TRUE,
         status = "primary",
         width = NULL,
         DT::dataTableOutput('filterTable')
       
       ),
       box(    # group assignement box
         title = tagList(icon("tags"), "DEGs Table Check"),
         solidHeader = TRUE,
         status = "primary",
         width = NULL,
         DT::dataTableOutput('filterTableDEG')
       )))))
