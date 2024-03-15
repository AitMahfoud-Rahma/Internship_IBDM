# ui-normalization.R

fluidPage(fluidRow(column(
  3,
  box(                   # parameter box
    title = tagList(icon("cogs"), "Parameters"),
    width = NULL,
    solidHeader = TRUE,
    status = "primary",
    uiOutput("DEAParams")
    )),
  
  #result table 
  column(
    9,
    navbarPage("Results",             # result panels
               id = "tabs",           # id to redirect
               tabPanel(              # Rmd informations panel
                 title = tagList(icon("question"), "TCC info"),
                 width = NULL,
                 solidHeader = T,
                 status = "primary",
                 includeMarkdown("documents/tccinfo.Rmd")
               ),
               tabPanel(             # Normalized data table
                 title = tagList(icon("table"), "Normalization Table"),
                 width = NULL,
                 solidHeader = TRUE,
                 status = "primary",
                 uiOutput("NormResultTable")
               ),
               tabPanel(            # all results table
                 title = tagList(icon("table"), "Result Table"),
                 value = 'redirectres', # redirection to this table when the calculation is done
                 width = NULL,
                 solidHeader = TRUE,
                 status = "primary",
                 uiOutput("mainResultTable")
               ),
               tabPanel(           # only DEG results table
                 title = tagList(icon("table"), "DEG Table"),
                 width = NULL,
                 solidHeader = TRUE,
                 status = "primary",
                 uiOutput("mainsortedResultTable")
               )
    ))))