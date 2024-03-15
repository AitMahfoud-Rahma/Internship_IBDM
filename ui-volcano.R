# ui-volcano-plot.R


fluidPage(fluidRow(column(
  3,
  box(                                       # parameter box
    title = tagList(icon("cogs"), "Volcano Plot Parameters"),
    solidHeader = TRUE,
    status = "primary",
    width = NULL,
    uiOutput("CondvolcanoParams")
  )
),
column(
  9,
  navbarPage("Results",                     # result panels 
             tabPanel(                      # volcano plot panel
               title = tagList(icon("chart-area"), "Volcano Plot"),
               solidHeader = TRUE,
               status = "info",
               width = NULL,
               uiOutput("volcanoUI")
             ),
             tabPanel(                      # all result table
               title = tagList(icon("table"), "Result Table"),
               solidHeader = TRUE,
               status = "info",
               width = NULL,
               uiOutput('MainResultTableVolc')
             ),
             tabPanel(                     # downregulated table
               title = tagList(icon("table"), "Downregulated Table"),
               solidHeader = TRUE,
               status = "info",
               width = NULL,
               DT::dataTableOutput('resultTabledown')
             ),
             tabPanel(                    # upregulated table
               title = tagList(icon("table"), "Upregulated Table"),
               solidHeader = TRUE,
               status = "info",
               width = NULL,
               DT::dataTableOutput('resultTableup')
             )
  ))))