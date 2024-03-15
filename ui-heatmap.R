# ui-heatmap.R

fluidPage(fluidRow(column(
  3,
  box(                               # parameter box
    title = tagList(icon("cogs"), "Heatmap Parameters"),
    width = NULL,
    solidHeader = TRUE,
    status = "primary",
    uiOutput("CondHeatmapParams")
    )),
  column(
    9,
    navbarPage("Results",           # result panels
               tabPanel(            # heatmap panel
                 title = tagList(icon("delicious"), "Heatmap"),
                 width = NULL,
                 solidHeader = TRUE,
                 status = "info", 
                 uiOutput("heatmapPlot"),
                 footer = "This part is time consuming, please wait patiently.",
               ),
               tabPanel(           # result table panel
                 title = tagList(icon("table"), "Result table"),
                 width = NULL,
                 solidHeader = TRUE,
                 status = "info", 
                 DT::dataTableOutput("resultTableInHeatmap")
               )
    )
    )
  )
  )