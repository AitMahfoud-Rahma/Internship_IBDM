# ui-ma-plot.R

fluidPage(fluidRow(column(
  3,
  box(                                 # parameter box
    title = tagList(icon("cogs"), "MA Plot Parameters"),
    solidHeader = TRUE,
    status = "primary",
    width = NULL,
    uiOutput("CondMAPlotParams")
  ),
),
column(
  9,
  navbarPage("Results",               # result panels
             tabPanel(                # MA plot panel
               title = tagList(icon("chart-line"), "MA plot"),
               width = NULL,
               solidHeader = TRUE,
               status = "info", 
               uiOutput("MAPlotUI"),
             ),
             tabPanel(               # result table panel
               title = tagList(icon("table"), "Result table"),
               width = NULL,
               solidHeader = TRUE,
               status = "info", 
               DT::dataTableOutput("resultTableInPlot") # dataframe output
             )
))))