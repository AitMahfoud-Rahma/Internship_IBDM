#ui-conversion.R



fluidPage(fluidRow(column(
  3,
  box(
    title = tagList(icon("cogs"), "Parameters"),
    width = NULL,
    solidHeader = TRUE,
    status = "primary",
    tagList(
      selectInput(   # organism
        "chosendatabase",
        "Choose your Organism",
        c("Drosophila melanogaster" = "org.Dm.eg.db")
      ),
      selectInput(   # inoput type 
        "inputtype",
        "Choose your input type",
        c("EntrezID" = "ENTREZID",
          "EnsemblID" = "ENSEMBL",
          " Symbol" = "SYMBOL")
      ),
      textAreaInput(  # gene list
        "inputids",
        "Paste Gene List",
        rows = 5,
        placeholder = "Input genes, one gene per line."
      )
    )
  ),
  do.call(actionBttn, c( # run button
    list(
      inputId = "convgo",
      label = "Convert",
      icon = icon("play")
    )))
),

#result table 
column(
  9,
  navbarPage("Results",
             id = "convtabs",
             tabPanel(
               title = tagList(icon("question"), "Info"), # info panel
               width = NULL,
               solidHeader = TRUE,
               status = "primary",
               includeMarkdown("documents/IDconversioninfo.Rmd")
             ),
             tabPanel(
               title = tagList(icon("table"), "Result Table"), # result table panel
               value = 'redirectconv',
               width = NULL,
               solidHeader = TRUE,
               status = "primary",
               uiOutput('ConversionResults')
             )
  ))))