#ui-flyenrichr.R


fluidPage(fluidRow(column(
  3,
  box(                          #parameter box 
    title = tagList(icon("tools"), "Parameters"),
    width = NULL,
    solidHeader = TRUE,
    status = "primary",
    tagList(
      uiOutput('EnrichParams'),
      uiOutput("EnrichfiltParams"),
      textAreaInput(
        "list_ids",
        "Paste Symbol List",
        rows = 5,
      ),
      selectInput(
        "inputorg",
        "Organism",
      c("Drosophila melanogaster" = "dmelanogaster"
      )),
      selectInput("databases", "Choose databases for enrichment",
                     choices = c("GO : CC","GO:MF","CO GO:BP",  "KEGG 19","GeneRIF","PPI: DroID 17","Wiki Pathways 18" ),
                     multiple = TRUE),
      sliderInput(
        "topres_enrich",
        "Top results to show",
        min = 1,
        max = 50,
        value = 10,
        step = 1
      ),
      
      do.call(actionBttn, c(          # run button 
        list(
          inputId = "enrichRgo",
          label = "Enrich",
          icon = icon("play")
        )))
    ))),
#result table 
column(
  9,
  navbarPage(theme=shinytheme("sandstone"),"Results",
             id = "entabs",
             tabPanel(
               title = tagList(icon("question"), "Info"), # info panel
               width = NULL,
               solidHeader = TRUE,
               status = "primary",
               includeMarkdown("documents/flyenrichinfo.Rmd")
             ),
             tabPanel(
               title = tagList(icon("table"), "Result Table"), # result table panel 
               value = 'redirectres',
               width = NULL,
               solidHeader = TRUE,
               status = "primary",
               uiOutput('EnrichResults')
             ),
             tabPanel(
               title = tagList(icon("braille"), "Bar Chart"), # bar chart panel
               width = NULL,
               solidHeader = TRUE,
               status = "primary",
               plotlyOutput('barenrich',height = 800)%>% withSpinner()
             )
  ))))
