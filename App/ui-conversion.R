#ui-conversion.R
# visible part of the ID conversion tool including selection of the organism, 
# the type of input and the place to paste the gene set. 
# provide a panel with a Rmd of informations and a panel with a result table

fluidPage(fluidRow(column(
  3,
  box(                                                 #parameter box
    title = tagList(icon("cogs"), "Parameters"),
    width = NULL,
    solidHeader = TRUE,
    status = "primary",
    tagList(                                           # set of parameters
      selectInput(
        "chosendatabase",
        "Choose your Organism",
        c("Drosophila melanogaster" = "dmelanogaster",
          "Mus musculus" = "mmusculus",
          "Homo sapiens" = "hsapiens", 
          "Caenorhabditis elegans" = "celegans",
          "Zebrafish" = "drerio",
          "Aspergillus fumigatus Af293" = "afumigatus",
          "Bonobo" = "ppaniscus",
          "Cat" = "fcatus",
          "Chicken" = "ggallus",
          "Chimpanzee" = "ptroglodytes",
          "Common Carp" = "ccarpio",
          "Cow" = "btaurus",
          "Dog" = "clfamiliaris",
          "Dolphin" = "ttruncatus",
          "Goat" = "chircus",
          "Gorilla" = "ggorilla",
          "Guppy" = "preticulata",
          "Horse" = "ecaballus",
          "Pig" = "sscrofa",
          "Platypus" = "oanatinus",
          "Rabbit" = "ocuniculus")
      ),
      selectInput(
        "outputtype",
        "Choose your target type",
        c("EntrezID" = "ENTREZGENE_ACC",
          "Ensembl Gene ID" = "ENSG",
          "Ensembl Protein ID" = "ENSP",
          "EMBL" = "EMBL",
          "Wikigene" = "WIKIGENE_ACC",
          "Protein ID" = "PROTEIN_ID")
      ),
      textAreaInput(
        "inputids",
        "Paste Gene List",
        rows = 5,
        placeholder = "Input genes, one gene per line."
      )
    )
  ),
  do.call(actionBttn, c(                           #validation button 
    list(
      inputId = "convgo",
      label = "Convert",
      icon = icon("play")
    )))
),




column(
  9,
  navbarPage("Results",
             id = "convtabs",
             tabPanel(                         # panel of information 
               title = tagList(icon("question"), "Info"),
               width = NULL,
               solidHeader = TRUE,
               status = "primary",
               includeMarkdown("documents/convinfo.Rmd")
             ),
             tabPanel(                        # panel of result table 
               title = tagList(icon("table"), "Result Table"),
               value = 'redirectconv',
               width = NULL,
               solidHeader = TRUE,
               status = "primary",
               uiOutput('ConversionResults')
             )
  ))))