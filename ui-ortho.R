#ui-ortho.R


fluidPage(fluidRow(column(
  3,
  box(                          #parameter box 
    title = tagList(icon("cogs"), "Parameters"),
    width = NULL,
    solidHeader = TRUE,
    status = "primary",
    tagList(                    # set of parameters
      textAreaInput(
        "ortho_ids",
        "Paste Gene List",
        rows = 5,
        placeholder = "Input ids, one gene per line."
      ),
      selectInput(
        "orthinputorg",
        "Choose your Organism",
        c("Drosophila melanogaster" = "dmelanogaster",
          "Mus musculus" = "mmusculus",
          "Homo sapiens" = "hsapiens", 
          "Caenorhabditis elegans" = "celegans",
          "Escherichia coli" = "ecoli",
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
        "orthtargetorg",
        "Choose your target Organism",
        c("Drosophila melanogaster" = "dmelanogaster",
          "Mus musculus" = "mmusculus",
          "Homo sapiens" = "hsapiens", 
          "Caenorhabditis elegans" = "celegans",
          "Escherichia coli" = "ecoli",
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
      )
    ),
    do.call(actionBttn, c(          # run button 
      list(
        inputId = "orthogo",
        label = "Ortho",
        icon = icon("play")
      )))
  )),
  
  #result table 
  column(
    9,
    navbarPage(           # result panels
      theme=shinytheme("sandstone"),"Results",
      id = "ortabs",
      tabPanel(  # Rmd info panel
        title = tagList(icon("question"), "Info"),
        width = NULL,
        solidHeader = TRUE,
        status = "primary",
        includeMarkdown("documents/orthoinfo.Rmd")
      ),
      tabPanel(  # result table panel
        title = tagList(icon("table"), "Result Table"),
        value = 'redirectres',
        width = NULL,
        solidHeader = TRUE,
        status = "primary",
        uiOutput('OrthoResults')
      )
    ))))