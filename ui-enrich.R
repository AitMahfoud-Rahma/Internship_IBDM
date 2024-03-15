#ui-enrich.R


fluidPage(fluidRow(column(
  3,
  box(                          #parameter box 
    title = tagList(icon("cogs"), "Parameters"),
    width = NULL,
    solidHeader = TRUE,
    status = "primary",
    tagList(
    uiOutput('EnrichParams'),
    uiOutput("EnrichfiltParams"),
    textAreaInput(
      "list_ids",
      "Paste Gene List",
      rows = 5,
    ),
     selectInput(
      "enrichment_type",
      "Choose Enrichment Type",
      choices = c("gProfiler", "EnrichR"),
      selected = "gProfiler"
    ),
    sliderInput(
      "topres_enrich",
      "Top results to show",
      min = 1,
      max = 50,
      value = 10,
      step = 1
    ),
    selectInput(
      "inputorg",
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
    checkboxGroupInput(
      "chosenEnrich",
      "Choose your enrichment",
      c("GO : Biological Processes" = "GO:BP",
        "GO : Molecular Functions" = "GO:MF",
        "GO : Cellular Components" = "GO:CC",
        "KEGG Pathways" = "KEGG",
        "Reactome" = "REAC",
        "WikiPathways" = "WP",
        "TRANSFAC" = "TF",
        "MirTarBase" = "MIRNA",
        "Human Phenotype Ontology" = "HP",
        "Human Protein Atlas" = "HPA",
        "CORUM" = "CORUM")
    ),
    selectInput(
      "correction",
      "Multiple testing correction",
      c("gSCS threshold" = "gSCS",
        "Benjamini-Hochberg FDR" = "fdr", 
        "Bonferroni correction" = "bonferroni")
    ),
    sliderInput(
      "userpval_cutoff",
      "Significant p-value threshold",
      min = 0,
      max = 1,
      value = 0.05,
      step = 0.01
    ), 
    selectInput(
      "chosenscope",
      "Background list",
      c("Only annotated genes" = "annotated",
        "All known genes" = "known")
    ),
    do.call(actionBttn, c(          # run button 
      list(
        inputId = "enrichmentgo",
        label = "Enrich",
        icon = icon("play")
      )))
  ))),
    #result table 
    column(
      9,
      tabsetPanel(
        id = "entabs",
        type = "tabs",
        tabPanel(
          "Info",
          icon = icon("question"),
          width = NULL,
          solidHeader = TRUE,
          status = "primary",
          includeMarkdown("documents/goinfo.Rmd")
        ),
        tabPanel(
          "Result Table",
          icon = icon("table"),
          value = 'redirectres',
          width = NULL,
          solidHeader = TRUE,
          status = "primary",
          uiOutput('EnrichResults')
        ),
        tabPanel(
          "Bar Chart",
          icon = icon("braille"),
          width = NULL,
          solidHeader = TRUE,
          status = "primary",
          uiOutput('EnrichBar', height = 1000) %>% withSpinner()
        ),
        # Define additional tabs for other pages as needed (e.g., EnrichR page)
        tabPanel(
          "EnrichR Page",
          value = 'enrichR_page',
          icon = icon("star"),
          width = NULL,
          solidHeader = TRUE,
          status = "primary",
          uiOutput('EnrichR_UI'),
          includeMarkdown("documents/flyenrichinfo.Rmd")
        )
      )
    )
  )
)

