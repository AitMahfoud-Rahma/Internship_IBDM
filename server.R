# Define server of the app
options(shiny.maxRequestSize = 100*1024^2) # maximum size up to 100MB

shinyServer(function(input, output, session) { #all server files 
  
  source(file = "server-data-import.R",
         local = TRUE,
         encoding = "UTF-8")
  source(file = "server-deanalysis.R",
         local = TRUE,
         encoding = "UTF-8")
  source(file = "server-pca.R",
         local = TRUE,
         encoding = "UTF-8")
  source(file = "server-heatmap.R",
         local = TRUE,
         encoding = "UTF-8")
  source(file = "server-filter.R",
         local = TRUE,
         encoding = "UTF-8")
  source(file = "server-volcano.R",
         local = TRUE,
         encoding = "UTF-8")
  source(file = "server-ma.R",
         local = TRUE,
         encoding = "UTF-8")
  source(file = "server-mfuzz.R",
         local = TRUE, 
         encoding = "UTF-8")
  source(file = "server-enrich.R",
         local = TRUE,
         encoding = "UTF-8")
  source(file = "server-ortho.R",
         local = TRUE, 
         encoding = "UTF-8")
  source(file = "server-conversion.R",
         local = TRUE, 
         encoding = "UTF-8")
  
  # reactive variables that are saved and can be use in another file than the original where it was created
  var = reactiveValues(
    count = NULL,                        #
    InputTable = data.frame(),           # original input dataa frame
    LowCountGenes = data.frame(),        # data frame of filtered data from the original one
    CountData = data.frame(),            # input data frame after filtering low count genes 
    groupdf = data.frame(),              # data frame from the group assignement, associating replicates to groups
    matrixcount = matrix(),              # matrix of CountData
    groupList = NULL,                    # list of the groups only
    selectedgroups = NULL,               # selected groups of the original data frame is not all are selected
    mfuzzTable = data.frame(),           # mfuzz upload table    
    mfuzzCountData = data.frame(),       # mfuzz data table
    timepoints = NULL,
    design = NULL,                       # design for deseq2 and edgeR
    DEAMETHOD = NULL,                    # chosen analysis method
    result = data.frame("Results will show here." = character(0)), #vmessage to put in the beginning when no input yet
    tccObject = NULL,                    # tcc object containing results of the tcc calculation 
    norData = matrix(),                  # matrix of normalized counts
    pcadata = NULL)                      # pca data 
  
})
