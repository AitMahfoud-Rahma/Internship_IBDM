# server-normalization.R


AnalysisRun <- reactiveValues(AnalysisRunValue = FALSE)# to precise the run button has not been clicked



output$CondDEAParams <- renderUI({  # if a count data table has been uploaded then it shows the parameters
  if (v$importActionValue){
    uiOutput("DEAParams")
  }else{                            # if not, error message to do it 
    sendSweetAlert(
      session = session,
      title = "ERROR",
      text = "You must upload a count data fist.",
      type = "info"
    )
    helpText("Please upload a count data first.")
  }
})


output$DEAParams <- renderUI({ 
  tagList(
    selectInput(
      "DEAmethod", "Analysis Method",
      c( TCC = "tcc",
         DESeq2 = "DESeq2",
         edgeR = "edgeR")),
    conditionalPanel(
      condition = "input.DEAmethod == 'tcc'",
      uiOutput("TCCParams")
    ),
    conditionalPanel(
      condition = "input.DEAmethod == 'DESeq2'",
      uiOutput("DESeq2Params")
    ),
    conditionalPanel(
      condition = "input.DEAmethod == 'edgeR'",
      uiOutput("edgeRParams")
    ), 
    do.call(actionBttn, c(           # run button 
      list(
        inputId = "DEA",
        label = "Run Analysis",
        icon = icon("play")
      ))))
})

output$TCCParams <- renderUI({# set of paramters for tcc method
  tagList(
    selectInput(
      "normMethod",
      "Normalization Method",
      c("TMM" = "tmm",
        "DESeq2" = "deseq2")
    ),
    selectInput(
      "testMethod",
      "DEG Identification Method",
      c(
        "edgeR" = "edger",
        "DESeq2" = "deseq2",
        "baySeq" = "bayseq"
      )),
    numericInput(
      inputId = "fdr",
      label = "FDR Cut-off",
      min = 0,
      value = 0.01,
      max = 1,
      step = 0.0001
    ),
    sliderInput(
      "floorpdeg",
      "Elimination of Potential DEGs",
      min = 0,
      max = 1,
      value = 0.05,
      step = 0.01
    )
  )
})

output$DESeq2Params <- renderUI({ # set of paramters for deseq2 method 
  numericInput(
    "deseq2cutoff",
    "FDR Cut-Off",
    min = 0,
    max = 1,
    value = 0.01
  )
})

output$edgeRParams <- renderUI({ # set of paramters for edgeR method 
  tagList(
    selectInput(
      "edgeRMethod",
      "Normalization Method",
      c("TMM" = "TMM",
        "RLE" = "RLE",
        "upperquartile" = "upperquartile",
        "none" = "none")
    ),
    numericInput(
      inputId = "edgeRfdr",
      label = "FDR Cut-off",
      min = 0,
      value = 0.001,
      max = 1,
      step = 0.0001
    ))
})
observeEvent(input$DEA, {           # when the run button is clicked 
  progressSweetAlert(               # progress bar 
    session = session,
    id = "DEAnalysisProgress",
    title = "Work in progress",
    display_pct = TRUE,
    value = 0
  )
  
  var$newData <- var$CountData
  
  if(input$DEAmethod == "tcc"){

  # Creation of a TCC Object 
  tcc <-                           
    new("TCC", var$newData, var$selectedgroups)
  var$tccObject <- tcc             # save the object

  
  updateProgressBar(               # updating progress bar
    session = session,
    id = "DEAnalysisProgress",
    title = "DE Analysis in progress...",
    value = 50
  )
  tcc <- calcNormFactors(         # first calculation of the normalization and estimation of DEGs
    tcc,
    norm.method = input$normMethod,
    test.method = input$testMethod,
    FDR = input$fdr,
    floorPDEG = input$floorpdeg,
    iteration = 3                # iteration value set to 3 
  )
  
  updateProgressBar(             # updating progress bar 
    session = session,
    id = "DEAnalysisProgress",
    title = "DE Analysis in progress...",
    value = 75
  )
  tcc <- estimateDE(tcc,        # final estimation of the DEGs 
                    test.method = input$testMethod,
                    FDR = input$fdr)
  
  
  var$tccObject <- tcc         # save the updated object 
  var$result <- getResult(tcc, sort = FALSE) %>% mutate_if(is.factor, as.character) # get the result of the calculation
  
  if (length(var$groupList2) == 2){
    var$result_m <- var$result
    colnames(var$result_m) <- c("gene_id","BaseMean", "Log2FC","P-Value", "FDR", "Rank", "estimatedDEG")
    var$result_e <- var$result[which(var$result_m$estimatedDEG >0),] # selection of the DEGs
    var$result_s <- var$result_e[,-7]      # deleting the column showing which one is a DEG and which one is not
  }
  else{
    var$result_a <- var$result[,-2]        # deleting the a value (Basemean) of the results
    var$result_m <- var$result_a[,-2]      # deleting the m value (Log2FC) of the results
    colnames(var$result_m) <- c("gene_id", "P-Value", "FDR", "Rank", "estimatedDEG")
    var$result_e <- var$result_m[which(var$result_m$estimatedDEG >0),] # selection of the DEGs
    var$result_s <- var$result_e[,-5]      # deleting the column showing which one is a DEG and which one is not
  }
  
  var$norData <- tcc$getNormalizedData() # only the normalized data
  var$norDT <- var$norData
  var$genelist <- var$result_s[,1]
  var$DEAMETHOD <- 'tcc'
  }

  
  ######################################### deseq2 method #################################################
  
  
 if(input$DEAmethod == "DESeq2"){

   tcc <-                           
     new("TCC", var$newData, var$selectedgroups)
   var$tccObject <- tcc             # just to get the groups for pca 
   dds <- DESeqDataSetFromMatrix(countData=var$newData, colData=var$select, design=var$design)
   
   updateProgressBar(               # updating progress bar
     session = session,
     id = "DEAnalysisProgress",
     title = "DE Analysis in progress...",
     value = 25
   )
   
   dds <- DESeq(dds)
    
    updateProgressBar(               # updating progress bar
      session = session,
      id = "DEAnalysisProgress",
      title = "DE Analysis in progress...",
      value = 50
    )
    var$resultz <- results(dds)
    var$norData <- as.matrix(counts(dds, normalized = TRUE)) # normalization
    var$norDT <- var$norData
    var$resultz <- as.matrix(var$resultz)
    var$result <- data.frame(row.names(var$resultz))
    var$result['a.value'] <- var$resultz[,1]
    var$result['m.value'] <- var$resultz[,2]
    var$result['p.value'] <- var$resultz[,6]
    var$result['q.value'] <- p.adjust(var$resultz[,6], method = 'fdr')
    names(var$result)[1] <- "gene_id"
    

    
    if (length(var$groupList2) != 2){
      var$result <- var$result[,-2] # suppr basemean
      var$result <- var$result[,-2] # supp log2fc
    }

    var$DESeq2DEGs <- var$result[which(var$result$q.value <= as.numeric(input$deseq2cutoff)),] 
    var$result["estimatedDEG"] = "0"
    var$result <- var$result[complete.cases(var$result), ]
    for (row in 1:nrow(var$result)){
      if(var$result[row,'q.value'] <= as.numeric(input$deseq2cutoff)){
        var$result[row, 'estimatedDEG'] = "1"
      }else{ 
        var$result[row,'estimatedDEG'] = "0"
      }
      
    }
   
    var$genelist <- var$DESeq2DEGs[,1]
    var$DEAMETHOD <- 'deseq2'
    }
 
  ################################################ edgeR method ###############################################
  
  
  if(input$DEAmethod == "edgeR"){     # formatting for edgeR'''
    tcc <-                           
      new("TCC", var$newData, var$selectedgroups)
    var$tccObject <- tcc             # just to get the groups for pca 
    
    dgList <- DGEList(counts=var$newData, group = var$selectedgroups)
    
    updateProgressBar(               # updating progress bar
      session = session,
      id = "DEAnalysisProgress",
      title = "DE Analysis in progress...",
      value = 25
    )
    
    dgList <- calcNormFactors(dgList, method=input$edgeRMethod)
    
    updateProgressBar(               # updating progress bar
      session = session,
      id = "DEAnalysisProgress",
      title = "DE Analysis in progress...",
      value = 50
    )
    
    dgList <- estimateGLMCommonDisp(dgList,
                                    method = "deviance", robust = TRUE,
                                    subset = NULL)
    design <- model.matrix(~var$selectedgroups)
    fit <- glmFit(dgList, design)
    lrt <- glmLRT(fit) 
    var$result <- data.frame(row.names(lrt$table))
    var$result['a.value'] <- lrt$table$logCPM
    var$result['m.value'] <- lrt$table$logFC
    var$result['p.value'] <- lrt$table$PValue
    var$result['q.value'] <- p.adjust(var$result$p.value, method = 'fdr')
    names(var$result)[1] <- "gene_id"

    

    
    if (length(var$groupList2) != 2){
      var$result <- var$result[,-2] # suppr log2fc
      var$result <- var$result[,-2] # supp basemean
    }
    var$norData <- lrt$fitted.values
    var$norDT <- var$norData
    var$result["estimatedDEG"] = "0"
    for (row in 1:nrow(var$result)){
      if(var$result[row,'q.value'] <= as.numeric(input$edgeRfdr)){
        var$result[row, 'estimatedDEG'] = "1"
      }else{ 
       var$result[row,'estimatedDEG'] = "0"
      }}
    var$edgeRDEGs <- var$result[which(var$result$q.value <= as.numeric(input$edgeRfdr)),] 
    var$edgeRDEGs <- var$edgeRDEGs[,-4]
    var$genelist <- var$edgeRDEGs[,1]
    var$DEAMETHOD <- 'edgeR'

  }
################################

  output$normresultTable <- DT::renderDataTable({  # normaliszed data table
    data <- var$norData
    DT::datatable(
      data,        
      extensions = 'Buttons',                      # download button 
      option = list(
        paging = TRUE,
        searching = TRUE,
        fixedColumns = TRUE,
        autoWidth = TRUE,
        ordering = TRUE,
        dom = 'Bfrtip',
        buttons = list(list(
          extend = 'collection',
          buttons = list(extend='csv',
                         filename = "results_norm_data"),
          text = 'Download')),
        scrollX = TRUE,
        pageLength = 10,
        searchHighlight = TRUE,                  # search bar 
        orderClasses = TRUE
        
      ),
      
      class = "display")
  }, server = FALSE)
  
  output$fullresultTable <- DT::renderDataTable({   # full results table where genes under the cut off are colored in red
    data <- var$norData

    if(input$DEAmethod =="tcc"){
      gene_id <- row.names(data)
      data <- cbind(data, gene_id = gene_id)
    resultTable <- merge(var$result_m, data, by = "gene_id")
    }else{
      data <- as.data.frame(data)
      data['gene_id'] <- row.names(data)
      resultTable <- merge(var$result, data, by = "gene_id")
    }
    
    DT::datatable(
      resultTable,        
      extensions = 'Buttons',
      option = list(
        paging = TRUE,
        searching = TRUE,
        fixedColumns = TRUE,
        autoWidth = TRUE,
        ordering = TRUE,
        dom = 'Bfrtip',
        buttons = list(list(
          extend = 'collection',
          buttons = list(extend='csv',
                         filename = "results_dea"),
          text = 'Download')),
        scrollX = TRUE,
        pageLength = 10,
        searchHighlight = TRUE,
        orderClasses = TRUE
        
      ),
      
      class = "display",
      caption = tags$caption(
        tags$li(
          HTML("<font color=\"#B22222\"><b>Gene Name</b></font> is colored when under FDR cut-off")
        )
      ))%>% formatStyle(
        "gene_id",
        "estimatedDEG",
        color = styleEqual(1, "#B22222"),
        fontWeight = styleEqual(c(0, 1), c("normal", "bold"))
      )
  }, server = F)
  
  output$sortedresultTable <- DT::renderDataTable({            # only DEGs table 
    data <- var$norData
    if(input$DEAmethod == 'tcc'){
      gene_id <- row.names(data)
      data <- cbind(data, gene_id = gene_id)
    resultTable <- merge(var$result_s, data, by = "gene_id")
    }
    if(input$DEAmethod == "DESeq2"){
      gene_id <- row.names(data)
      data <- cbind(data, gene_id = gene_id)
      resultTable <- merge(var$DESeq2DEGs,data, by = "gene_id")
    }
    if(input$DEAmethod == "edgeR"){
      gene_id <- row.names(data)
      data <- cbind(data, gene_id = gene_id)
      resultTable <- merge(var$edgeRDEGs, data, by = "gene_id")
    }
    
    DT::datatable(
      resultTable,        
      extensions = 'Buttons',
      option = list(
        paging = TRUE,
        searching = TRUE,
        fixedColumns = TRUE,
        autoWidth = TRUE,
        ordering = TRUE,
        dom = 'Bfrtip',
        buttons = list(list(
          extend = 'collection',
          buttons = list(extend='csv',
                         filename = "results_DEGs"),
          text = 'Download')),
        scrollX = TRUE,
        pageLength = 10,
        searchHighlight = TRUE,
        orderClasses = TRUE
        
      ),
      
      class = "display")
  }, server = FALSE)
  
  closeSweetAlert(session = session)       # close alert precising the calculation is done
  sendSweetAlert(session = session,
                 title = "DONE",
                 text = "DE Analysis was successfully performed.",
                 type = "success")
  
  
  AnalysisRun$AnalysisRunValue <- input$DEA  # precise the run button has been clicked 
  updateNavbarPage(session, "tabs", "redirectres") # redirection to the full result table
  
})
resultTable <- reactive({   # saving the updated results to plot furtherly 
  var$result
})


# results tables render

output$genelist <- renderUI({
  if(AnalysisRun$AnalysisRunValue){ # if the calculation is done then show the tables 
    tagList(
      fluidRow(column(
        12, DT::dataTableOutput('genelistTable') %>% withSpinner()
      )))} else {                       # if not, message to do it 
        helpText("Run Normalization to obtain Result Table.")
      }
})


output$NormResultTable <- renderUI({
  if(AnalysisRun$AnalysisRunValue){ # if the calculation is done then show the tables 
    tagList(
      fluidRow(column(
        12, DT::dataTableOutput('normresultTable') %>% withSpinner()
      )))} else {                       # if not, message to do it 
        helpText("Run Normalization to obtain Result Table.")
      }
})


output$mainResultTable <- renderUI({
  if(AnalysisRun$AnalysisRunValue){    # if the calculation is done then show the tables 
    tagList(
      fluidRow(column(
        12, DT::dataTableOutput('fullresultTable') %>% withSpinner()
      )))} else {                       # if not, message to do it 
        helpText("Run Normalization to obtain Result Table.")
      }
})


output$mainsortedResultTable <- renderUI({
  if(AnalysisRun$AnalysisRunValue){    # if the calculation is done then show the tables 
    tagList(
      fluidRow(column(
        12, DT::dataTableOutput('sortedresultTable') %>% withSpinner()
      )))} else {                      # if not, message to do it 
        helpText("Run Normalization to obtain Result Table.")
      }
})

