# server-filter.R

FilterRun <- reactiveValues(FilterRunValue = FALSE)# to precise the run button has not been clicked
output$condFilter<- renderUI({
  
  if (AnalysisRun$AnalysisRunValue){  # if a DEA has been performed, then show parameters
    tagList(
      selectizeInput("filter_samples", label="Select Samples",
                     choices=var$cond_id2,
                     selected = var$cond_id2,
                     multiple=TRUE),
      helpText(HTML("Choose the 2 groups you want to study for a two groups comparison : MA plot and Volcano plot")),
      do.call(actionBttn,c(   # run button 
        list(
          inputId = "confirmed2groups",
          label = "Confirm groups",
          icon = icon("play")
        ))
      ),
      helpText(HTML("Reset groups to come back to the original normalized table and choose other groups")),
      do.call(actionBttn,c(   # run button 
        list(
          inputId = "resetButton",
          label = "Reset Groups",
          icon = icon("play")
        ))
      )
    )
  }else{                              # if not, error message to do it
    sendSweetAlert(
      session = session,
      title = "ERROR",
      text = "You must perform a DEA before.",
      type = "info"
    )
    helpText("Please perform a DEA first.")
  }

  
  
})

observeEvent(input$resetButton,{
  tagList(
  updateSelectInput(
    session = getDefaultReactiveDomain(),
    'filter_samples',
    label="Select Samples",
    choices=var$cond_id2,
    selected = var$cond_id2),
  helpText(HTML("Choose the 2 groups you want to study for a two groups comparison : MA plot and Volcano plot"))
  )
  var$norData <- var$norDT
  var$newData <- var$CountData
  
})

observeEvent(input$filter_samples,{
  selectedsamples <- input$filter_samples
  tmprem = match(as.character(var$sampleid2[which(!(var$cond_id2%in%selectedsamples))]),colnames(var$norData))
  tmpkeep = setdiff(1:ncol(var$norData),tmprem)
  var$norData = var$norData[,tmpkeep]
  tmprem2 = match(as.character(var$sampleid2[which(!(var$cond_id2%in%selectedsamples))]),colnames(var$newData))
  tmpkeep2 = setdiff(1:ncol(var$newData),tmprem2)
  var$newData = var$newData[,tmpkeep2]
  var$sampleid3 <- colnames(var$newData)
  var$actualgroups2 <- do.call(rbind,strsplit(var$sampleid3,"_",fixed=TRUE))
  var$actualgroups2 <- var$actualgroups2[,c(2,1)]
  var$group3 <- var$actualgroups2[,1]
  var$cond_id3 <- var$actualgroups2[,2]
  group3 <- as.data.frame(var$actualgroups2) 
  group3$V1 <- var$sampleid3
  var$select2 <- as.data.frame(group3$V2, row.names =  colnames(var$norData))
  colnames(var$select2) <- "group"
  var$groupList3 <-  # set the groups
    lapply(unique(group3$V2), function(x) {
      group3[group3$V2 == x, ]$V1
    })
  
  names(var$groupList3) <- unique(group3$V2)
  data.list3 <- rep(0, ncol(var$norData))
  convertion <- function(x, df) {
    grep(x, colnames(df))
  }
  for (i in 1:length(var$groupList3)) { # assign replicates to groups
    data.list3[unlist(lapply(var$groupList3[[i]], convertion, df = var$norData))] = names(var$groupList3[i])
  }
  var$selectedgroups <- data.list3
  
  var$sampleid4 <- colnames(var$newData)
  var$actualgroups3 <- do.call(rbind,strsplit(var$sampleid4,"_",fixed=TRUE))
  var$actualgroups3 <- var$actualgroups3[,c(2,1)]
  var$groupd2 <- as.data.frame(var$actualgroups3)
  group <- var$cond_id3
  var$select3 <- as.data.frame(group3$V2, row.names =  colnames(var$newData))
  colnames(var$select3) <-"group"
  var$design2 <- formula(as.formula(paste("~", paste(colnames(as.data.frame(group)), collapse = "+"))))
})


observeEvent(input$confirmed2groups, {
  progressSweetAlert(               # progress bar 
    session = session,
    id = "newDEAprogress",
    title = "Work in progress",
    display_pct = TRUE,
    value = 0
  )

  ######################################################################
  
  
  if(length(var$groupList3) == 2){
  
  if(input$DEAmethod == "tcc"){
    # Creation of a TCC Object 
    tcc <-                           
      new("TCC", var$newData, var$selectedgroups)
    var$tccObject <- tcc             # save the object
    
    
    updateProgressBar(               # updating progress bar
      session = session,
      id = "newDEAprogress",
      title = "Work in progress...",
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
      id = "newDEAprogress",
      title = "Work in progress...",
      value = 75
    )
    tcc <- estimateDE(tcc,        # final estimation of the DEGs 
                      test.method = input$testMethod,
                      FDR = input$fdr)
    
    
    var$tccObject <- tcc         # save the updated object 
    var$result <- getResult(tcc, sort = FALSE) %>% mutate_if(is.factor, as.character) # get the result of the calculation
    
      var$result_m <- var$result
      colnames(var$result_m) <- c("gene_id","BaseMean", "Log2FC","P-Value", "FDR", "Rank", "estimatedDEG")
      var$result_e <- var$result[which(var$result_m$estimatedDEG >0),] # selection of the DEGs
      var$result_s <- var$result_e[,-7]      # deleting the column showing which one is a DEG and which one is not
    
    var$norData <- tcc$getNormalizedData() # only the normalized data
    var$filter_genelist <- var$result_s[,1]
    var$DEAMETHOD <- 'tcc'
  }
  
  
  ######################################### deseq2 method #################################################
  
  
  if(input$DEAmethod == "DESeq2"){
    
    tcc <-                           
      new("TCC", var$newData, var$selectedgroups)
    var$tccObject <- tcc             # just to get the groups for pca 
    
    dds <- DESeqDataSetFromMatrix(countData=var$newData, colData=var$select3, design=var$design2)
    
    updateProgressBar(               # updating progress bar
      session = session,
      id = "newDEAprogress",
      title = "Work in progress...",
      value = 25
    )
    
    dds <- DESeq(dds)
    
    updateProgressBar(               # updating progress bar
      session = session,
      id = "newDEAprogress",
      title = "Work in progress...",
      value = 50
    )
    var$resultz <- results(dds)
    var$norData <- as.matrix(counts(dds, normalized = TRUE)) # normalization
    var$resultz <- as.matrix(var$resultz)
    var$result <- data.frame(var$resultz[,1], row.names = rownames(var$resultz))
    var$result['m.value'] <- var$resultz[,2]
    var$result['p.value'] <- var$resultz[,6]
    var$result['q.value'] <- p.adjust(var$resultz[,6], method = 'fdr')
    names(var$result)[1] <- "a.value"
    var$DESeq2DEGs <- var$result[which(var$result$q.value <= as.numeric(input$deseq2cutoff)),] 
    var$result["estimatedDEG"] = "0"
    var$result <- var$result[complete.cases(var$result), ]
    var$filter_genelist <- rownames(var$DESeq2DEGs)
    
    for (row in 1:nrow(var$result)){
      if(var$result[row,'q.value'] <= as.numeric(input$deseq2cutoff)){
        var$result[row, 'estimatedDEG'] = "1"
      }else{ 
        var$result[row,'estimatedDEG'] = "0"
      }
    }
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
      id = "newDEAprogress",
      title = "Work in progress...",
      value = 25
    )
    
    dgList <- calcNormFactors(dgList, method=input$edgeRMethod)
    
    updateProgressBar(               # updating progress bar
      session = session,
      id = "newDEAprogress",
      title = "Work in progress...",
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
    var$norData <- lrt$fitted.values
    var$result["estimatedDEG"] = "0"
    for (row in 1:nrow(var$result)){
      if(var$result[row,'q.value'] <= as.numeric(input$edgeRfdr)){
        var$result[row, 'estimatedDEG'] = "1"
      }else{ 
        var$result[row,'estimatedDEG'] = "0"
      }}
    var$edgeRDEGs <- var$result[which(var$result$q.value <= as.numeric(input$edgeRfdr)),] 
    var$edgeRDEGs <- var$edgeRDEGs[,-4]
    var$DEAMETHOD <- 'edgeR'
    var$filter_genelist <- var$edgeRDEGs[,1]
  }
    
    closeSweetAlert(session = session)       # close alert precising the calculation is done
    sendSweetAlert(session = session,
                   title = "DONE",
                   text = "Work was successfully performed.",
                   type = "success")
  }else{
    sendSweetAlert(
      session = session,
      title = "ERROR",
      text = "You must choose 2 groups only.",
      type = "info"
    )
    helpText("You must choose 2 groups only.")
  }
  
  FilterRun$FilterRunValue <- input$confirmed2groups  # precise the run button has been clicked 
})

resultTable <- reactive({   # saving the updated results to plot furtherly 
  var$result
})

output$filterTable <-  DT::renderDataTable({ 
data <- var$norData
    
  if(input$DEAmethod =="tcc"){
    gene_id <- row.names(data)
    data <- cbind(data, gene_id = gene_id)
    resultTable <- merge(var$result_m, data, by = "gene_id")
  }
  if(input$DEAmethod == "edgeR"){
    data <- as.data.frame(data)
    data['gene_id'] <- row.names(data)
    resultTable <- merge(var$result, data, by = "gene_id")
  }
  if(input$DEAmethod == "DESeq2"){
    data <- as.data.frame(data)
    resultTable <- merge(var$result, data, by="row.names")
    names(resultTable)[1] <-'gene_id'
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




output$filterTableDEG <- DT::renderDataTable({ 

    data <- var$norData
  
  if(input$DEAmethod == 'tcc'){
    gene_id <- row.names(data)
    data <- cbind(data, gene_id = gene_id)
    resultTable <- merge(var$result_s, data, by = "gene_id")
  }
  if(input$DEAmethod == "DESeq2"){
    resultTable <- merge(var$DESeq2DEGs, data, by = "row.names")
    names(resultTable[1]) <- 'gene_id'
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
  
  

