# server-heatmap.R

runHeatmap <- reactiveValues(runHeatmapValue = FALSE, height = 300) # to precise the run button has not been clicked

output$CondHeatmapParams <- renderUI({
  if (AnalysisRun$AnalysisRunValue){           # if a DEA analysis has been performed then show the parameters 
    uiOutput("HeatParams")
  }else{                                       # if not error message to do it 
    sendSweetAlert(
      session = session,
      title = "ERROR",
      text = "You must perform a DEA before.",
      type = "info"
    )
    helpText("Please perform a DEA first.")
  }
})


output$HeatParams <- renderUI({               # set of parameters 
  tagList(
    radioGroupButtons(
      inputId = "heatmapGeneSelectType",
      label = "Select Genes",
      choices = c("By List" = "By list",
                  "By FDR" = "By FDR"),
      justified = TRUE,
      status = "primary"
    ),
    uiOutput("heatmapSelectGene"),
    selectInput(
      "heatmapDist",
      "Distance Measure",
      choices = list(
        "Euclidean" = "euclidean",
        "Maximum" = "maximum",
        "Manhattan" = "manhattan",
        "Canberra" = "canberra",
        "Binary" = "binary",
        "Minkowski" = "minkowski"
      ),
      selected = "euclidean"
    ),
    selectInput(
      "heatmapCluster",
      "Agglomeration Method",
      choices = list(
        "ward.D2" = "ward.D2",
        "Single" = "single",
        "Complete" = "complete",
        "UPGMA" = "average"
      ),
      selected = "complete"
    ),
    numericInput(
      inputId = "clusterswanted",
      label = "Clusters Wanted",
      min = 1,
      value = 5,
      max = 100,
      step = 1
    ),
    textInput(
      inputId = "heatX",
      label = "X label",
      value = "Genes",
      placeholder = "Genes"
    ),
    textInput(
      inputId = "heatY",
      label = "Y label",
      value = "Conditions",
      placeholder = "Conditions"
    ),
    sliderInput(
      inputId = "heatheight",
      label = "Plot Height",
      value = 800,
      min = 400,
      max = 1600, 
      step = 50
    ),
    tagList(
      selectInput(
        "heatmapColor",
        "Choose Colormap",
        choices = list(
          "PiYG",
          "PRGn",
          "BrBG",
          "PuOr",
          "OrRd",
          "Oranges",
          "RdGy",
          "RdBu",
          "RdYlBu",
          "RdYlGn",
          "Spectral",
          "coolwarm"
        ),
        selected = "RdYlGn"
      )
    ),
    tags$b("Color Preview"),
    plotOutput("colorPreview", height = "20px"),
    do.call(actionBttn, c(
      list(
        inputId = "heatmapRun",
        label = "Run Heatmap",
        icon = icon("play")
      )
    )))
})


output$heatmapSelectGene <- renderUI({
  switch(
    input$heatmapGeneSelectType,
    "By list" = tagList(
      textAreaInput(
      "heatmapTextList",
      "Paste Gene List",
      rows = 5,
      placeholder = "Input gene's name (first column in the dataset), one gene per line."
    ),
    textInput(
      inputId = "heattitle",
      label = "Plot Title",
      value = "Title",
      placeholder = "Heatmap title"
    )
    ),
    "By FDR" =
        numericInput(
          inputId = "heatmapFDR",
          label = "FDR Cut-off",
          min = 0.00001,
          value = 0.001,
          max = 0.01,
          step = 0.0001
        )
      )
})


colorPanel <- reactive({  # Color palette
  colorPal <- c("white")
  if (length(input$heatmapColor) > 0) {
    colorPal <- switch(
      input$heatmapColor,
      "PiYG" = PiYG(20),
      "PRGn" = PRGn(20),
      "BrBG" = BrBG(20),
      "PuOr" = PuOr(20),
      "OrRd" = OrRd(20),
      "Oranges" = Oranges(20),
      "RdGy" = RdGy(20),
      "RdBu" = RdBu(20),
      "RdYlBu" = RdYlBu(20),
      "RdYlGn" = RdYlGn(20),
      "Spectral" = Spectral(20),
      "coolwarm" = cool_warm(20)
    )
  }
  colorPal
})

#palette preview
output$colorPreview <- renderPlot({
  colorPal <- colorPanel()
  op <- par(mar = c(0.5, 0, 0, 0))
  plot(
    c(0, length(colorPal)),
    c(0, 1),
    type = "n",
    xlab = "",
    ylab = "",
    ann = F,
    bty = "n",
    xaxt = "n",
    yaxt = "n"
  )
  i <- 0:(length(colorPal) - 1)
  rect(0 + i, 0, 1 + i, 1, col = colorPal, lwd = 0)
  par(op)
})


# heatmaply object
observeEvent(input$heatmapRun, {
  data.list <- var$selectedgroups # seelcting only the selected groups
  data <- var$norData             # the normalized data 
  data.list <- data.list[data.list != 0]
  if (input$heatmapGeneSelectType == "By list") {  # if gene selection is by list, find these genes in the global list of gene from the input table
    selectedListForHeatmap <-
      row.names(data) %in% unlist(strsplit(x = input$heatmapTextList, split = '[\r\n]'))
    heatmapTitle <- input$heattitle
  }
  
  if (input$heatmapGeneSelectType == "By FDR") { # if gene selection is by fdr cut off 
    
    selectedListForHeatmap <-     # it justs select the genes respecting the cut off
      row.names(data) %in% resultTable()[resultTable()$q.value <= input$heatmapFDR,]$gene_id
    heatmapTitle <-
      paste0("Heatmap of gene expression : q.value < ",  # title of the heatmap accoring to the fdr cut off
             input$heatmapFDR,
             ", ",
             sum(selectedListForHeatmap),
             "DEGs (",
             input$heatmapDist, " and ", input$heatmapCluster, ")"
             )
  }
  
  data <- data[selectedListForHeatmap, ]  # updating selected data
  
  
  if (nrow(data) == 0) {    # error is gene list is selected and is empty
    sendSweetAlert(
      session = session,
      title = "ERROR",
      text = "Genes list is empty!",
      type = "error"
    )
    return()
  } else {               # shows the number of DEGs used according to the selection and the samples (groups)
    showNotification(paste0(dim(data)[1], " DEGs, ", dim(data)[2], " sample will be used."))
    showNotification("Generating, please be patient...", type = "message")
  }
  colorPal <- colorPanel()
  datat <- t(data) # transform the matrix to have genes as column and groups as row
  datal <-  log1p(datat)
  datan <- heatmaply::normalize(datal)
  dend <- hclust(dist(t(datan), method = input$heatmapDist), method = input$heatmapCluster) # to assign the cluster
  cut <- cutree(dend, k = input$clusterswanted) # with the number of wanted clusters
  cute <- as.data.frame(cut)   # and finally to accord it as a dataframe to reveal it
  cute$gene_id <- rownames(cute)
  colnames(cute) <- c("cluster","gene_id")
  rownames(cute) <- NULL
  output$heatmap <- renderPlotly({    # heatmap
    
    p <- heatmaply(
      datan,
      colors = colorPal,
      k_col = input$clusterswanted,# clusters wanted
      dist_method = input$heatmapDist, # distance method
      hclust_method = input$heatmapCluster, # agglomeration method
      xlab = input$heatX,
      ylab = input$heatY,
      main = heatmapTitle,    # title
      margins = c(150, 100, 40, 20),
      dendrogram = "column",   # add a dendrogram on the columns (genes)
      labCol = colnames(datan),
      labRow = row.names(datan)
    )
    p
  })
  
  
  gene_id <- row.names(data)
  data <- cbind(data, gene_id = gene_id)
  heatdata <- var$result        # select only gene id, pvalue fdr and rank for the result table
  heatdata <- var$result[,-2]   #
  heatdata <- heatdata[,-2]     #
  heatdata <- heatdata[,-5]     #
  resultTable <- merge(cute, heatdata, by = "gene_id")
  resultTable <- merge(resultTable, data, by = "gene_id")
  resultTable <- resultTable[order(resultTable[,2]),]
  rownames(resultTable) <- resultTable[,1]
  resultTable <- resultTable[,-1]
  
  
  #result table 
  output$resultTableInHeatmap <- DT::renderDataTable({
    
    DT::datatable(
      resultTable,        
      extensions = 'Buttons',      # download button 
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
                         filename = "result_heatmap"),
          text = 'Download')),
        scrollX = TRUE,
        pageLength = 10,
        searchHighlight = TRUE,      #search bar
        orderClasses = TRUE
        
      ),
      
      class = "display")
  }, server = FALSE)
  
  
  
  runHeatmap$runHeatmapValue <- input$heatmapRun #precise the run button has been clicked and heatmap performed
  closeSweetAlert(session = session)
  sendSweetAlert(session = session,
                 title = "Completed!",
                 type = "success")
  
  
  
})


# remder final heatmap 

output$heatmapPlot <- renderUI({
  if (runHeatmap$runHeatmapValue) {   # if the run button  has been clicked, then it shows the heatmap
    plotlyOutput("heatmap", height = input$heatheight) %>% withSpinner()
  }
  else{                               # if not, message to do it
    helpText("Enter parameters to plot the heatmap first.")
  }
})


