# server-data-import.R



observeEvent(input$uploadCountData, {   # when a table is being uploaded 
  tryCatch({
    var$InputTable <-    # assign the table as a data frame to this variable 
      data.frame(fread(input$uploadCountData$datapath), row.names = 1)
    var$control <- FALSE
    var$sampleid <- colnames(var$InputTable)
    var$groupdfs <- do.call(rbind,strsplit(var$sampleid,"_",fixed=TRUE))
    var$groupdfs <- var$groupdfs[,c(2,1)]
    var$rep <- var$groupdfs[,1]
    var$cond_id <- var$groupdfs[,2]
    var$matrixcount <- as.matrix(var$InputTable)
    var$CountData <- var$InputTable[,which(colnames(var$InputTable) == rownames(var$groupdfs))]

    
    
    updateSelectizeInput(session,"data_samples", 
                         choices=var$cond_id,selected=var$cond_id)
},
error = function(e) {  # error messages about the input table 
  sendSweetAlert(
    session = session,
    title = "Input data error!",
    text = as.character(message(e)),
    type = "error"
  )
  return()
},
warning = function(w) {
  sendSweetAlert(
    session = session,
    title = "Input data warning!",
    text = "Error in dataset",
    type = "warning"
  )
  return()
})

      
  observeEvent(input$filterCount,{   # when a filter of low count genes is set 
    if (input$filterCount != 0) {  # automatically filter the origianl table and update the summary
      var$LowCountGenes <- var$InputTable[rowSums(var$InputTable >= as.numeric(input$filterCount)) == 0,]
      var$CountData <- var$InputTable[rowSums(var$InputTable >= as.numeric(input$filterCount)) > 0 , ]
    }else{
      var$CountData <- var$InputTable
      var$LowCountGenes <- "No filtered data."
    }
    

  })

  group <- as.data.frame(var$groupdfs) 
  group$V1 <- var$sampleid
  var$groupdf <- as.data.frame(group$V2, row.names = var$sample_id)
  colnames(var$groupdf) <- "group"
  var$groupList <-  # set the groups
    lapply(unique(group$V2), function(x) {
      group[group$V2 == x, ]$V1
    })
  
  names(var$groupList) <- unique(group$V2)
  data.list <- rep(0, ncol(var$CountData))
  
  
  # Convert the input of group information to a specific format for normalization.
  convertion <- function(x, df) {
    grep(x, colnames(df))
  }
  
  for (i in 1:length(var$groupList)) { # assign replicates to groups
    data.list[unlist(lapply(var$groupList[[i]], convertion, df = var$CountData))] = names(var$groupList[i])
  }
  
observeEvent(input$data_samples,{
  if(!(input$data_samples[1]=="")) {
  var$selectedsamples <- input$data_samples
  tmprem = match(as.character(var$sampleid[which(!(var$cond_id%in%var$selectedsamples))]),colnames(var$CountData))
  tmpkeep = setdiff(1:ncol(var$CountData),tmprem)
  var$CountData = var$CountData[,tmpkeep]
  
  #################################################################
  var$sampleid2 <- colnames(var$CountData)
  var$actualgroups <- do.call(rbind,strsplit(var$sampleid2,"_",fixed=TRUE))
  var$actualgroups <- var$actualgroups[,c(2,1)]
  var$group2 <- var$actualgroups[,1]
  var$cond_id2 <- var$actualgroups[,2]
  group2 <- as.data.frame(var$actualgroups) 
  group2$V1 <- var$sampleid2
  var$select <- as.data.frame(group2$V2, row.names =  colnames(var$CountData))
  colnames(var$select) <- "group"
  group <- var$cond_id2
  var$design <- formula(as.formula(paste("~", paste(colnames(as.data.frame(group)), collapse = "+"))))
  var$groupList2 <-  # set the groups
    lapply(unique(group2$V2), function(x) {
      group2[group2$V2 == x, ]$V1
    })

  names(var$groupList2) <- unique(group2$V2)
  data.list2 <- rep(0, ncol(var$CountData))
  convertion <- function(x, df) {
    grep(x, colnames(df))
  }
  for (i in 1:length(var$groupList2)) { # assign replicates to groups
    data.list2[unlist(lapply(var$groupList2[[i]], convertion, df = var$CountData))] = names(var$groupList2[i])
  }
  
  var$selectedgroups <- data.list2
  

  ######################################################
 
}})

var$control <- TRUE

})
# save the updated table and associate a name to facilitate the use 
datasetInput <- reactive({
  var$CountData
})

output$DataSummary <- renderUI({  # summary render
  odf <- var$InputTable
  dt <- datasetInput()
  orowCount <- nrow(odf)
  if(input$filterCount == 0){ # filtered and raw genes count
    rowCount <- orowCount
    filtCount <- 0
  }else{
    rowCount <- nrow(dt)
    filtCount <- (orowCount - rowCount)
  }
  groupCount <- length(var$groupList2)       # groups count and setting 
  groupText <- sapply(var$groupList2, length)
  if (length(groupText) > 0) {
    gText <- paste0(names(groupText), ": ", groupText, ';', collapse = "\n")
  } else {
    gText <- NULL
  }
  
  tagList(     # set of info
    tipify(    # actual count
      tags$p(tags$b("N", tags$sub("genes")), ":", rowCount),
      title = "Number of Genes",
      placement = "left"
    ),
    tipify(    # raw initial count
      tags$p(tags$b("N", tags$sub(" input genes")), ":", orowCount),
      title = "Number of Input Genes",
      placement = "left"
    ),
    tipify(   # filtered genes count
      tags$p(tags$b("N", tags$sub("filtered genes")), ":", filtCount),
      title = "Number of Filtered Genes",
      placement = "left"
    ),
    tipify(  # number of groups
      tags$p(tags$b("N", tags$sub("group")), ": ", groupCount),
      title = " Number of Groups",
      placement = "left"
    ),
    tipify(   # replciated per groups
      tags$p(tags$b("N", tags$sub("replicates")), ": ", gText),
      title = " Number of Replicates",
      placement = "left"
    )
  )
})







# Render a table of raw count data

output$table <- DT::renderDataTable({
  df <- datasetInput()
  DT::datatable(
    df,
    colnames = c("Gene Name" = 1),
    extensions = c("Scroller", "RowReorder"),
    option = list(
      rowReorder = TRUE,
      deferRender = TRUE,
      scrollY = 400,
      scroller = TRUE,
      scrollX = TRUE,
      searchHighlight = TRUE, # search bar
      orderClasses = TRUE
    )
  )
})


# Render DataTable of row data count

output$showTable <- renderUI({
  if (nrow(datasetInput()) == 0) {  # if no uploaded table or empty, message
    tags$p("No data to show. Upload your dataset.")
  } else {    # if not, render the table
    DT::dataTableOutput('table')
  }
})
 ###### input 
output$inputable <- DT::renderDataTable({
  inputdf <- var$InputTable
  DT::datatable(
    inputdf,
    colnames = c("Gene Name" = 1),
    extensions = c("Scroller", "RowReorder"),
    option = list(
      rowReorder = TRUE,
      deferRender = TRUE,
      scrollY = 400,
      scroller = TRUE,
      scrollX = TRUE,
      searchHighlight = TRUE,
      orderClasses = TRUE
    )
  )
})


# Render DataTable of row data count

output$showInputTable <- renderUI({
  if (nrow(datasetInput()) == 0) {# if no uploaded table or empty, message
    tags$p("No data to show. Upload your dataset.")
  } else { # if not, render the table
    DT::dataTableOutput('inputable')
  }
})


output$filtable <- DT::renderDataTable({
  low <- var$LowCountGenes
  DT::datatable(
    low,
    colnames = c("Gene Name" = 1),
    extensions = c("Scroller", "RowReorder"),
    option = list(
      rowReorder = TRUE,
      deferRender = TRUE,
      scrollY = 400,
      scroller = TRUE,
      scrollX = TRUE,
      searchHighlight = TRUE,
      orderClasses = TRUE
    )
  )
})


# Render DataTable of row data count

output$showLowTable <- renderUI({ # if no uploaded table or empty, message
  if (is.data.frame(var$LowCountGenes) == FALSE) {
    tags$p("No Filtered data.")
  } else { # if not, render the table
    DT::dataTableOutput('filtable')
  }
})


v <- reactiveValues(importActionValue = FALSE)



################### BOXPLOT  #####################
output$CountDistribBox <- renderPlotly({
  if (length(var$CountData) > 0) {
    data <- as.matrix(var$CountData)    # set the data to use
    
    cpm <- log2(data + 1)   # counts 
    cpm_stack <- data.frame(stack(cpm))
    
    group <- data.frame("col" = rownames(var$select),  # with respect to groups
                 "group" = var$select$group)
    
    data <- left_join(cpm_stack, group, by = "col")  # to plot with respect to groups 
    data <- arrange(data, group)
    
    p <- plot_ly(  # plot 
      data,
      x = ~ col,
      y = ~ value,
      type = "box",
      split = ~ group,
      color = ~ group  # color with respect to groups
    ) %>% layout(
      title = input$CountDistribTitle,
      xaxis = list(title = input$CountDistribXlab, categoryarray = "array", categoryarray = ~col),
      yaxis = list(title = "log<sub>2</sub>(Count + 1)")
    )
    p
  } else {
    return()
  }
})




# render UI 
output$CountDistrib <- renderUI({
  if (var$control) {  # if data where imported and everything is ok then it can provides to plots
    tagList(fluidRow(
      column(
        3,
        textInput(   # set of parameters 
          inputId = "CountDistribTitle",
          label = "Title",
          value = "Raw Count",
          placeholder = "Raw Count"
        ),
        textInput(
          inputId = "CountDistribXlab",
          label = "X label",
          value = "Sample",
          placeholder = "Sample"
        )
      ),
      column(
        9,
        plotlyOutput("CountDistribBox", height = 800) %>% withSpinner()  # render 
      )
    ))
  } else {   # if no data then message 
    helpText("No data for ploting.")
  }
})

################### HEATMAP #####################
output$rawheatmap <- renderPlotly({
  if (length(var$CountData) > 0) {
    data <- var$CountData # data selection 
    data <- data.frame(1 - cor(data, method = input$correlation)) # with the chosen method of correlation 
    heatmaply( #heatmap
      data,
      hclust_method = "complete",
      labRow = rownames(data),
      labCol = colnames(data),
      colors = rev(RdYlGn(500))
    )
    
  }else {
    return()
  }
})

# Render UI 
output$clustUI <- renderUI({
  if (var$control) { # if data and no errors then run parameters and plot
    tagList(fluidRow(
      column( #parameter
        3,
        selectInput(
          inputId = "correlation",
          label = "Distance Measure",
          choices = c("Spearman" = "spearman",
                      "Pearson" = "pearson")
        ),
        tags$div(  # instruction
          HTML('<div class="panel panel-primary">
                    <div class="panel-heading"> <span style="padding-left:10px"><b> Distance measures </b> </span></div>
                  <div class="panel-body">
                  <style type="text/css">
                  .tg {
                  border-collapse: collapse;
                  border-spacing: 0;
                  border: none;
                  }
                  .tg td {
                  font-family: Arial, sans-serif;
                  font-size: 14px;
                  padding: 10px 5px;
                  border-style: solid;
                  border-width: 0px;
                  overflow: hidden;
                  word-break: normal;
                  }
                  .tg .tg-s6z2 {
                  text-align: center
                  }
                  </style>
                  <table class="tg">
                  <tr>
                  <th class="tg-031e"> <span class="label label-primary"> Spearman </span></th>
                  <th class="tg-031e"> Spearman distance is a square of Euclidean distance between two rank vectors.
                  </tr>
                  <tr>
                  <th class="tg-031e"> <span class="label label-primary"> Pearson</span></th>
                  <th class="tg-031e"> Pearson correlation measures the degree of a linear relationship between two profiles.
                  </tr>
                  </table>
                  </div>
                  </div>'))
      ),
      column(9, plotlyOutput("rawheatmap",height = 600, width = 800) %>% withSpinner() # render heatmap
      )
    ))
  } else { # if no data, then message 
    helpText("No data for ploting.")
  }
})

################### PCA #####################

# 2D Plot 
output$pcaPlotObject2d <- renderPlotly({
  if (length(var$CountData) > 0) {
    data <- as.matrix(var$CountData)
    data <- log1p(data) # data selection 
    data <- data[apply(data, 1, var) != 0, ] # selection over counts 
    data <- t(data[order(apply(data, 1, var), decreasing = TRUE)[1:100], ])
    data.pca.all <- prcomp(data,center = T, scale. = T) #pca 
    data <- data.frame(data.pca.all$x)
    data$name <- rownames(data)
    group <- var$select
    group$name <- rownames(var$select)
    data <- left_join(x = data, y = group, by = "name") # to perform over groups 
    p <- plot_ly(  # plot 
      data = data,
      x = ~ PC1,
      y = ~ PC2,
      color = ~ factor(group),
      text = ~ name,
      textposition = "top right",
      type = "scatter",
      mode = "markers+text"
    ) %>%
      layout(title = "PCA 2D Plot")
    p
  } else {
    return(0)
  }
})

# 3D Plot
output$pcaPlotObject3d <- renderPlotly({
  if (length(var$CountData) > 0) {
    data <- as.matrix(var$CountData)
    data <- log1p(data) # data selection 
    data <- t(data[apply(data, 1, var) != 0, ]) # selection over counts
    data.pca.all <- prcomp(data,center = T,scale. = T) # pca
    
    data <- data.frame(data.pca.all$x)
    data$name <- rownames(data)
    group <- var$select
    group$name <- rownames(var$select)
    data <- left_join(x = data, y = group, by = "name") # to perform the pca over groups
    p <- plot_ly(   #plot
      data = data,
      x = ~ PC1,
      y = ~ PC2,
      z = ~ PC3,
      color = ~ factor(group),
      text = ~ name,
      textposition = "top right",
      type = "scatter3d",
      mode = "markers+text"
    ) %>%
      layout(title = "PCA 3D Plot")
    p
  } else {
    return(0)
  }
})


# render pca
output$pcaUI <- renderUI({
  if (var$control) {
             tabsetPanel(  # render plots 
               tabPanel(title = "2D Plot", plotlyOutput("pcaPlotObject2d", width = 1200, height = 600) %>% withSpinner()),
               tabPanel(title = "3D Plot", plotlyOutput("pcaPlotObject3d", width = 1200, height = 600) %>% withSpinner())
             )
  } else { # if no data, message
    helpText("No data for ploting.")
  }
})
