# server-pca.R

runPCA <- reactiveValues(runPCAValue = FALSE) # to precise the run button has not been clicked yet


output$CondPCAParams <- renderUI({              # if a DEA has not been performed error message to perform it
  if (AnalysisRun$AnalysisRunValue){            # if DEA done, it shows the parameters
    uiOutput("PCAParams")
  }else{
    sendSweetAlert(
      session = session,
      title = "ERROR",
      text = "You must perform a DEA before.",
      type = "info"
    )
    helpText("Please perform a DEA first.")
  }
  
  
})

output$PCAParams <- renderUI({                # set of paramters 
  tagList(
    tipify(
    numericInput(
      inputId = "pcFDR",
      label = "FDR Cut-off",
      min = 0.00001,
      value = 0.001,
      max = 0.01,
      step = 0.001
    ),
    title = "The fdr cut off can be lowered from the analysis in the pca",
    placement = "bottom"
  ),
  
  tipify(
    numericInput(
      inputId = "firstTopGenes",
      label = "Top Genes Selection",
      value = 100,
      min = -1,
      step = 1
    ),
    title = "Leave it empty to use all the genes in the table, or choose yourfirst top genes to plot the pca",
    placement = "bottom"
    ),
  helpText(HTML(" To plot the PCA, data are tranformed to log and one can choose to represent all genes or just the chosen top genes")),
    do.call(actionBttn, c(                 # run button 
      list(
        inputId = "pcRun",
        label = "Run PCA",
        icon = icon("play")
      )
    ))
  )
})


observeEvent(input$pcRun, {              # when the run button is clicked
  runPCA$runPCAValue <- input$pcRun      # precise the button has been clicked
  data <- var$norData                    # use normalized data
  data <- data[var$result$q.value <= input$pcFDR,]  # selection of genes with respect of the selected fdr cut-off
  data <- log1p(data)
  if (!is.na(input$firstTopGenes) & input$firstTopGenes < nrow(data)) {
    data <- t(data[order(apply(data, 1, var), decreasing = TRUE)[1:input$firstTopGenes], ])
  }
  else {
    data <- t(data)
  }
  data.pca <- prcomp(data[, apply(data, 2, var) != 0], #pca
                     center = T,
                     scale. = T)
  var$pcadata <- data.pca               # save the pca to reuse the data
})


# 2D plotly object
output$D2pca <- renderPlotly({          
  if (length(var$pcadata) > 0) {
    data.pca <- var$pcadata
    data <- data.frame(data.pca$x)
    data$name <- rownames(data)
    group <- var$select
    group$name <- rownames(var$select)
    data <- left_join(x = data, y = group, by = "name") # to perform a pca over the groups 
    p <- plot_ly(
      data = data,
      x = ~ PC1,
      y = ~ PC2,
      color = ~ factor(group),          # colors according to groups
      text = ~ name,                    # hovering a point on the plot gives the name of the group
      textposition = "top right",
      type = "scatter",
      mode = "markers+text"
    ) %>%
      layout(title = "PCA Plot (2D)")
    p
  } else {
    return()
  }
})

#  3D plotly object       
output$D3pca <- renderPlotly({       # same in 3D 
  if (length(var$pcadata) > 0) {
    data.pca <- var$pcadata
    data <- data.frame(data.pca$x)
    data$name <- rownames(data)
    group <- var$select
    group$name <- rownames(var$select)
    data <- left_join(x = data, y = group, by = "name")
    p <- plot_ly(
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
      layout(title = "PCA Plot (3D)")
    p
  } else {
    return()
  }
})


output$normheatmap <- renderPlotly({
  if (length(var$norData) > 0) {
    data_norm <- data.frame(1 - cor(var$norData, method = input$correlation)) # with the chosen method of correlation 
    heatmaply( #heatmap
      data_norm,
      hclust_method = "complete",
      labRow = rownames(data_norm),
      labCol = colnames(data_norm),
      colors = rev(RdYlGn(500))
    )
    
  }else {
    return()
  }
})

# Render 2D Plot UI 
output$D2PlotUI <- renderUI({
  if (runPCA$runPCAValue) {                    # if the pca run button has been clicked 
    plotlyOutput("D2pca", width = 1200, height = 600) %>% withSpinner()    # it shows the 2D pca
  } else {                                     # if not, it just precise to run it 
    helpText("Click [Run PCA] first.")
  }
})

# Render 3D Plot UI 
output$D3PlotUI <- renderUI({
  if (runPCA$runPCAValue) {
    plotlyOutput("D3pca", width = 1200, height = 600) %>% withSpinner()
  } else {
    helpText("Click [Run PCA] first.")
  }
})

output$normclust <- renderUI({
  if(AnalysisRun$AnalysisRunValue) { # if data and no errors then run parameters and plot
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
      column(9, plotlyOutput("normheatmap",height = 600, width = 800) %>% withSpinner() # render heatmap
      )
    ))
  } else { # if no data, then message 
    helpText("Run normalization and DEA before.")
  }
})

