# server-volcano-plot.R

runVolcano <- reactiveValues(runVolcanoValue = FALSE) # to precise the run button has not been clicked 


output$CondvolcanoParams <- renderUI({ # if a DEA has been performed then shows the parameters 
  if (AnalysisRun$AnalysisRunValue){
    uiOutput("volcanoParams")
  }else{     # if not error to do it
    sendSweetAlert(
      session = session,
      title = "ERROR",
      text = "You must perform a DEA before.",
      type = "info"
    )
    helpText("Please perform a DEA first.")
  }
  
  
})

output$volcanoParams <- renderUI({   # parameters
  tagList(
    sliderInput(
      "volcanoPointSize",
      "Point Size",
      min = 1,
      max = 5,
      value = 3,
      step = 0.2
    ),
    sliderInput(
      "CutFC",
      "Fold Change (X-axis) Cut-off",
      min = ceiling(min(resultTable()$m.value)),
      max = floor(max(resultTable()$m.value)),
      value = c(-2, 2),
      step = 0.5
    ),
    numericInput(
      inputId = "Cutfdr",
      label = "FDR Cut-off",
      min = 0.00001,
      value = 0.01,
      max = 0.01,
      step = 0.0001
    ),
    numericInput(
      inputId = "volcheight",
      label = "Plot Height",
      value = 800,
      min = 400,
      max = 1600, 
      step = 50
    ),
    spectrumInput(
      inputId = "downColor",
      label = tagList("Down-regulated" ,htmlOutput("downPreview")),
      choices = list(
        list(
          "red",
          'black',
          'white',
          'blanchedalmond',
          'steelblue',
          'forestgreen'
        ),
        as.list(brewer.pal(n = 9, name = "Oranges")),
        as.list(brewer.pal(n = 9, name = "Reds")),
        as.list(brewer.pal(n = 11, name = "Spectral"))
      ),
      options = list(`toggle-palette-more-text` = "Show more")
    ),
    spectrumInput(
      inputId = "upColor",
      label = tagList("Up-regulated" ,htmlOutput("upPreview")),
      choices = list(
        list(
          "green",
          'black',
          'white',
          'blanchedalmond',
          'steelblue',
          'forestgreen'
        ),
        as.list(brewer.pal(n = 9, name = "Blues")),
        as.list(brewer.pal(n = 9, name = "Greens")),
        as.list(brewer.pal(n = 11, name = "Spectral"))
      ),
      options = list(`toggle-palette-more-text` = "Show more")
      
    ),
    do.call(actionBttn, c(
      list(
        inputId = "makeVolcanoPlot",
        label = "Generate Volcano Plot",
        icon = icon("play")
      )
    ))
  )
})

# Preview up and down regulated genes under Color selection 
observeEvent({
  input$CutFC
  input$Cutfdr
}, {
  dt <- resultTable()
  downCut <- input$CutFC[1]
  upCut <- input$CutFC[2]
  FDRCut <- input$Cutfdr
  dtDEG <- dt[which(dt$estimatedDEG >0),]
  
  downCount <-
    nrow(dtDEG[dtDEG$m.value <= downCut & dtDEG[["q.value"]] <= FDRCut,])
  upCount <-
    nrow(dtDEG[dtDEG$m.value >= upCut & dtDEG[["q.value"]] <= FDRCut,])
  
  output$downPreview <- renderText({ # render for downregulated count
    paste0("<font color=\"",
           input$downColor,
           "\"><b>",
           downCount,
           " genes</b></font>")
  })
  output$upPreview <- renderText({ # render for upregulated count
    paste0("<font color=\"",
           input$upColor,
           "\"><b>",
           upCount,
           " genes</b></font>")
  })
  
})

# when the botton has been clicked, generates volcano plot 
# just like the ma plot, there is a bar plot connected to the volcano plot with raw counts
observeEvent(input$makeVolcanoPlot, {
  yaxis <- "q.value"
  output$volcanoPloty <- renderPlotly({
    validate(need(resultTable()[[yaxis]] != "", "No data for ploting.")) # validation 
    req(input$makeVolcanoPlot)
    isolate({
      dt <- resultTable() # shortcut to the data
      downCut <- input$CutFC[1] # cuts of Log2FC
      upCut <- input$CutFC[2]
      dt$color <- "None"
      
      tryCatch({   # catch cutoffs 
        dt[dt$m.value <= downCut,]$color <- "Down"
        dt[dt$m.value >= upCut,]$color <- "Up"   
        dt[dt[[yaxis]] > input$Cutfdr,]$color <- "None"
      }, error = function(e) {
        sendSweetAlert(session = session, title = "ERROR", text = "No data was satisfied to your cut-off!")
      })
      
      FCcut <- factor(dt$color) # conversion as a factor to use it in the plot
      
      # link to bar plot
      if(var$DEAMETHOD == 'deseq2'){
        key <- row.names(resultTable())
      }else{
      key <- resultTable()$gene_id
      }
      p <- plot_ly( # plot
        dt,  # data
        x = ~ m.value,
        y = ~ -log10(dt[[yaxis]]),
        type = "scatter",
        mode = "markers",
        color = ~ FCcut,  # color according to cut ofss
        colors = c(input$downColor, "black", input$upColor),
        marker = list(size = input$volcanoPointSize),
        hoverinfo = "text",  # when hover over a point, the following info shows
        text = ~ paste(
          "</br>Gene:",
          resultTable()$gene_id,
          "</br>Log2FC:",
          m.value,
          "</br>p-value:",
          p.value,
          "</br>q-value:",
          q.value
        ),
        key =  ~ key,
        source = "volcano"
      ) %>%
        layout(
          xaxis = list(title = "log<sub>2</sub>(Fold Change)"),
          yaxis = list(title = "-log<sub>10</sub>(FDR)"),
          title = paste0(
            "Volcano Plot with q-value < ",
            input$Cutfdr
          ),
          legend = list(
            orientation = 'h',
            xanchor = "center",
            x = 0.5,
            y = 1.05
          ),
          shapes = list(   # lines of cut offs 
            list(
              type = 'line', #up log2fc cutoff
              y0 =  ~ min(-log10(dt[[yaxis]])),
              y1 =  ~ max(-log10(dt[[yaxis]])),
              x0 = upCut,
              x1 = upCut,
              line = list(dash = 'dot', width = 2)
            ),
            list(
              type = 'line', # down log2fc cut off
              y0 =  ~ min(-log10(dt[[yaxis]])),
              y1 =  ~ max(-log10(dt[[yaxis]])),
              x0 = downCut,
              x1 = downCut,
              line = list(dash = 'dot', width = 2)
            ),
            list(
              type = 'line',  # fdr cut off
              y0 = -log10(input$Cutfdr),
              y1 = -log10(input$Cutfdr),
              x0 =  ~ min(m.value),
              x1 =  ~ max(m.value),
              line = list(dash = 'dot', width = 2)
            )
          )
        )
      p
    })
  })
  runVolcano$runVolcanoValue <- input$makeVolcanoPlot # validation run button has been clicked
})


# render barplot next to volcanoplot

output$VolcanoBarPlot <- renderPlotly({
  eventdata <- event_data("plotly_hover", source = "volcano")
  validate(need(
    !is.null(eventdata),
    "Hover over the point to show gene's expression level of interest."
  ))
  
  gene_id <- eventdata$key
  expression <- # counts 
    var$newData[row.names(var$newData) == gene_id,]
  data <- var$norData
  dataGroups <- var$selectedgroups # according to selected groups
  expression <- t(expression[dataGroups != 0])
  
  xOrder <-
    data.frame("name" = row.names(expression), "group" = dataGroups)
  xOrderVector <- unique(xOrder[order(xOrder$group),]$name)
  xform <- list(categoryorder = "array",
                categoryarray = xOrderVector,
                title = "")
  
  plot_ly( # plot
    x = ~ row.names(expression),
    y = ~ expression[, 1],
    color = as.factor(dataGroups),
    text = expression[, 1],
    textposition = 'outside',
    showlegend = FALSE,
    type = "bar",
    name = "Raw"
  ) %>%
    layout(
      xaxis = xform,
      yaxis = list(title = "Normalized Count"),
      title = colnames(expression)
    )
})


# Render table result

output$resultTableVolc <- DT::renderDataTable({ # full result table 
  method <- var$DEAMETHOD
  if (nrow(resultTable()) == 0) {
    DT::datatable(resultTable())
  } else {
      fdrCut <- input$Cutfdr
      volcT <- resultTable() # using of the  result table
      volcT <- volcT[which(volcT$estimatedDEG >0),] # selection of the DEGs only 
      data <- var$norData # using normalized data
      gene_id <- row.names(data)
      data <- cbind(data, gene_id = gene_id)
      
    if(method == "tcc"){
      volcT <- volcT[,-7] # deleting the column with 0 and 1 saying the gene is a DEG or not
      resultTable <- merge(volcT, data, by = "gene_id")
    }else{
      volcT <- volcT
      resultTable <- volcT
      
    }

    
    t <- DT::datatable(
      resultTable,
      filter = "bottom",
      extensions = 'Buttons',   # download button 
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
                         filename = "volcano_results"),
          text = 'Download')),
        scrollX = TRUE,
        pageLength = 10,
        searchHighlight = TRUE,  # search bar 
        orderClasses = TRUE
        
      ),
      class = "display",
      caption = tags$caption(
        tags$li(
          "Gene Name was colored according to Fold Change and set bold according to P-value cut-off."
        )
      ))
    
    if (!is.na(sum(volcT$m.value))) {   # coloring results according to cut offs 
      t   %>% formatStyle("gene_id", "m.value",
                          color = styleInterval(input$CutFC,
                                                c(
                                                  input$downColor, "black", input$upColor
                                                ))) %>% formatStyle("gene_id",
                                                                    "p.value",
                                                                    fontWeight = styleInterval(fdrCut, c("bold", "normal")))
    } else {
      t
    }
  }
},server = F)


# final render 
output$MainResultTableVolc <- renderUI({
  if(runVolcano$runVolcanoValue){ # if the button has been clicked, it shows the result table
    tagList(fluidRow(column(
      12, DT::dataTableOutput('resultTableVolc') %>% withSpinner()
    )))} else {
      helpText("Run Volcano to obtain Result Table.") # if not, message to do it 
    }
})

############################################################DOWN REGULATED##################################

output$resultTabledown <- DT::renderDataTable({ # datatable render for downregulated genes
  sortedvolc <- var$result
  method <- var$DEAMETHOD
  data <- var$norData
  gene_id <- row.names(data)
  data <- cbind(data, gene_id = gene_id)
  if (nrow(sortedvolc) == 0) {
    DT::datatable(sortedvolc)
  } else {
      downCut <- input$CutFC[1]
      upCut <- input$CutFC[2]
      fdrCut <- input$Cutfdr
      sortedvolc <- sortedvolc[sortedvolc$q.value < fdrCut,]
      sortedvolc <- sortedvolc[sortedvolc$m.value < downCut,]
      sortedvolc <- sortedvolc[which(sortedvolc$estimatedDEG >0),]
      if(method == 'tcc'){
        sortedvolc <- sortedvolc[,-2]
        sortedvolc <- sortedvolc[,-6]
        downresultTable <- merge(sortedvolc, data, by = "gene_id")
      }else{
        if(method == "edgeR"){
          sortedvolc <- sortedvolc[,-2]
          sortedvolc <- sortedvolc[,-4]
        }}

    downresultTable <- sortedvolc

   
    
    t <- DT::datatable(
      downresultTable,
      filter = "bottom",
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
                         filename = "downregulated_genes"),
          text = 'Download')),
        scrollX = TRUE,
        pageLength = 10,
        searchHighlight = TRUE,
        orderClasses = TRUE
        
      ),
      
      class = "display",
      caption = tags$caption(
        tags$li(
          "Please verify your Log2FC if changed from the default one."
        )
      ))
    
    if (!is.na(sum(sortedvolc$m.value))) {
      t   %>% formatStyle("gene_id", "m.value",
                          color = styleInterval(input$CutFC,
                                                c(
                                                  input$downColor, "black", input$upColor
                                                ))) %>% formatStyle("gene_id",
                                                                    "p.value",
                                                                    fontWeight = styleInterval(fdrCut, c("bold", "normal")))
    } else {
      t
    }
  }
},server = F)


##########################################################UPREGULATED######################################

output$resultTableup <- DT::renderDataTable({ #datatable render for downregulated genes
  sortedvolc <- var$result
  method <- var$DEAMETHOD
  data <- var$norData
  gene_id <- row.names(data)
  data <- cbind(data, gene_id = gene_id)
  if (nrow(sortedvolc) == 0) {
    DT::datatable(sortedvolc)
  } else {
      downCut <- input$CutFC[1]
      upCut <- input$CutFC[2]
      fdrCut <- input$Cutfdr
      sortedvolc <- sortedvolc[sortedvolc$q.value < fdrCut,]
      sortedvolc <- sortedvolc[sortedvolc$m.value > upCut,]
      sortedvolc <- sortedvolc[which(sortedvolc$estimatedDEG >0),]
      if(method == 'tcc'){
        sortedvolc <- sortedvolc[,-2]
        sortedvolc <- sortedvolc[,-6]
        downresultTable <- merge(sortedvolc, data, by = "gene_id")
      }else{
        if(method == "edgeR"){
          sortedvolc <- sortedvolc[,-2]
          sortedvolc <- sortedvolc[,-4]
        }}

    upresultTable <- sortedvolc
    
    
    
    t <- DT::datatable(
      upresultTable,
      filter = "bottom",
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
                         filename = "upregulated_genes"),
          text = 'Download')),
        scrollX = TRUE,
        pageLength = 10,
        searchHighlight = TRUE,
        orderClasses = TRUE
        
      ),
      
      class = "display",
      caption = tags$caption(
        tags$li(
          "Please verify your Log2FC if changed from the default one."
        )))
    
    if (!is.na(sum(sortedvolc$m.value))) {
      t   %>% formatStyle("gene_id", "m.value",
                          color = styleInterval(input$CutFC,
                                                c(
                                                  input$downColor, "black", input$upColor
                                                ))) %>% formatStyle("gene_id",
                                                                    "p.value",
                                                                    fontWeight = styleInterval(fdrCut, c("bold", "normal")))
    } else {
      t
    }
  }
},server = F)

# volcanoUI 
output$volcanoUI <- renderUI({
  if (length(var$groupList2) || length(var$groupList3) == 2)  {
    
    if(runVolcano$runVolcanoValue){ # if run button clicked, then shows plots 
      tagList(
        fluidRow(
          column(8, plotlyOutput("volcanoPloty", height = input$volcheight) %>% withSpinner()),
          column(4, plotlyOutput("VolcanoBarPlot", height = input$volcheight) %>% withSpinner())
        )
      )
    } else {
      helpText("Please click [Generate Volcano Plot] first.")
  }}else{  # volcano plot is available for onyl 2 groups comparisons, if more, then error 
    sendSweetAlert(
      session = session,
      title = "ERROR",
      text = "Volcano Plot is unavailable for multiple comparison now.",
      type = "info"
    )
    helpText("Volcano Plot is unavailable for multiple comparison now.")}
  
})