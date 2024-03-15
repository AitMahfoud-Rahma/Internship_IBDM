# server-ma-plot.R

runMA <- reactiveValues(runMAValues = FALSE) # to precise the run button has not been clicked yet


output$CondMAPlotParams <- renderUI({
  if (AnalysisRun$AnalysisRunValue){  # if a DEA has been performed, then show parameters
    uiOutput("MAPlotParams")
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


    # parameters
  output$MAPlotParams <- renderUI({
    tagList(
      sliderInput(
        "pointSize",
        "Point Size",
        min = 1,
        max = 5,
        value = 3,
        step = 0.2
      ),
      numericInput(
        inputId = "maFDR",
        label = "FDR Cut-off",
        min = 0.00001,
        value = input$fdr,
        max = 0.01,
        step = 0.0001
      ),
      spectrumInput(
        inputId = "fdrColor",
        label = tagList("DEGs Color", htmlOutput("maFDRpreview")),
        choices = list(
          list(
            "#B22222",
            'black',
            'white',
            'blanchedalmond',
            'steelblue',
            'forestgreen'
          ),
          as.list(brewer.pal(n = 9, name = "Reds")),
          as.list(brewer.pal(n = 9, name = "Greens")),
          as.list(brewer.pal(n = 11, name = "Spectral")),
          as.list(brewer.pal(n = 8, name = "Dark2"))
        ),
        options = list(`toggle-palette-more-text` = "Show more")
      ),
      do.call(actionBttn,c(   # run button 
        list(
          inputId = "makeMAPlot",
          label = "Generate MA Plot",
          icon = icon("play")
        ))
      )
    )
  })




observeEvent(input$makeMAPlot, { # if run button has been clicked 
  output$maplotly <- renderPlotly({ # plotting the ma plot
    validate(need(resultTable()$a.value != "", "No MA values for ploting.")) # if basemean values allow it 
    
    isolate({
      if(var$DEAMETHOD == 'deseq2'){
        key <- row.names(resultTable())
      }else{
        key <- resultTable()$gene_id
      }
      DEGcut <- cut(resultTable()$q.value, breaks = c(0, input$maFDR, 1)) # coloration of the ma plot according to the cut off
      
      p <- plot_ly( # ma plot Log2FC with repect ot Basemean 
        data = resultTable(),
        x = ~ a.value, # basemean
        y = ~ m.value, # log2fc
        type = "scatter",
        mode = "markers",
        color = ~ DEGcut, # color with respect to cut off
        colors = c(input$fdrColor, "#000000"),
        marker = list(size = input$pointSize), # selected size of the points
        hoverinfo = "text+name", # hover infos 
        text = ~ paste(
          "</br>Gene:",
          resultTable()$gene_id,
          "</br>BaseMean (A value):",
          round(a.value, 4),
          "</br>Log2FC (M value):",
          round(m.value, 4)
        ),
        key =  ~ key,
        source = "ma"
      ) %>%
        layout(
          xaxis = list(title = "BaseMean (A) = (log<sub>2</sub>(G2)+log<sub>2</sub>(G1))/2"),
          yaxis = list(title = "Log2FC (M) = log<sub>2</sub>(G2)-log<sub>2</sub>(G1)"),
          title = paste0(
            "MA Plot with q-value < ",
            input$maFDR
          ),
          legend = list(
            orientation = 'h',
            xanchor = "center",
            x = 0.5,
            y = 1.05
          )
        )
      p
    })
  })
  runMA$runMAValues <- input$makeMAPlot # run button has been clicked 
})



output$geneBarPlot <- renderPlotly({ # bar plot
  eventdata <- event_data("plotly_hover", source = "ma")
  validate(need(
    !is.null(eventdata),
    "Hover a gene to show its expression."
  )) # a bar blot of raw counts is appearign when a point of the ma plot is hovered
  
  gene_id <- eventdata$key # key bar plot 
  expression <-
    var$newData[row.names(var$newData) == gene_id, ]
  data <- var$norData # data selection 
  dataGroups <- var$selectedgroups # selected groups 
  expression <- t(expression[dataGroups != 0]) 
  
  # orders to link 
  xOrder <-
    data.frame("name" = row.names(expression), "group" = dataGroups)
  xOrderVector <- unique(xOrder[order(xOrder$group),]$name)
  xform <- list(categoryorder = "array",
                categoryarray = xOrderVector,
                title = "")
  
  plot_ly( # plot of raw expression in groups
    x = ~ row.names(expression),
    y = ~ expression[, 1],
    color = as.factor(dataGroups),
    text = expression[, 1],
    textposition = "outside",
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



output$resultTableInPlot <- DT::renderDataTable({
      fdrCut <- input$maFDR
      fdrColor <- input$fdrColor
    
    DT::datatable(   # result table 
      resultTable(),
      filter = "bottom",
      caption = tags$caption(
        tags$li("Gene Name was colored according to FDR cut-off.")
      ),
      extensions = 'Buttons',  # download buttong
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
                         filename = "results_maplot"),
          text = 'Download')),
        scrollX = TRUE,
        pageLength = 10,
        searchHighlight = TRUE,  # search bar 
        orderClasses = TRUE
      )) %>% formatRound(
        columns = c("a.value",
                    "m.value",
                    "p.value",
                    "q.value"),
        digits = 10  # digits after comma
      ) %>% formatStyle("gene_id",
                        "q.value",
                        color = styleInterval(fdrCut, c(fdrColor, ""))) # color according to cut off

},server = F)


# Under FDR cutoff, preview the gene number ----
output$maFDRpreview <- renderText({
  count <- nrow(resultTable()[resultTable()$q.value <= input$maFDR,])
  paste0("<font color=\"",
         input$fdrColor,
         "\"><b>",
         count,
         " genes</b></font>")
})

#main  plot output 
output$MAPlotUI <- renderUI({
  if (length(var$groupList2) || length(var$groupList3) == 2) { # ma plot is only available for 2 groups comparisons, error if tryign ith more
    if (runMA$runMAValues) { # if the run button has been clicked, then shows plots
      tagList(fluidRow(
        column(8, plotlyOutput("maplotly") %>% withSpinner()),
        column(4, plotlyOutput("geneBarPlot") %>% withSpinner())
      ))
    } else {
      helpText("Please click [Generate MA Plot] first.")
    }
  }else{
    sendSweetAlert(
      session = session,
      title = "ERROR",
      text = "MA Plot is unavailable for multiple comparison now.",
      type = "info"
    )
    helpText("MA Plot is unavailable for multiple comparison now.")
}
})



observeEvent(input$makeMAPlot, {
  output$runMAPlot <- renderText({
    if (resultTable()$a.value == "") { # if not basemean values, message 
      "No MA values for plotting."
    }
  })
})

