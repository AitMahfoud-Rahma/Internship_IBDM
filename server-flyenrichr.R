#server-flyenrichr.R
EnrichRRun <- reactiveValues(EnrichRRunValue = FALSE) # to precise the run button has not been clicked

observeEvent(input$enrichRgo,{ #when run button clicked progress bar launched
  progressSweetAlert(
    session = session,
    id = "enrichProgress",
    title = "Work in progress",
    display_pct = TRUE,
    value = 0
  )
  
  
  
  geneset <- unlist(strsplit(input$refseqids, split = '\n')) # takes the gene list 
  
  updateProgressBar( # update progress bar
    session = session,
    id = "enrichProgress",
    title = "Enrichment in progress...",
    value = 50
  )
  
  
  res <- input$chosenGO # chosen ontology enrichR
  enriched <- enrichr(geneset, res) # enrichment 
  
  
  updateProgressBar( # update progress bar 
    session = session,
    id = "enrichProgress",
    title = "Enrichement in progress...",
    value = 75
  )
  
  res_enrich <- as.data.frame(enriched) 
  res_enrich <- res_enrich[,-4]   # deleting useless values to keep the essential
  res_enrich <- res_enrich[,-4]
  res_enrich <- res_enrich[,-4]
  colnames(res_enrich) <- c("Term","Overlap","P.value","Odd.Ratio","Combined.Score","Genes") # rename columns 
  res_enrich <- res_enrich[order(res_enrich[,3]),] # p-value ordered
  n <- as.numeric(input$topres)
  res_enrich <- res_enrich[1:n,]
  output$EnrichResultTable <-  DT::renderDataTable({ # table render
    DT::datatable(
      res_enrich,        
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
                         filename = "results_enrichment"),
          text = 'Download')),
        scrollX = TRUE,
        pageLength = 10,
        searchHighlight = TRUE,
        orderClasses = TRUE
        
      ),
      
      class = "display")
  }, server = FALSE)
  
  EnrichRRun$EnrichRRunValue <- input$enrichRgo # previse run button has been clicked to show graph
  updateNavbarPage(session, "entabs", "redirectres") # redirection from info page to table 
  
  closeSweetAlert(session = session) # close progress bar 
  sendSweetAlert(session = session,
                 title = "DONE",
                 text = "Enrichment was successfully performed.",
                 type = "success")
  
  
  
  output$barenrich <- renderPlotly({ # bar chart render
    fig <- plot_ly(
      res_enrich,
      x = ~(-log(P.value)),
      y = ~reorder(Term,(-log(P.value))),
      text = ~Term, 
      textposition = 'auto',
      type = "bar",
      colors = "Reds"
    )%>% layout(title = 'Statistics of the Enrichment',
                yaxis = list(title = 'Enrichment'),
                xaxis = list(title = '-log(P-value)'))
    
    fig
  })
  
  
})

# result table render

output$EnrichResults <- renderUI({
  if(EnrichRun$EnrichRunValue){
    tagList(
      fluidRow(column(
        12, dataTableOutput('EnrichResultTable') %>% withSpinner()
      )))} else {
        helpText("Run Enrichment to obtain the Result Table.")
      }
})
  