#server-conversion.R
ConvRun <- reactiveValues(ConvRunValue = FALSE) # precise the run button has not been clicked


observeEvent(input$convgo,{ # when the run button is clicked 
  
  
  
  inputids <- unlist(strsplit(input$inputids, split = '\n')) # takes gene list
  conversion <- bitr( # conversion 
    geneID = inputids, 
    fromType = input$inputtype,
    toType = c('ENTREZID','ENSEMBL','SYMBOL'),
    OrgDb = input$chosendatabase
  )
  
  
  
  conversion <- as.data.frame(conversion) #as data frame to show result
  
  output$ConvResults <-  DT::renderDataTable({ # result table render 
    DT::datatable(
      conversion,        
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
                         filename = "results_conversion"),
          text = 'Download')),
        scrollX = TRUE,
        pageLength = 10,
        searchHighlight = TRUE,
        orderClasses = TRUE
        
      ),
      
      class = "display")
  }, server = FALSE)
  
  ConvRun$ConvRunValue <- input$convgo #precise the run button has been clicked to show results 
  updateNavbarPage(session, "convtabs", "redirectconv") # redirection from info panel to result table panel
  
  
})  

# result table render

output$ConversionResults <- renderUI({
  if(ConvRun$ConvRunValue){
    tagList(
      fluidRow(column(
        12, dataTableOutput('ConvResults') %>% withSpinner()
      )))} else {
        helpText("Run Conversion to obtain the Result Table.")
      }
})
