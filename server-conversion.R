#server-conversion.R
# ID conversion tool 

ConvRun <- reactiveValues(ConvRunValue = FALSE)    #to precise the conversion is not done at this time


observeEvent(input$convgo,{      # if the validation butto is clicked 
  
  
  
  inputids <- unlist(strsplit(input$inputids, split = '\n')) 
  conversion <- gconvert(                          #gprofiler2 package
    inputids,                         
    organism = input$chosendatabase,                
    target = input$outputtype              
  )
  
  
  
  conversion <- conversion[,-1]
  conversion <- conversion[,-2] 
  conversion <- conversion[,-5]
  output$ConvResults <-  DT::renderDataTable({ # creation of the result table
  DT::datatable(
    conversion,                                # data to add in the table
    extensions = 'Buttons',                    # adding a download button in csv format
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
      scrollX = TRUE,                        # possible scroll as the length is 10
      pageLength = 10,                       # size of the table
      searchHighlight = TRUE,                # search bar 
      orderClasses = TRUE
      
    ),
    
    class = "display")
}, server = FALSE)
  
  ConvRun$ConvRunValue <- input$convgo      # to precise the validation button has been clicked
  updateNavbarPage(session, "convtabs", "redirectconv") # automatic redirection from the Rmd panel 
                                                        # to the result table panel
  
  
})  

# result table render

output$ConversionResults <- renderUI({      # verification, if the conversion has not been done, message to do it
  if(ConvRun$ConvRunValue){
    tagList(
      fluidRow(column(
        12, dataTableOutput('ConvResults') %>% withSpinner() # if it is done, show the results
      )))} else {                
        helpText("Run Conversion to obtain the Result Table.")
      }
})
