#server-ortho.R

OrthoRun <- reactiveValues(OrthoRunValue = FALSE) # to precise the run button has not been clicked


observeEvent(input$orthogo,{  # when the button is clicked 
  progressSweetAlert(              # progress bar 
    session = session,
    id = "orthoProgress",
    title = "Orthology in progress",
    display_pct = TRUE,
    value = 0
  )
  
  
  
  search <- unlist(strsplit(input$ortho_ids, split = '\n')) # takes the gene set 
  
  updateProgressBar(              # update progress bar 
    session = session,
    id = "orthoProgress",
    title = "Orthology in progress...",
    value = 50
  )
  
  res_ortho <- gorth(search, 
                     source_organism = input$orthinputorg,
                     target_organism = input$orthtargetorg,
                     )
  res_ortho <- res_ortho[,-1]
  
  updateProgressBar(           # updating the progress bar 
    session = session,
    id = "orthoProgress",
    title = "Orthology in progress...",
    value = 75
  )
  
  
  
  
  output$OrthoResultTable <-  DT::renderDataTable({   # result table
    DT::datatable(
      res_ortho,        
      extensions = 'Buttons',    # download button 
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
                         filename = "results_orthology"),
          text = 'Download')),
        scrollX = TRUE,
        pageLength = 10,
        searchHighlight = TRUE,     # search bar 
        orderClasses = TRUE
        
      ),
      
      class = "display")
  }, server = FALSE)
  
  OrthoRun$OrthoRunValue <- input$orthogo   # precise the run button has been clicked
  updateNavbarPage(session, "ortabs", "redirectres") # redirection to the result table after the enrichment is done
  
  closeSweetAlert(session = session)        # close alert that the enrichment is done 
  sendSweetAlert(session = session,
                 title = "DONE",
                 text = "Orthology was successfully performed.",
                 type = "success")
  
  ##########
  

  
})

# result table render

output$OrthoResults <- renderUI({
  if(OrthoRun$OrthoRunValue){   # if the run button has been clicked, then show the results
    tagList(
      fluidRow(column(
        12, dataTableOutput('OrthoResultTable') %>% withSpinner()
      )))} else { 
        if(length(res_ortho) == 0){
         helpText("No results to show") 
          }else{# if not message to do it 
        helpText("Run Orthology to obtain the Result Table.")
      }}
})




