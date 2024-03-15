# ui-data-import.R


navbarPage(theme=shinytheme("sandstone"),"Data Visualization", # theme for navbars of the app
           tabPanel(
             title = tagList(icon("table"), "Data"),
             fluidPage(
               fluidRow(column(4,
                               box(        # uplaod data box
                                 title = tagList(icon("cloud-upload-alt"), "Upload"),
                                 solidHeader = T,
                                 status = "primary",
                                 width = NULL,
                                 fileInput(
                                   "uploadCountData",
                                   "Upload Count Data",
                                   accept = c("text/csv",
                                              "text/comma-separated-values,text/plain",
                                              ".csv"),
                                   buttonLabel = "Upload...",
                                   placeholder = "No file has been uploaded."
                                 ),
                                 tags$div( # instructions 
                                   HTML('<div class="panel panel-primary">
                    <div class="panel-heading"> <span style="padding-left:10px"><b> Input file description</b> </span></div>
                  <div class="panel-body">
                  <style type="text/css">
                  .tg {
                  border-collapse: collapse;
                  border-spacing: 0;
                  border: none;
                  }
                  .tg th {
                  font-family: Arial, sans-serif;
                  font-size: 14px;
                  font-weight: normal;
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
                  <th class="tg-031e"> <span class="label label-primary"> Format</span></th>
                  <th class="tg-031e"> comma-separated values (CSV)
                  </tr>
                  <tr>
                  <th class="tg-031e"> <span class="label label-primary"> Column 1</span></th>
                  <th class="tg-031e"> Sample ID
                  </tr>
                  <tr>
                  <th class="tg-031e"> <span class="label label-primary"> Column 2-n</span></th>
                  <th class="tg-031e"> Other metadata (condition, covariates) </th>
                  </tr>
                  <tr>
                  </table>
                  </div>
                  </div>'))
                               )),
                        
                        column(4,
                               box(     # filtering box
                                 title = tagList(icon("filter"), "Filter Low Count Genes"),
                                 solidHeader = TRUE, 
                                 status = "primary",
                                 width = NULL, 
                                 sliderInput(
                                   "filterCount",
                                   "Filter Low Count Genes",
                                   min = 0,
                                   max = 100,
                                   value = 0,
                                   step = 1
                                 )
                               ),
                               box(    # group assignement box
                                 title = tagList(icon("tags"), "Group Assignment"),
                                 solidHeader = TRUE,
                                 status = "primary",
                                 width = NULL,
                                 selectizeInput("data_samples", label="Select Samples",
                                                choices=NULL,
                                                multiple=TRUE)
                               )),
                        column(4,
                               box(           # summary of upload table box
                                 title = tagList(icon("file-alt"), "Summary"),
                                 solidHeader = TRUE,
                                 status = "primary",
                                 width = NULL,
                                 uiOutput("DataSummary"))
                               
                               
                               
                        )),
             tabBox(                  # panels of different tables box
               title = "",
               width = NULL,
               tabPanel(              # final table of selected genes
                 title = tagList(icon("chart-bar"), "Actual Table"),
                 uiOutput("showTable")
               ),
               tabPanel(             # raw input table
                 title = tagList(icon("chart-bar"), "Input Table"),
                 uiOutput("showInputTable")
               ),
               tabPanel(             # table of filtered datz
                 title = tagList(icon("chart-bar"), "Filtered Table"),
                 uiOutput("showLowTable")
               )
             ))),
           tabPanel(   # panel of the count distribution bar chart
             title = tagList(icon("chart-bar"), "Count Distribution"),
                    uiOutput("CountDistrib")
             ),
           tabPanel(  # panel of groups heatmap 
             title = tagList(icon("sitemap"), "Hierarchical Clustering"),
                    uiOutput("clustUI")
             ),
           tabPanel(   # panel of PCA
             title = tagList(icon("object-group"), "PCA"),
                    uiOutput("pcaUI")
             ))
