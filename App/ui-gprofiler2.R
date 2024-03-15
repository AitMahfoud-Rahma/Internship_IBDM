#ui-mfuzz.R


navbarPage("Mfuzz clustering", 
           tabPanel(             
             title = tagList(icon("info-circle"), "Info"),
             width = NULL,
             solidHeader = T,
             status = "primary",
             includeMarkdown("documents/mfuzzinfo.Rmd")
           ),
           tabPanel(
             title = tagList(icon("dice-one"), "  Inertia/Elbow"),
             fluidPage(fluidRow(
               column(3,
                      box(
                        title = tagList(icon("cogs"),"Parameters"),
                        solidHeader = T,
                        status = "primary",
                        width = NULL,
                        fileInput(
                          "mfuzzCountData",
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
                                    <th class="tg-031e"> Time points </th>
                                    </tr>
                                    <tr>
                                    </table>
                                    </div>
                                    </div>')),
                        sliderInput(
                          "maxclass",
                          "Max clusters to show ",
                          min = 2,
                          max = 40,
                          value = 20,
                          step = 1
                        ))),
               column(9,
                      uiOutput("inertia_elbowUI")
               )))),
           tabPanel(
             title = tagList(icon("dice-two"), "  Overlap"),
             fluidPage(
               fluidRow(column(3,
                               box(
                                 title = tagList(icon("exclamation-triangle"),"Choose the number of clusters"),
                                 solidHeader = T,
                                 status = "danger",
                                 width = NULL,
                                 sliderInput(
                                   "inertiaclass",
                                   "Chosen clusters number ",
                                   min = 1,
                                   max = 30,
                                   value = 5,
                                   step = 1
                                 )),
                               box(
                                 title = tagList(icon("cogs"),"Parameters"),
                                 solidHeader = T,
                                 status = "primary",
                                 width = NULL,
                                 sliderInput(
                                   "ov_threshold",
                                   "Overlap Threshold",
                                   min = 0,
                                   max = 1,
                                   value = 0.3,
                                   step = 0.1
                                 ),
                                 helpText(HTML(' Threshold for visualization. Cluster overlaps below the threshold will not be visualized.'))
                               )
               ),
               column(9, 
                      uiOutput("overlap")))
             )),
           tabPanel(
             title = tagList(icon("dice-three"), "  Mfuzz Plots"),
             fluidPage(
               fluidRow(column(12,
                               uiOutput("mfuzz")
               )))),
           tabPanel(
             title = tagList(icon("dice-four"), "Cluster Enrichment"),
             fluidPage(
               fluidRow(column(12, 
                               uiOutput("mfenrichement")))
             )))
