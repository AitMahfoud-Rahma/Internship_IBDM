# ui-pca.R

fluidPage(column(
  3,
  box(                         # parameter box
    title = tagList(icon("cogs"), "PCA Parameters"),
    width = NULL,
    solidHeader = TRUE,
    status = "primary",
    uiOutput("CondPCAParams")
  )
),
column(
  9,
  navbarPage("Results",       # result panels
             tabPanel(        # 2D pca panel
               tagList(icon("square"), "PCA Plot (2D)"),  
                      uiOutput("D2PlotUI")
               ), 
             tabPanel(       # 3D pca panel
               tagList(icon("cube"), "PCA Plot (3D)"),
                      uiOutput("D3PlotUI")
               ),
             tabPanel(        # norm sample heatmap
               tagList(icon("sitemap"), 'Normalized Samples Heatmap'),
               uiOutput("normclust")
             )
             
  )
))