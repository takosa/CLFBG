library(shiny)

shinyUI(
  
  navbarPage(
    "CLFBG",
    tabPanel(
      "Main",
      fluidPage(
  tags$head(
    tags$style(HTML("
      .shiny-output-error-validation {
        color: green;
      }
    "))
  ),
        titlePanel("Clustering Fluorescence-Based Genotyping Data"), 
        p("This tool cluster fluorescence-based genotyping data like KASP [1]"),
        sidebarLayout(
          sidebarPanel(
            fileInput(
              "inFile", "Input data",
              accept = c("text/csv", "application/vnd.ms-excel",
                         "application/vnd.openxmlformats-officedocument.spreadsheetml.sheet",
                         ".csv",
                         ".txt",
                         ".xls",
                         ".xlsx")),
            fileInput(
              "sampleFile", "Sample-Well file (option)",
              accept = c("text/csv", "application/vnd.ms-excel",
                         "application/vnd.openxmlformats-officedocument.spreadsheetml.sheet",
                         ".csv",
                         ".txt",
                         ".xls",
                         ".xlsx")),
            actionButton("demo", "or load demo data"),
            div(style = "height: 10px;"),
            uiOutput("sheet"),
            radioButtons("normMethod", "Choose normalization method",
                         choices = c("none", "min-max", "standard"),
                         selected = "none"),
            radioButtons("method", "Choose clustering method",
                         choiceNames = c("k-means", "k-medoids", "DBSCAN [2]", "GMM [3]"),
                         choiceValues = c("k-means", "k-medoids", "DBSCAN", "GMM"),
                         selected = "k-means"),
            conditionalPanel(
              condition = "input.method == 'k-means' || input.method == 'k-medoids'",
              tags$label("k-means or k-medoids parameters"),
              numericInput("k", "k", 4, min = 1, step = 1)
            ),
            conditionalPanel(
              condition = "input.method == 'DBSCAN'",
              tags$label("DBSCAN parameters"),
              numericInput("eps", "eps", 0.1, min = 0, step = 0.01),
              numericInput("minPts", "minPts", 5, min = 1, step = 1)
            ),
            tags$div(
              tags$label("Let the genotype of the sample indicated by the selected point be"),
              br(),
              actionButton("A", "A"),
              actionButton("B", "B"),
              actionButton("H", "H"),
              actionButton("NA_", "N/A"),
              actionButton("unknown", "?"),
              actionButton("ntc", "NTC"),
            ),
            br(),
            downloadButton("downloadData", "Download result file")
          ), 
          # Show a plot of the generated distribution
          mainPanel(
            plotOutput("plot",
                       click = "plot_click",
                       dblclick = "plot_dblclick",
                       hover = "plot_hover",
                       brush = "plot_brush"),
            DT::dataTableOutput("table"),
          )
        ),
        tags$hr(),
        tags$h3("Reference"),
        tags$ul(
          style = "list-style: none;",
          tags$li("[1] Semagn, K., Babu, R., Hearne, S. et al. Single nucleotide polymorphism
          genotyping using Kompetitive Allele Specific PCR (KASP): overview of the
          technology and its application in crop improvement. Mol Breeding 33, 1–14 (2014).
          https://doi.org/10.1007/s11032-013-9917-x"),
          tags$li("[2] Hahsler M, Piekenbrock M, Doran D (2019). “dbscan: Fast Density-Based 
          Clustering with R.” _Journal of Statistical Software_, *91*(1), 1-30.
          doi: 10.18637/jss.v091.i01 (URL: https://doi.org/10.18637/jss.v091.i01)."),
          tags$li("[3] Scrucca L., Fop M., Murphy T. B. and Raftery A. E. (2016) mclust 5:
          clustering, classification and density estimation using Gaussian
          finite mixture models The R Journal 8/1, pp. 289-317")
        ),
      )
    ),
    tabPanel(
      "Usage",
      basicPage(
        withMathJax(),
          tags$h2("Usage"),
          tags$hr(),
          tags$h3("1. Upload input data"),
          tags$p("requiered 'Well', 'FAM', 'VIC' and 'ROX' column. Each row represent each sample"),
          tags$p("If Excel file is uploaded, you can select sheet number. 'ALL' means all sheets"),
          tags$p("You can also upload a table of mapping Well to Sample"),
          tags$h3("2. Choose a normalization(scaling) method"),
          tags$p("The meaning of each normalization(scaling) method is below."),
          tags$dl(
            tags$dt("none"),
            tags$dd("no scaling"),
            tags$dt("min-max"),
            tags$dd(div(style = "width: 20%; min-width: 560px;", "$$x'=\\frac{x-\\min(x)}{\\max(x)-\\min(x)}$$")),
            tags$dt("standard"),
            tags$dd(div(style = "width: 20%; min-width: 560px;","$$x'=\\frac{x-\\bar{x}}{\\sigma}$$"))
          ),
          tags$strong("NOTE: If all of the data points are same genotype, scaling may cause some misunderstanding"),
          tags$h3("3. Choose a clustering method"),
          tags$dl(
            tags$dt("k-means"),
            tags$dd("k-means method. set parameter k."),
            tags$dt("k-medoids"),
            tags$dd("k-medoids method. set parameter k."),
            tags$dt("DBSCAN"),
            tags$dd("Density-based spatial clustering of applications with noise. set parameter eps and minPts [2]"),
            tags$dt("GMM"),
            tags$dd("gaussian mixture models, which determin number of clusters automaticaly with BIC. The model name is 'EVE' [3]")
          ),
          tags$h3("4. Plot and table are display automaticaly"),
          tags$p("you can select points by drag on plot. The data of selected points are shown in table below."),
          tags$h3("5. Download result as CSV file"),
        tags$footer(
          tags$hr(),
          tags$h3("Reference"),
          tags$ol(
            tags$li("Semagn, K., Babu, R., Hearne, S. et al. Single nucleotide polymorphism
          genotyping using Kompetitive Allele Specific PCR (KASP): overview of the
          technology and its application in crop improvement. Mol Breeding 33, 1–14 (2014).
          https://doi.org/10.1007/s11032-013-9917-x"),
            tags$li("Hahsler M, Piekenbrock M, Doran D (2019). “dbscan: Fast Density-Based 
          Clustering with R.” _Journal of Statistical Software_, *91*(1), 1-30.
          doi: 10.18637/jss.v091.i01 (URL: https://doi.org/10.18637/jss.v091.i01)."),
            tags$li("Scrucca L., Fop M., Murphy T. B. and Raftery A. E. (2016) mclust 5:
          clustering, classification and density estimation using Gaussian
          finite mixture models The R Journal 8/1, pp. 289-317")
          ),
        )
      )
    )
  )
)
