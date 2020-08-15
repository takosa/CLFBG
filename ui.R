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
        titlePanel("clustering fluorescence-based genotyping data"), 
        p("This tool cluster fluorescence-based genotyping data like KASP",tags$sup("*")),
        p(
          "* Semagn, K., Babu, R., Hearne, S. et al. Single nucleotide polymorphism genotyping using Kompetitive Allele Specific PCR (KASP): overview of the technology and its application in crop improvement. Mol Breeding 33, 1–14 (2014). https://doi.org/10.1007/s11032-013-9917-x",
        ),
        sidebarLayout(
          sidebarPanel(
            tabsetPanel(
              tabPanel(
                title = "Analysis",
                
                fileInput(
                  "inFile", "Input data",
                  accept = c("text/csv", "application/vnd.ms-excel",
                             "application/vnd.openxmlformats-officedocument.spreadsheetml.sheet",
                             ".csv",
                             ".xls",
                             ".xlsx")),
                tags$span("or"),
                actionButton("demo", "load demo data"),
                uiOutput("sheet"),
                radioButtons("normMethod", "Choose normalization method",
                             choices = c("none", "conventional", "min-max", "standard"),
                             selected = "none"),
                radioButtons("method", "Choose clustering method",
                             choices = c("k-means", "k-medoids", "DBSCAN", "gauss-mix"),
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
                br(),
                downloadButton("downloadData", "Download CSV file")
              ),
              tabPanel(
                title = "Edit",
                
                tags$div(
                  tags$label("Let the genotype of the sample indicated by the selected point be"),
                  actionButton("A", "A"),
                  actionButton("B", "B"),
                  actionButton("H", "H"),
                  actionButton("NA_", "N/A"),
                  actionButton("unknown", "?"),
                ),
              )
            )
          ), 
          # Show a plot of the generated distribution
          mainPanel(
            plotOutput("plot",
                       click = "plot_click",
                       dblclick = "plot_dblclick",
                       hover = "plot_hover",
                       brush = "plot_brush"),
            DT::dataTableOutput("table")
          )
        )
      )
    ),
    tabPanel(
      "Usage",
      fluidPage(
        tags$h1("Usage"),
        tags$hr(),
        tags$h2("1. Upload input data"),
        tags$p("requiered 'Well', 'FAM', 'VIC' and 'ROX' column. Each row represent each sample"),
        tags$p("If Excel file is uploaded, you can select sheet number. 'ALL' means all sheets"),
        tags$h2("2. Choose a normalization(scaling) method"),
        tags$p("The meaning of each normalization(scaling) method is below."),
        tags$dl(
          tags$dt("none"),
          tags$dd("no scaling"),
          tags$dt("min-max"),
          tags$dd(withMathJax("$$x'=\\frac{x-\\min(x)}{\\max(x)-\\min(x)}$$")),
          tags$dt("standard"),
          tags$dd(withMathJax("$$x'=\\frac{x-\\bar{x}}{\\sigma}$$"))
        ),
        tags$strong("NOTE: If all of the data points are same genotype, scaling may cause some misunderstanding"),
        tags$h2("3. Choose a clustering method"),
        tags$dl(
          tags$dt("k-means"),
          tags$dd("k-means method. set parameter k."),
          tags$dt("k-medoids"),
          tags$dd("k-medoids method. set parameter k."),
          tags$dt("DBSCAN"),
          tags$dd("Density-based spatial clustering of applications with noise. set parameter eps and minPts"),
          tags$dt("gauss-mix"),
          tags$dd("gaussian mixture models, which determin number of clusters automaticaly with BIC. The model name is 'EVE'")
        ),
        tags$h2("4. Plot and table are display automaticaly"),
        tags$p("you can select points by drag on plot. The data of selected points are shown in table below."),
        tags$h2("5. Download result as CSV file"),
        tags$footer(
          
        )
      )
    )
  )
)
