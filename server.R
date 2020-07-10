library(shiny)
library(dplyr)
library(purrr)
library(ggplot2)

# each column name required
fam <- "FAM"
vic <- "VIC"
rox <- "ROX"

# x and y axis of plot
x <- "FAM/ROX"
y <- "VIC/ROX"

shinyServer(function(input, output, session) {
    
    variables <- reactiveValues(
        tables = NULL,
        isExcel = FALSE
    )
    
    observeEvent(input$demo, {
        sheetNames <- readxl::excel_sheets("demo.xlsx")
        tbls <- lapply(sheetNames, function(n) {
            readxl::read_excel("demo.xlsx", sheet = n) %>%
                select(Well,
                       FAM = starts_with(fam, ignore.case = TRUE),
                       VIC = starts_with(vic, ignore.case = TRUE),
                       ROX = starts_with(rox, ignore.case = TRUE)) %>%
                mutate(`FAM/ROX` = FAM / ROX, `VIC/ROX` = VIC / ROX, sheet = n)
        })
        names(tbls) <- sheetNames
        variables$tables <- tbls
        variables$isExcel <- TRUE
    })
    
    observeEvent(input$inFile, { 
        variables$isExcel <- grepl("(\\.xls|\\.xlsx)$", input$inFile$name)
        if (variables$isExcel) {
            # read table from EXCEL file
            sheetNames <- readxl::excel_sheets(input$inFile$datapath)
            tables <- lapply(sheetNames, function(n) {
                tbl <- readxl::read_excel(input$inFile$datapath, sheet = n)
                tryCatch({
                    tbl %>%
                        select(Well,
                               FAM = starts_with(fam, ignore.case = TRUE),
                               VIC = starts_with(vic, ignore.case = TRUE),
                               ROX = starts_with(rox, ignore.case = TRUE)) %>%
                        mutate(`FAM/ROX` = FAM / ROX, `VIC/ROX` = VIC / ROX, sheet = n)
                }, error = function(e) NULL)
            })
            
            if (!all(sapply(tables, is.null))) {
                sheetNames <- sheetNames[!sapply(tables, is.null)]
                tables <- purrr::compact(tables)
                names(tables) <- sheetNames
            } else {
                stop("Invalid input file.")
            }
            
        } else {
            # read table from CSV file
            tables <- tryCatch({
                read.csv(input$inFile$datapath, check.names = F) %>%
                    select(FAM = starts_with(fam, ignore.case = TRUE),
                           VIC = starts_with(vic, ignore.case = TRUE),
                           ROX = starts_with(rox, ignore.case = TRUE)) %>%
                    mutate(`FAM/ROX` = FAM / ROX, `VIC/ROX` = VIC / ROX, sheet = 1)
            }, error = function(e) {
                stop("Invalid input file.")
            })
            tables <- list(tables)
        }
        variables$tables <- tables
    })
        
    #runNormalization <- 
    observe({
        req(variables$tables)
        tables <- variables$tables
        
        normFun <- switch (input$normMethod,
            "none" = identity,
            "min-max" = function(x) (x - min(x)) / (max(x) - min(x)),
            "standard" = function(x) as.numeric(scale(x)),
        )
        
        req(input$sheet)
        if (variables$isExcel) {
            if (input$sheet == "ALL") {
                tables <- lapply(tables, function(x) {
                    x %>%
                    mutate(`FAM/ROX` = normFun(`FAM/ROX`),
                           `VIC/ROX` = normFun(`VIC/ROX`))
                })
            } else {
                tables <- tables[[input$sheet]] %>%
                    mutate(`FAM/ROX` = normFun(`FAM/ROX`),
                           `VIC/ROX` = normFun(`VIC/ROX`))
                tables <- list(tables)
            }
        } else {
            tables <- tables[[1]] %>% 
                mutate(`FAM/ROX` = normFun(`FAM/ROX`),
                       `VIC/ROX` = normFun(`VIC/ROX`))
            tables <- list(tables)
        }
        
    #    variables$tables <- tables
    ##})
    #
    ##runClustering <- 
    ##observe({
    #    tables <- variables$tables
        clusterFun <- switch (input$method,
            "k-means" = function(x) {
                km <- kmeans(x, centers = input$k, iter.max = 100, nstart = 100)
                cls <- km$cluster
            },
            "k-medoids" = function(x) {
                km <- cluster::pam(x, k = input$k, stand = FALSE)
                cls <- km$clustering
            },
            "DBSCAN" = function(x) {
                ds <- dbscan::dbscan(x, eps = input$eps, minPts = input$minPts)
                cls <- ds$cluster
            },
            "gauss-mix" = function(x) {
                require(mclust)
                mc <- mclust::Mclust(x, modelNames = "EVE")
                cls <- mc$classification
            }
        )
        
        tables <- lapply(tables, function(x) {
            x %>% mutate(cluster = clusterFun(x[c("FAM/ROX", "VIC/ROX")]))
        })
        
    #    variables$table <- tables
    ##})
    #
    ##interpretResult <- 
    ##observe({
        tables <- tables %>%
            lapply(function(tbl){
            # centers of each cluster exclude noize which labelled 0
            centers <- tbl %>%
                group_by(cluster) %>%
                summarise(x = mean(`FAM/ROX`), y = mean(`VIC/ROX`)) %>%
                filter(cluster != 0)
            # range of x and y
            xr <- range(tbl$`FAM/ROX`)
            yr <- range(tbl$`VIC/ROX`)
            # determin N/A cluster whose center is in 1/4 bottom left 
            # and which has smallest x and smallest y
            na_no <- centers %>%
                filter(x < diff(xr)/4+xr[1] & y < diff(yr)/4+yr[1]) %>%
                mutate(n2 = (x-xr[1])^2 + (y-yr[1])^2) %>%
                arrange(n2) %>%
                head(1) %>%
                pull(cluster)
            if (length(na_no) == 0) na_no <- -1
            # determin B cluster
            b_no <- centers %>% filter(cluster != na_no) %>%
                filter(y >= yr[1] & y <= (x-xr[1]) * tan(pi/6) + yr[1]) %>%
                mutate(n2 = -((x-xr[1])^2 + (y-yr[1])^2)) %>%
                arrange(n2) %>%
                head(1) %>%
                pull(cluster)
            if (length(b_no) == 0) b_no <- -2
            # determin A cluster
            a_no <- centers %>% filter(!cluster %in% c(na_no, b_no)) %>%
                filter(x >= xr[1] & y >= (x - xr[1]) * tan(pi/3) + yr[1]) %>%
                mutate(n2 = -((x-xr[1])^2 + (y-yr[1])^2)) %>%
                arrange(n2) %>%
                head(1) %>%
                pull(cluster)
            if (length(a_no) == 0) a_no <- -3
            # determin Hetero cluster
            h_no <- centers %>% filter(!cluster %in% c(na_no, a_no, b_no)) %>%
                filter(x >= (x - xr[1]) * tan(pi/6) + yr[1] & y <= (x - xr[1]) * tan(pi/3) + yr[1]) %>%
                mutate(n2 = -((x-xr[1])^2 + (y-yr[1])^2)) %>%
                arrange(n2) %>%
                head(1) %>%
                pull(cluster)
            if (length(h_no) == 0) h_no <- -4
            
            # labelling (if it is niether A nor B nor H nor N/A, it is labelled as ?.)
            labels <- c("?", "A", "B", "H", "N/A")
            names(labels) <- as.character(c(0, a_no, b_no, h_no, na_no))
            
            mutate(tbl, genotype = ifelse(cluster %in% names(labels), labels[as.character(cluster)], "?"))
        })
        
        variables$results <- tables
    })
    
    output$sheet <- renderUI({
        sheetNames <- names(variables$tables)
        selectInput("sheet", "Select sheet", choices = c(sheetNames, "ALL"))
    })
    
    output$plot <- renderPlot({
        
        req(variables$results)
        tables <- variables$results
            
        values <- c("A" = "#dc143c", "B" = "#4169e1", "H" = "#3cb371", "N/A" = "#ffb6c1", "?" = "#808080")
        tables %>% bind_rows() %>%
            mutate(genotype = factor(genotype, levels = names(values))) %>%
            ggplot(aes(x = `FAM/ROX`, y = `VIC/ROX`)) +
            geom_point(aes(col = genotype)) +
            facet_wrap(vars(sheet), ncol = 3) +
            coord_equal(ratio = 1) +
            scale_colour_manual(values = values)
    })
    
    output$table <- DT::renderDataTable({
        req(variables$results)
        tables <- variables$results
        bind_rows(tables) %>% 
            brushedPoints(input$plot_brush, xvar = "FAM/ROX", yvar = "VIC/ROX", allRows = TRUE) %>%
            {
                if (any(.$selected_)) {
                    filter(., selected_)
                } else {
                    .
                }
            } %>%
            select(-selected_) %>%
            DT::datatable() %>%
            DT::formatRound(c("FAM/ROX", "VIC/ROX"), 2)
            
    })
    
    output$downloadData <- downloadHandler(
        filename = function() {
            paste("data-", Sys.Date(), ".csv", sep = "")
        },
        content = function(con) {
            data <- bind_rows(variables$results)
            write.csv(data, con)
        }
    )
})
