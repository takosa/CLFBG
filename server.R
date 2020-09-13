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


# input function
get_tables <- function(file) {
    isExcel <- grepl("(\\.xls|\\.xlsx)$", file)
    isCsv <- grepl("\\.csv$", file)
    isTxt <- grepl("\\.txt$", file)
    if (isExcel) {
        # read Excel file
        sheetNames <- readxl::excel_sheets(file)
        tables <- lapply(sheetNames, function(n) {
            readxl::read_excel(file, sheet = n)
        })
        names(tables) <- sheetNames
    } else if (isCsv) {
        # read csv file
        tables <- list(read.csv(file))
        names(tables) <- "sheet1"
    } else if (isTxt) {
        # read txt file
        tables <- list(read.delim(file))
        names(tables) <- "sheet1"
    } else {
        stop("Invalide file type!")
    }
    tables
}

# format table function
format_tables <- function(tables) {
    tables <- imap(tables, function(tbl, n) {
        isFormatOk <- "Well" %in% colnames(tbl) &&
            any(startsWith(colnames(tbl), fam)) &&
            any(startsWith(colnames(tbl), vic)) &&
            any(startsWith(colnames(tbl), rox))
        if (isFormatOk) {
            tbl <-
                tbl %>% select(
                    Well,
                    FAM = first(starts_with(fam, ignore.case = TRUE)),
                    VIC = first(starts_with(vic, ignore.case = TRUE)),
                    ROX = first(starts_with(rox, ignore.case = TRUE))) %>%
                mutate(`FAM/ROX` = FAM / ROX, `VIC/ROX` = VIC / ROX, sheet = n)
            return(tbl)
        } else {
            return(NULL)
        }
    })
    tables <- compact(tables)
    if (length(tables) == 0) {
        stop("Tables should include Well, FAM, VIC and ROX columns!")
    }
    tables
}

shinyServer(function(input, output, session) {
    
    variables <- reactiveValues(
        tables = NULL,
        isExcel = FALSE,
        input_error = FALSE,
        isDownloaded = FALSE
    )
    
    # input demo data
    observeEvent(input$demo, {
        tables <- get_tables("demo.xlsx")
        tables <- format_tables(tables)
        variables$tables <- tables
        variables$joinedTables <- tables
    })
    
    # input user uploaded data
    observeEvent(input$inFile, {
        tables <- tryCatch({
          tables <- get_tables(input$inFile$datapath)
          tables <- format_tables(tables)
        },
        error = function(e){
            showNotification(e$message, type = "error")
            NULL
        })
        variables$tables <- tables
        variables$joinedTables <- tables
    })
    
    # input sample data
    observeEvent(input$sampleFile, {
        variables$sampleTables <- tryCatch({
            get_tables(input$sampleFile$datapath)
        }, error = function(e) {
            showNotification(e$message, type = "error")
            NULL
        })
    })
    
    # join samples table and data tables
    observe({
        req(variables$tables, variables$sampleTables)
        if (length(variables$sampleTables) == length(variables$tables)) {
            names(variables$sampleTables) <- names(variables$tables)
        }
        tryCatch({
            variables$joinedTables <-  
            map2(variables$sampleTables, variables$tables,
                                     ~right_join(.x, .y, by = "Well"))
        }, error = function(e) {
            showNotification("Cannot join sample and data.", type = "error")
        })
    })
    
    # select sheet 
    observe({
        req(variables$joinedTables)
        if (is.null(input$sheet) || input$sheet == "ALL") {
            variables$selectedTables <- variables$joinedTables
        } else {
            variables$selectedTables <- variables$joinedTables[input$sheet]
        }
    })
    
    # normalize data
    observe({
        req(variables$selectedTables)
        tables <- variables$selectedTables
        
        normFun <- switch (input$normMethod,
            "none" = identity,
            "min-max" = function(x) (x - min(x)) / (max(x) - min(x)),
            "standard" = function(x) as.numeric(scale(x)),
        )
        
        tables <- lapply(tables, function(x) {
            x %>%
            mutate(`FAM/ROX` = normFun(`FAM/ROX`),
                   `VIC/ROX` = normFun(`VIC/ROX`))
        })
        variables$normalizedTables <- tables
    })
    
    # clustering data
    observe({
        req(variables$normalizedTables)
        tables <- variables$normalizedTables
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
            "GMM" = function(x) {
                require(mclust)
                mc <- mclust::Mclust(x, modelNames = "EVE")
                cls <- mc$classification
            }
        )
        
        tables <- lapply(tables, function(x) {
            x %>% mutate(cluster = clusterFun(x[c("FAM/ROX", "VIC/ROX")]))
        })
        
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
            
            tbl <- mutate(tbl, genotype = ifelse(cluster %in% names(labels), labels[as.character(cluster)], "?"))
            if (any("Sample" == colnames(tbl))) {
                tbl <- mutate(tbl, genotype = ifelse(Sample == "NTC", "NTC", genotype))
            }
            tbl
        })
        
        variables$results <- tables
    })
    
    # edit to A
    observeEvent(input$A, {
        variables$results %>% 
            bind_rows() %>% 
            brushedPoints(input$plot_brush, xvar = "FAM/ROX", yvar = "VIC/ROX", allRows = TRUE) %>%
            mutate(genotype = if_else(selected_, "A", genotype)) %>% 
            select(-selected_) -> variables$results
    })
    
    # edit to B
    observeEvent(input$B, {
        variables$results %>% 
            bind_rows() %>% 
            brushedPoints(input$plot_brush, xvar = "FAM/ROX", yvar = "VIC/ROX", allRows = TRUE) %>%
            mutate(genotype = if_else(selected_, "B", genotype)) %>% 
            select(-selected_) -> variables$results
    })
    
    # edit to H
    observeEvent(input$H, {
        variables$results %>% 
            bind_rows() %>% 
            brushedPoints(input$plot_brush, xvar = "FAM/ROX", yvar = "VIC/ROX", allRows = TRUE) %>%
            mutate(genotype = if_else(selected_, "H", genotype)) %>% 
            select(-selected_) -> variables$results
    })
    
    # edit to NA
    observeEvent(input$NA_, {
        variables$results %>% 
            bind_rows() %>% 
            brushedPoints(input$plot_brush, xvar = "FAM/ROX", yvar = "VIC/ROX", allRows = TRUE) %>%
            mutate(genotype = if_else(selected_, "N/A", genotype)) %>% 
            select(-selected_) -> variables$results
    })
    
    # edit to unknown(?)
    observeEvent(input$unknown, {
        variables$results %>% 
            bind_rows() %>% 
            brushedPoints(input$plot_brush, xvar = "FAM/ROX", yvar = "VIC/ROX", allRows = TRUE) %>%
            mutate(genotype = if_else(selected_, "?", genotype)) %>% 
            select(-selected_) -> variables$results
    })
    
    # edit to unknown(?)
    observeEvent(input$ntc, {
        variables$results %>% 
            bind_rows() %>% 
            brushedPoints(input$plot_brush, xvar = "FAM/ROX", yvar = "VIC/ROX", allRows = TRUE) %>%
            mutate(genotype = if_else(selected_, "NTC", genotype)) %>% 
            select(-selected_) -> variables$results
    })
    
    # Input UI for selecting sheet
    output$sheet <- renderUI({
        if (length(variables$joinedTables) > 1) {
            sheetNames <- names(variables$joinedTables)
            selectInput("sheet", "Select sheet", choices = c(sheetNames, "ALL"), selected = "ALL")
        }
    })
    
    # plot
    output$plot <- renderPlot({
        validate(need(variables$input_error == FALSE,
                      "Invalid input file! Please upload Excel or CSV file which include 'Well', 'FAM', 'VIC' and 'ROX' columns."))
        req(variables$results)
        tables <- variables$results
            
        values <- c("A" = "#dc143c", "B" = "#4169e1", "H" = "#3cb371", "N/A" = "#ffb6c1", "?" = "#808080", "NTC" = "#000000")
        values2 <- c("A" = "#dc143c55", "B" = "#4169e155", "H" = "#3cb37155", "N/A" = "#ffb6c155", "?" = "#80808055", "NTC" = "#00000055")
        values3 <- c("A" = 21, "B" = 21, "H" = 21, "N/A" = 21, "?" = 21, "NTC" = 24)
        tables %>% bind_rows() %>% 
            mutate(genotype = factor(genotype, levels = names(values))) %>%
            ggplot(aes(x = `FAM/ROX`, y = `VIC/ROX`)) +
            geom_point(aes(col = genotype, shape = genotype, fill = genotype)) +
            facet_wrap(vars(sheet), ncol = 3) +
            coord_equal(ratio = 1) +
            scale_colour_manual(values = values) +
            scale_fill_manual(values = values2) +
            scale_shape_manual(values = values3)
    })
    
    # tables
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
            DT::datatable(
                options = list(
                    scrollX = TRUE,
                    columnDefs = list(
                        list(targets = which("Sample" == colnames(.)), searchable = TRUE),
                        list(targets = which("VIC/ROX" == colnames(.)), searchable = FALSE),
                        list(targets = which((c("FAM", "VIC", "ROX", "FAM/ROX", "VIC/ROX")%in%colnames(.)))+1L, searchable = FALSE)
                    ) 
                )
            ) %>%
            DT::formatRound(c("FAM/ROX", "VIC/ROX"), 2)
            
    })
    
    # download
    output$downloadData <- downloadHandler(
        filename = function() {
            paste0("data-", Sys.Date(), ".", tools::file_ext(input$inFile$datapath))
        },
        content = function(con) {
            if(is.null(input$inFile)) {
                showNotification("You cannot download before upload input data.", type = "error")
                stop("Sorry, you cannot download demo data.")
            }
            isExcel <- grepl("(\\.xls|\\.xlsx)$", input$inFile$datapath)
            isCsv <- grepl("\\.csv$", input$inFile$datapath)
            isTxt <- grepl("\\.txt$", input$inFile$datapath)
            if (isExcel) {
              wb <- openxlsx::createWorkbook("Results")
              if (!is.null(names(variables$results))) {
                walk(names(variables$results), ~openxlsx::addWorksheet(wb, .))
              } else {
                walk(seq_along(variables$results), ~openxlsx::addWorksheet(wb, .))
              }
              iwalk(variables$results, ~openxlsx::writeDataTable(wb, sheet = .y, x = as.data.frame(.x)))
              iwalk(variables$results, function(x, i) {
                  values <- c("A" = "#dc143c", "B" = "#4169e1", "H" = "#3cb371", "N/A" = "#ffb6c1", "?" = "#808080")
                  x <- mutate(x, genotype = factor(genotype, levels = names(values)))
                  gp <- ggplot(x, aes(x = `FAM/ROX`, y = `VIC/ROX`)) +
                      geom_point(aes(col = genotype)) +
                      facet_wrap(vars(sheet), ncol = 3) +
                      coord_equal(ratio = 1) +
                      scale_colour_manual(values = values)
                  ggsave(tf <- tempfile(tmpdir = ".", fileext = ".png"), plot = gp)
                  openxlsx::insertImage(wb, sheet = i, file = tf, width = 6, height = 6)
              })
              openxlsx::saveWorkbook(wb, con)
            } else if (isCsv) {
              data <- bind_rows(variables$results)
              write.csv(data, con, row.names = F, quote = FALSE)
            } else if (isTxt) {
              data <- bind_rows(variables$results)
              write.table(data, con, row.names = F, quote = FALSE, sep = "\t")
            }
        }
    )
})
