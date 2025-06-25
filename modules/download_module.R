# Module UI
downloadModuleUI <- function(id) {
  ns <- NS(id)
  fluidRow(
    column(4,
           h4("Downloads"),
           # ZIP of all tables
           downloadLink(ns("dl_all"), "Download All Tables (ZIP)"),
           # db_metadata link just below zip
           tags$div(style = "margin:4px 0;",
                    downloadLink(ns("dl_db_metadata"), "Download db_metadata")
           ),
           tags$hr(),
           # all other tables
           div(class = "download-links",
               uiOutput(ns("download_links"))
           )
    ),
    column(8,
           selectInput(ns("table_select"), "Choose table:", choices = NULL, width = "100%"),
           DTOutput(ns("table_view"))
    )
  )
}

# Module Server
downloadModuleServer <- function(id, con) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    # 1) load gene symbol map once
    gene_map <- dbReadTable(con, "genes")[, c("GeneID", "symbol")]
    
    # 2) reactive list of tables
    tables <- reactive(dbListTables(con))
    
    # 3) populate selector on startup
    observeEvent(tables(), {
      tbls <- tables()
      updateSelectInput(session, "table_select",
                        choices  = tbls,
                        selected = if (length(tbls)) tbls[[1]] else NULL)
    }, ignoreNULL = FALSE)
    
    # 4) render chosen table, with symbol join if GeneID present
    output$table_view <- renderDT({
      req(input$table_select)
      df <- dbReadTable(con, input$table_select)
      if ("GeneID" %in% names(df)) {
        df <- merge(df, gene_map, by = "GeneID", all.x = TRUE)
        # put symbol next to GeneID
        cols <- c("GeneID", "symbol", setdiff(names(df), c("GeneID", "symbol")))
        df <- df[, cols]
      }
      datatable(df, options = list(pageLength = 10, scrollX = TRUE))
    })
    
    # 5) special download for db_metadata
    output$dl_db_metadata <- downloadHandler(
      filename = "db_metadata.csv",
      content  = function(file) {
        df <- dbReadTable(con, "db_metadata")
        if ("GeneID" %in% names(df)) {
          df <- merge(df, gene_map, by = "GeneID", all.x = TRUE)
          cols <- c("GeneID", "symbol", setdiff(names(df), c("GeneID", "symbol")))
          df <- df[, cols]
        }
        write.csv(df, file, row.names = FALSE)
      }
    )
    
    # 6) one download link per remaining table
    output$download_links <- renderUI({
      tbls <- setdiff(tables(), "db_metadata")
      if (length(tbls) == 0) {
        return(tags$p("No other tables available"))
      }
      lapply(tbls, function(tbl) {
        tags$div(style = "margin:0; padding:2px 0;",
                 downloadLink(ns(paste0("dl_", tbl)), tbl)
        )
      })
    })
    
    # 7) handlers for each remaining table
    observe({
      lapply(setdiff(tables(), "db_metadata"), function(tbl) {
        output[[paste0("dl_", tbl)]] <- downloadHandler(
          filename = function() paste0(tbl, ".csv"),
          content  = function(file) {
            df <- dbReadTable(con, tbl)
            if ("GeneID" %in% names(df)) {
              df <- merge(df, gene_map, by = "GeneID", all.x = TRUE)
              cols <- c("GeneID", "symbol", setdiff(names(df), c("GeneID", "symbol")))
              df <- df[, cols]
            }
            write.csv(df, file, row.names = FALSE)
          }
        )
      })
    })
    
    # 8) ZIP download of all tables (with symbol merge)
    output$dl_all <- downloadHandler(
      filename = "all_tables.zip",
      content  = function(file) {
        td   <- tempdir()
        tbls <- tables()
        paths <- vapply(tbls, function(tbl) {
          df <- dbReadTable(con, tbl)
          if ("GeneID" %in% names(df)) {
            df <- merge(df, gene_map, by = "GeneID", all.x = TRUE)
            cols <- c("GeneID", "symbol", setdiff(names(df), c("GeneID", "symbol")))
            df <- df[, cols]
          }
          f <- file.path(td, paste0(tbl, ".csv"))
          write.csv(df, f, row.names = FALSE)
          f
        }, character(1))
        zip::zipr(zipfile = file, files = paths, root = td)
      }
    )
  })
}