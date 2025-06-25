# 2) Module UI
downloadModuleUI <- function(id) {
  ns <- NS(id)
  fluidRow(
    column(4,
           h4("Downloads"),
           # link to download entire DB as zip
           downloadLink(ns("dl_all"), "Download All Tables (ZIP)"),
           tags$hr(),
           # individual table links, no extra spacing
           div(class = "download-links",
               uiOutput(ns("download_links"))
           )
    ),
    column(8,
           # selector + DT in one column
           selectInput(ns("table_select"), "Choose table:", choices = NULL, width = "100%"),
           DTOutput(ns("table_view"))
    )
  )
}

# 3) Module Server
downloadModuleServer <- function(id, con) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    # reactive list of all tables
    tables <- reactive(dbListTables(con))
    
    # populate selector as soon as tables() is ready
    observeEvent(tables(), {
      tbls <- tables()
      updateSelectInput(session, "table_select",
                        choices  = tbls,
                        selected = if (length(tbls)) tbls[[1]] else NULL)
    }, ignoreNULL = FALSE)
    
    # render the selected table; req() ensures it only fires once something is chosen
    output$table_view <- renderDT({
      req(input$table_select)
      df <- dbReadTable(con, input$table_select)
      datatable(df, options = list(pageLength = 10, scrollX = TRUE))
    })
    
    # link to download every single table
    output$download_links <- renderUI({
      tbls <- tables()
      if (length(tbls) == 0) {
        return(tags$p("No tables available"))
      }
      # plain links, one per line, no extra <br> spacing
      lapply(tbls, function(tbl) {
        tags$div(style = "margin:0; padding:2px 0;",
                 downloadLink(ns(paste0("dl_", tbl)), tbl)
        )
      })
    })
    
    # one downloadHandler per table
    observe({
      lapply(tables(), function(tbl) {
        output[[paste0("dl_", tbl)]] <- downloadHandler(
          filename = function() paste0(tbl, ".csv"),
          content  = function(file) {
            write.csv(dbReadTable(con, tbl), file, row.names = FALSE)
          }
        )
      })
    })
    
    # download handler for the whole DB as a zip of CSVs
    output$dl_all <- downloadHandler(
      filename = "all_tables.zip",
      content = function(file) {
        td    <- tempdir()
        tbls  <- tables()
        paths <- vapply(tbls, function(tbl) {
          f <- file.path(td, paste0(tbl, ".csv"))
          write.csv(dbReadTable(con, tbl), f, row.names = FALSE)
          f
        }, character(1))
        zip::zipr(zipfile = file, files = paths, root = td)
      }
    )
  })
}