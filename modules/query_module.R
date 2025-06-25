# query_module.R

# Module UI function
queryModuleUI <- function(id, individual_tables, filter_metadata, class_table_map) {
  ns <- NS(id)
  tagList(
    sidebarLayout(
      sidebarPanel(
        width = 6,  # 1/4 width
        div(
          class = "custom-sidebar",
          div(
            class = "overflow-auto",
            style = "max-height: 85vh;",
            
            accordion(
              # Info
              accordion_panel(
                span(
                  tagList(icon("info-square"), "Information"),
                  tooltip(
                    bs_icon("info-circle"),
                    "Information about this page.",
                    placement = "right"
                  )
                ),
                uiOutput(ns("query_page_info")),
                value = "info"
              ),
              
              # SAVED GENE LISTS UI
              accordion_panel(
                span(
                  tagList(icon("list"), "Save Gene List"),
                  tooltip(
                    bs_icon("info-circle"),
                    "Save the Matching Genes, including the filters applied to retreive the list.",
                    placement = "right"
                  )
                ),
                uiOutput(ns("saved_gene_lists_ui")),
                actionButton(ns("add_gene_list"), "+ Save current gene list", class = "btn btn-success"),
                value = "saved"
              ),
              
              # FILTERS UI
              accordion_panel(
                span(
                  tagList(icon("filter"), "Filters"),
                  tooltip(
                    bs_icon("info-circle"),
                    "Set filters and retreive the intersecting Genes from the resulting query.",
                    placement = "right"
                  )
                ),
                tagList(
                  fluidRow(
                    column(6, actionButton(ns("clear_filters"), "Clear Filters", width = "100%")),
                    column(6, actionButton(ns("show_filters"), "View Filters", width = "100%"))
                  ),
                  hr(),
                  # Build outer accordion by class
                  accordion(
                    lapply(names(class_table_map), function(class_name) {
                      tables_in_class <- class_table_map[[class_name]]
                      
                      accordion_panel(
                        title = class_name,
                        value = class_name,
                        accordion(
                          lapply(tables_in_class, function(tbl) {
                            # Get the nice name for this table
                            nice_name <- filter_metadata$table_nice_name[match(tbl, filter_metadata$table)]
                            if (is.na(nice_name) || nice_name == "") nice_name <- tbl
                            
                            accordion_panel(
                              title = nice_name,
                              value = tbl,
                              uiOutput(ns(paste0("filters_", tbl)))
                            )
                          }),
                          open = TRUE
                        )
                      )
                    }),
                    open = TRUE
                  )
                ),
                value = "controls"
              ),
              
              accordion_panel(
                span(
                  tagList(icon("upload"), "Input Custom list"),
                  tooltip(
                    bs_icon("info-circle"),
                    "Select the Table and Column to query then input a custom set of values. E.g. A Gene List, A list of Phenotypes. N.b. This query will be applied in conjunction with any queries from the Filters tab.",
                    placement = "right"
                  )
                ),
                tagList(
                  fluidRow(
                    column(12, actionButton(ns("list_input"), "Input custom list", width = "100%", class = "btn-primary"))
                  )
                ),
                value = "input_list"
              ),
              
              open = "info"
            )
          )
        )
      ),
      
      mainPanel(
        width = 6,
        uiOutput(ns("filtered_genes_panel"))
      )
    ) # End outer div
  ) # End tagList
}

# Module Server function
queryModuleServer <- function(id, con, individual_tables, saved_gene_lists, filter_metadata, class_table_map) {
  moduleServer(id, function(input, output, session) {
    #____________________________________________________________________#
    ##### +++++++++++++++++++++++++ SideBar +++++++++++++++++++++++++ ####
    #--------------------------------------------------------------------#
    
    ##### ========================= Reactive value for GeneID & Symbol mappings ========================= #####
    ### Reactive gene mapping from the "genes" table (assumes columns 'GeneID' and 'symbol') (for showing symbols in Plotly Traces)
    gene_mapping <- reactive({
      dbReadTable(con, "genes")
    })
    
    ######################################################################
    ##### ==================== Saved Gene Lists ===================== ####
    ######################################################################
    
    ##### ========================= Save gene lists (UI) ========================= #####
    output$saved_gene_lists_ui <- renderUI({
      if(length(saved_gene_lists$data) == 0) {
        HTML("<em>No saved gene lists.</em>")
      } else {
        tagList(
          lapply(names(saved_gene_lists$data), function(name) {
            count <- length(saved_gene_lists$data[[name]]$genes)
            applyId <- paste0("apply_", gsub(" ", "_", name))
            removeId <- paste0("remove_", gsub(" ", "_", name))
            fluidRow(
              column(3, strong(name)),
              column(3, paste("Genes:", count)),
              column(3, actionButton(session$ns(applyId), "Apply Filters", class = "btn-sm")),
              column(3, actionButton(session$ns(removeId), "Remove", class = "btn-danger btn-sm"))
            )
          })
        )
      }
    })
    
    ##### ========================= Save gene list Modal (UI) ========================= #####
    observeEvent(input$add_gene_list, {
      showModal(modalDialog(
        title = "Save Gene List",
        textInput(session$ns("gene_list_name"), "Enter a name for this gene list:"),
        footer = tagList(
          modalButton("Cancel"),
          actionButton(session$ns("confirm_save"), "Save")
        ),
        easyClose = TRUE
      ))
    })
    
    ##### ========================= Save Gene List (Server) ========================= #####
    observeEvent(input$confirm_save, {
      req(input$gene_list_name)
      current_genes <- intersected_gene_ids()
      current_filters <- reactiveValuesToList(input)
      filter_keys <- names(current_filters)[sapply(names(current_filters), function(x) {
        any(sapply(individual_tables, function(tbl) {
          startsWith(x, paste0(tbl, "_"))
        }))
      })]
      saved_filters <- current_filters[filter_keys]
      saved_gene_lists$data[[input$gene_list_name]] <- list(
        genes = current_genes,
        filters = saved_filters
      )
      removeModal()
    })
    
    ##### ========================= "Apply filters" Button (Server) ========================= #####
    observe({
      req(saved_gene_lists$data)
      for(name in names(saved_gene_lists$data)) {
        local({
          listName <- name
          # Dynamically namespace the button ID
          applyId <- paste0("apply_", gsub(" ", "_", name))
          observeEvent(input[[applyId]], {
            saved_filters <- saved_gene_lists$data[[listName]]$filters
            for(key in names(saved_filters)) {
              val <- saved_filters[[key]]
              if (is.logical(val)) {
                updateSwitchInput(session, key, value = val, onLabel = "include", offLabel = "exclude")
              } else if (is.numeric(val)) {
                updateSliderInput(session, key, value = val)
              } else {
                updateSelectInput(session, key, selected = val)
              }
            }
          }, ignoreInit = TRUE)
        })
      }
    })
    
    ##### ========================= Remove saved gene list (Server) ========================= #####
    observe({
      req(saved_gene_lists$data)
      for(name in names(saved_gene_lists$data)) {
        local({
          listName <- name
          removeId <- paste0("remove_", gsub(" ", "_", name))
          observeEvent(input[[removeId]], {
            saved_gene_lists$data[[listName]] <- NULL
          }, ignoreInit = TRUE)
        })
      }
    })
    
    
    ########################################################################
    ##### ================= Filter Controls ================= ####
    ########################################################################
    get_filter_description <- function(table_name, column_name) {
      entry <- filter_metadata[filter_metadata$table == table_name & filter_metadata$column == column_name, ]
      
      if (nrow(entry) > 0 && !is.na(entry$description)) {
        description <- entry$description[1]
        source <- entry$data_source[1]
        
        # Escape HTML if needed, then combine with <br>
        if (!is.na(source) && nzchar(source)) {
          return(HTML(paste0("<strong>Description:</strong> ", description, "<br><br><strong>Source:</strong> ", source)))
        } else {
          return(HTML(description))
        }
      } else {
        return(NULL)
      }
    }
    
    get_nice_col_name <- function(table_name, column_name) {
      entry <- filter_metadata[filter_metadata$table == table_name & filter_metadata$column == column_name, ]
      if (nrow(entry) > 0 && !is.na(entry$column_nice_name)) {
        return(entry$column_nice_name)
      } else {
        return(NULL)
      }
    }
    ##### ========================= Generate Filter Input for each Data Column (UI) ========================= #####
    lapply(individual_tables, function(tbl) {
      output[[paste0("filters_", tbl)]] <- renderUI({
        cols <- setdiff(dbListFields(con, tbl), "GeneID")
        ui_list <- lapply(cols, function(col) {
          input_id <- paste(tbl, col, sep = "_")
          na_id <- paste(input_id, "na", sep = "_")
          # sample_val <- dbGetQuery(con, sprintf("SELECT %s FROM %s LIMIT 1", col, tbl))[[col]]
          sample_val <- dbGetQuery(con, sprintf("SELECT %s FROM %s WHERE %s IS NOT NULL LIMIT 1", col, tbl, col))[[col]]
          
          # Get tooltip text
          nice_col_name <- get_nice_col_name(tbl, col)
          tooltip_text <- get_filter_description(tbl, col)

          if (is.numeric(sample_val)) {
            vals <- dbGetQuery(con, sprintf("SELECT %s FROM %s WHERE %s IS NOT NULL", col, tbl, col))[[col]]
            tagList(
              sliderInput(
                session$ns(input_id), 
                label = tooltip(
                  trigger = list(
                    nice_col_name,
                    bs_icon("info-circle")
                  ),
                  tooltip_text
                ),
                min = min(vals, na.rm = TRUE),
                max = max(vals, na.rm = TRUE),
                value = range(vals, na.rm = TRUE)
              ),
              switchInput(session$ns(na_id), value = TRUE, onLabel = "include", offLabel = "exclude", size = "mini")
            )
          } else {
            tagList(
              selectizeInput(
                session$ns(input_id), 
                label = tooltip(
                  trigger = list(
                    nice_col_name,
                    bs_icon("info-circle")
                  ),
                  tooltip_text
                ),
                choices = c("All", "Has no data"),
                selected = character(0), multiple = TRUE,
                # selected = "All", multiple = TRUE,
                options = list(
                  placeholder = paste("e.g.", sample_val)
                )
              ),
              switchInput(session$ns(na_id), value = TRUE, onLabel = "include", offLabel = "exclude", size = "mini")
            )
          }
        })
        do.call(tagList, ui_list)
      })
    })
    
    lapply(individual_tables, function(tbl) {
      shiny::outputOptions(
        output,
        paste0("filters_", tbl),
        suspendWhenHidden = FALSE
      )
    })
    
    ##### ========================= Add Choices to Non-Numeric Filters ========================= #####
    observe({
      for(tbl in individual_tables) {
        cols <- dbListFields(con, tbl)
        for(col in cols) {
          if(col == "GeneID") next
          input_id <- paste(tbl, col, sep = "_")
          sample_val <- tryCatch({
            dbGetQuery(con, sprintf("SELECT %s FROM %s LIMIT 1", col, tbl))[[col]]
          }, error = function(e) NULL)
          if (!is.null(sample_val) && !is.numeric(sample_val)) {
            queryVals <- dbGetQuery(con, sprintf("SELECT DISTINCT %s FROM %s", col, tbl))[[col]]
            uniqueVals <- unique(queryVals)
            uniqueVals <- uniqueVals[!is.na(uniqueVals)]
            choices <- c("All", "Has no data", uniqueVals)
            updateSelectizeInput(session, input_id, choices = choices, server = TRUE)
          }
        }
      }
    })
    
    ##### ========================= Function: Filter Data Table (Server) ========================= #####
    filtered_data <- function(tbl) {
      reactive({
        cols <- dbListFields(con, tbl)
        conditions <- c()
        params <- list()
        for (col in cols) {
          if(col == "GeneID") next
          input_id <- paste(tbl, col, sep = "_")
          na_id <- paste(input_id, "na", sep = "_")
          sample_val <- dbGetQuery(con, sprintf("SELECT %s FROM %s LIMIT 1", col, tbl))[[col]]
          include_na <- isTRUE(input[[na_id]])
          if (is.numeric(sample_val)) {
            slider_val <- input[[input_id]]
            if (!is.null(slider_val)) {
              if (include_na) {
                conditions <- c(conditions, sprintf("(%s BETWEEN ? AND ? OR %s IS NULL)", col, col))
              } else {
                conditions <- c(conditions, sprintf("%s BETWEEN ? AND ?", col))
              }
              params <- c(params, slider_val[1], slider_val[2])
            }
          } else {
            input_val <- input[[input_id]]
            if (!is.null(input_val)) {
              if("Has no data" %in% input_val) {
                conditions <- c(conditions, sprintf("GeneID NOT IN (SELECT GeneID FROM %s WHERE %s IS NOT NULL)", tbl, col))
              } else if(length(input_val) > 0 && !("All" %in% input_val)) {
                placeholders <- paste(rep("?", length(input_val)), collapse = ", ")
                if(include_na) {
                  conditions <- c(conditions, sprintf("(%s IN (%s) OR %s IS NULL)", col, placeholders, col))
                } else {
                  conditions <- c(conditions, sprintf("%s IN (%s)", col, placeholders))
                }
                params <- c(params, input_val)
              } else {
                if (!include_na) {
                  conditions <- c(conditions, sprintf("%s IS NOT NULL", col))
                }
              }
            }
          }
        }
        sql <- paste("SELECT * FROM", tbl)
        if (length(conditions) > 0) {
          sql <- paste(sql, "WHERE", paste(conditions, collapse = " AND "))
        }
        
        dbGetQuery(con, sql, params = params)
      }) |> debounce(50)
    }
    
    ##### ========================= Apply Filters to All Tables: Intersect GeneIDs ========================= #####
    intersected_gene_ids <- reactive({
      filtered_ids <- lapply(individual_tables, function(tbl) {
        df <- filtered_data(tbl)()
        # print(head(df))
        unique(df$GeneID)
      })
      if (length(filtered_ids) == 0) return(character(0))
      Reduce(intersect, filtered_ids)
    })
    
    ##### ========================= "View Filters" HTML construction ========================= #####
    output$current_filters <- renderUI({
      all_inputs <- reactiveValuesToList(input)
      filter_keys <- names(all_inputs)[sapply(names(all_inputs), function(x) {
        any(sapply(individual_tables, function(tbl) {
          startsWith(x, paste0(tbl, "_"))
        }))
      })]
      filters <- all_inputs[filter_keys]
      tables <- unique(sapply(filter_keys, function(x) strsplit(x, "_")[[1]][1]))
      html_out <- ""
      for(tbl in tables) {
        tbl_keys <- filter_keys[startsWith(filter_keys, paste0(tbl, "_"))]
        rows <- sapply(tbl_keys, function(key) {
          colname <- sub(paste0("^", tbl, "_"), "", key)
          value <- filters[[key]]
          if (is.vector(value) && length(value) > 1) {
            value <- paste(value, collapse = ", ")
          }
          paste0("<tr><td style='padding:4px;'><strong>", colname, "</strong></td>",
                 "<td style='padding:4px;'>", value, "</td></tr>")
        })
        tbl_html <- paste0("<h4>", tbl, "</h4>",
                           "<table style='width:100%; border-collapse: collapse; border: 1px solid #ccc;'>",
                           "<tr><th style='padding:4px;'>Filter</th><th style='padding:4px;'>Value</th></tr>",
                           paste(rows, collapse = ""), "</table>")
        html_out <- paste(html_out, tbl_html, sep = "<br>")
      }
      HTML(html_out)
    })
    
    ##### ========================= "View filters" (UI) ========================= #####
    observeEvent(input$show_filters, {
      showModal(modalDialog(
        title = "Current Filters",
        size = "l",
        easyClose = TRUE,
        footer = modalButton("Close"),
        uiOutput(session$ns("current_filters"))
      ))
    })
    
    ##### ========================= "Clear filters" ========================= #####
    observeEvent(input$clear_filters, {
      for(tbl in individual_tables) {
        cols <- dbListFields(con, tbl)
        for(col in cols) {
          if(col == "GeneID") next
          input_id <- paste(tbl, col, sep = "_")
          na_id <- paste(input_id, "na", sep = "_")
          sample_val <- dbGetQuery(con, sprintf("SELECT %s FROM %s LIMIT 1", col, tbl))[[col]]
          if (is.numeric(sample_val)) {
            vals <- dbGetQuery(con, sprintf("SELECT %s FROM %s WHERE %s IS NOT NULL", col, tbl, col))[[col]]
            default_val <- range(vals, na.rm = TRUE)
            updateSliderInput(session, input_id, value = default_val)
          } else {
            # updateSelectizeInput(session, input_id, selected = "All")
            updateSelectizeInput(session, input_id, selected = character(0))
          }
          updateSwitchInput(session, na_id, value = TRUE, onLabel = "include", offLabel = "exclude")
        }
      }
    })
    
    ##### ========================= "filtered_genes_panel" ========================= #####
    # Render intersected_gene_ids as text in a scrollable panel
    output$filtered_genes_panel <- renderUI({
      ids <- intersected_gene_ids()
      
      if (length(ids) == 0) {
        return(
          tagList(
            tags$h4(paste0("Matching Genes", " (n = ", length(ids), ")")),
            tags$div(
              style = "color: #999; font-style: italic; padding: 1rem;",
              "No matching genes"
            )
          )
        )
      }
      
      mapping <- gene_mapping()
      
      # Join GeneIDs to get symbols
      symbols <- mapping %>%
        dplyr::filter(GeneID %in% ids) %>%
        dplyr::pull(symbol)
      
      tagList(
        # uiOutput(session$ns("active_filters")),
        tags$h4(paste0("Matching Genes", " (n = ", length(ids), ")")),
        tags$div(
          style = "white-space: pre-wrap; background-color: #f7f7f7; padding: 1rem; border: 1px solid #ccc; border-radius: 6px; max-height: 500px; overflow-y: auto;",
          paste(symbols, collapse = ", ")
        )
      )
    })
    
    ##### ========================= "Input custom list" (UI) ========================= #####
    observeEvent(input$list_input, {
      
      # Make named vector of table choices (nice name = label, table = value)
      table_meta <- filter_metadata[!duplicated(filter_metadata$table), c("table", "table_nice_name")]
      table_choices <- setNames(table_meta$table, table_meta$table_nice_name)
      
      showModal(modalDialog(
        title = "Filter custom list (genes, phenotypes etc)",
        fluidPage(
          fluidRow(h5('Select Table and Column to filter')),
          fluidRow(
            column(6,
                   selectInput(session$ns("list_input_table"), "Table:",
                               choices = table_choices,
                               selected = names(table_choices)[1])
            ),
            column(6,
                   uiOutput(session$ns("list_input_column_ui"))
            )
          ),
          hr(),
          fluidRow(h5('Input custom list')),
          fluidRow(
            column(12,
                   textInput(session$ns("list_input_text"), NULL, value = "")
            )
          ),
          hr(),
          fluidRow(h5('Select list delimiter')),
          fluidRow(
            column(12,
                   selectInput(session$ns("list_input_separator"), NULL,
                               choices = c("Semicolon (;)" = ";",
                                           "Comma (,)" = ",",
                                           "Whitespace" = " ",
                                           "Pipe (|)" = "|"),
                               selected = ";")
            )
          ),
          fluidRow(
            column(12,
                   htmlOutput(session$ns("list_input_message"))
            )
          )
        ),
        easyClose = TRUE,
        footer = tagList(
          actionButton(session$ns("apply_list_input"), "Apply", class = "btn-primary"),
          modalButton("Close")
        )
      ))
    })
    
    
    ##### ========================= Update Column Choices based on Table selected ========================= #####
    output$list_input_column_ui <- renderUI({
      req(input$list_input_table)
      tbl <- input$list_input_table
      
      # Filter metadata for the selected table
      col_meta <- filter_metadata[filter_metadata$table == tbl, ]
      
      # Create named vector: column backend ID -> nice name
      col_choices <- setNames(col_meta$column, col_meta$column_nice_name)
      
      selectInput(session$ns("list_input_column"), "Select Column:", choices = col_choices, selected = names(col_choices)[1])
    })
    
    
    ##### ========================= Handle Application of List Input ========================= #####
    observeEvent(input$apply_list_input, {
      req(input$list_input_table, input$list_input_column, input$list_input_text, input$list_input_separator)
      
      tbl <- input$list_input_table
      col <- input$list_input_column
      
      filter_input_id <- paste(tbl, col, sep = "_")
      
      entries <- unlist(strsplit(input$list_input_text, split = input$list_input_separator, fixed = TRUE))
      entries <- trimws(entries)
      entries <- entries[entries != ""]
      
      # Get available distinct values
      available_values <- as.character(dbGetQuery(con, sprintf("SELECT DISTINCT %s FROM %s", col, tbl))[[col]])
      available_values <- available_values[!is.na(available_values)]
      
      # Match input entries to available values
      valid_entries <- entries[entries %in% available_values]
      invalid_entries <- setdiff(entries, valid_entries)
      
      # Update the related selectize input
      updateSelectizeInput(session, filter_input_id,
                           choices = c("All", "Has no data", available_values),
                           selected = valid_entries)
      
      # Clear the text input
      updateTextInput(session, "list_input_text", value = "")
      
      # Build feedback message
      message_parts <- c()
      if (length(valid_entries) > 0) {
        message_parts <- c(message_parts, paste("Matched terms:", paste(valid_entries, collapse = ", ")))
      }
      if (length(invalid_entries) > 0) {
        message_parts <- c(message_parts, paste("The following entries did not match any available terms:",
                                                paste(invalid_entries, collapse = ", ")))
      }
      
      output$list_input_message <- renderUI({
        HTML(paste(message_parts, collapse = "<br>"))
      })
    })
    
    # ##### ========================= View Filters Dynamically ========================= #####
    # output$active_filters <- renderUI({
    #   all_inputs <- reactiveValuesToList(input)
    #   
    #   filter_keys <- names(all_inputs)[sapply(names(all_inputs), function(x) {
    #     any(sapply(individual_tables, function(tbl) startsWith(x, paste0(tbl, "_"))))
    #   })]
    #   
    #   filters <- all_inputs[filter_keys]
    #   
    #   # Filter only non-empty values
    #   filters <- Filter(function(x) {
    #     if (is.null(x)) return(FALSE)
    #     if (is.character(x) || is.numeric(x)) return(length(x) > 0 && !all(x == "")) 
    #     return(TRUE)
    #   }, filters)
    #   
    #   if (length(filters) == 0) {
    #     return(NULL)
    #   }
    #   
    #   # === Group filters by table ===
    #   table_map <- list()
    #   
    #   for (key in names(filters)) {
    #     tbl <- NULL
    #     col <- NULL
    #     
    #     # Find longest matching table name prefix
    #     for (candidate_tbl in individual_tables[order(nchar(individual_tables), decreasing = TRUE)]) {
    #       prefix <- paste0(candidate_tbl, "_")
    #       if (startsWith(key, prefix)) {
    #         tbl <- candidate_tbl
    #         col <- sub(prefix, "", key)
    #         break
    #       }
    #     }
    #     
    #     if (is.null(tbl) || is.null(col)) return(NULL)  # skip if can't parse
    #     val <- filters[[key]]
    #     
    #     # Check if it's a 'na' switch
    #     is_na_filter <- grepl("_na$", key)
    #     col_clean <- sub("_na$", "", col)
    #     
    #     # Get nice names from metadata
    #     nice_tbl <- filter_metadata$table_nice_name[filter_metadata$table == tbl & filter_metadata$column == col_clean]
    #     if (length(nice_tbl) == 0 || is.na(nice_tbl)) nice_tbl <- tbl
    #     
    #     nice_col <- filter_metadata$column_nice_name[filter_metadata$table == tbl & filter_metadata$column == col_clean]
    #     if (length(nice_col) == 0 || is.na(nice_col)) nice_col <- col_clean
    #     
    #     # Skip NA filters unless they are explicitly set to FALSE (i.e. excluding missing data)
    #     if (is_na_filter && isTRUE(filters[[key]])) next
    #     
    #     label <- if (is_na_filter && !isTRUE(filters[[key]])) {
    #       paste(nice_col, "(Include Missing Data)")
    #     } else {
    #       nice_col
    #     }
    #     
    #     value_str <- if (is.logical(val)) {
    #       if (val) "Yes" else "No"
    #     } else if (is.vector(val) && length(val) > 1) {
    #       paste(val, collapse = ", ")
    #     } else {
    #       val
    #     }
    #     
    #     if (!nice_tbl %in% names(table_map)) table_map[[nice_tbl]] <- list()
    #     
    #     table_map[[nice_tbl]][[label]] <- value_str
    #   }
    #   
    #   # === Render grouped filters ===
    #   outer_ui <- lapply(names(table_map), function(tbl_nice_name) {
    #     filters_in_tbl <- table_map[[tbl_nice_name]]
    #     
    #     rows <- mapply(function(k, v) {
    #       tags$tr(
    #         tags$td(style = "padding: 4px; vertical-align: top;", tags$strong(k)),
    #         tags$td(style = "padding: 4px;", v)
    #       )
    #     }, names(filters_in_tbl), filters_in_tbl, SIMPLIFY = FALSE)
    #     
    #     tagList(
    #       tags$h5(tbl_nice_name),
    #       tags$table(
    #         style = "width: 100%; border-collapse: collapse;",
    #         rows
    #       ),
    #       tags$br()
    #     )
    #   })
    #   
    #   tagList(
    #     tags$div(
    #       style = "padding: 10px; border: 1px solid #ddd; background-color: #f9f9f9; margin-bottom: 10px; border-radius: 6px;",
    #       tags$h5("Active Filters"),
    #       outer_ui
    #     )
    #   )
    # })
    
    output$query_page_info <- renderUI({
      tagList(
        tags$h4("Query Page Information"),
        tags$p("• 'Save Gene List' Save your resulting gene list."),
        tags$p("• 'Filters' Combine multiple filters to intersect datasets."),
        tags$p("• 'Input Custom list' Allows for any list of values to be uploaded e.g. gene or phenotype list."),
        tags$p("• 'Matching Genes' Displays all genes resulting from the user query."),
        tags$p("• Hover over info icons for descriptions of each filter.")
      )
    })    
  })
}
