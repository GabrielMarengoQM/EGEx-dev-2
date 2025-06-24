##### ========================= Libraries ========================= #####
library(shiny)
library(bslib)
library(bsicons)
library(duckdb)
library(DBI)
library(DT)
library(plotly)
library(dplyr)
library(arrow)
library(zip)
library(UpSetR)
library(shinyWidgets)
library(shinycssloaders)
library(tidyr)
library(openxlsx)

options(shiny.host = "0.0.0.0")
options(shiny.port = 8180)

# Source Modules
source("./modules/query_module.R")
query <- "query_mod"
# source("./modules/goORA_module.R")
# goORA <- "goORA"
# source("./modules/reactomeORA_module.R")
# reactomeORA <- "reactomeORA"
# source("./modules/oddsRatio_module.R")
# oddsRatio <- "oddsRatio"
# source("./modules/downloads_module.R")
# downloads <- "downloads"


##### ========================= duckDB connect and access Tables ========================= #####
con <- dbConnect(duckdb(), "EGEx-db.duckdb")
all_tables <- dbListTables(con)
individual_tables <- setdiff(all_tables, "db_metadata")

# db-metadata
filter_metadata <- dbReadTable(con, "db_metadata")
# Get list of tables for each class
class_table_map <- split(filter_metadata$table, filter_metadata$class)
# Remove duplicated tables within each class
class_table_map <- lapply(class_table_map, unique)

##### ========================= User saved gene lists ========================= #####
# saved_gene_lists <- reactiveValues(data = list())

######################################################################
####### ========================= UI ========================= #######
######################################################################
ui <- page_navbar(
  id = "main_nav",                # Set ID for programmatic updates
  title = "EGEx",
  padding = "0.4rem",
  theme = bs_theme(bootswatch = "cosmo"),
  ##### ========================= Custom CSS ========================= #####
  tags$head(
    tags$link(rel = "stylesheet", type = "text/css", href = "styles.css")
  ),
  ##### ========================= Explore Data Page ========================= #####
  nav_panel("Explore Data",
            queryModuleUI(query, individual_tables, filter_metadata, class_table_map)
  )
  
)

######################################################################
##### ========================= Server ========================= #####
######################################################################
server <- function(input, output, session) {
  # Initialize per-session saved gene list
  saved_gene_lists <- reactiveValues(data = list())
  # Call module server functions
  queryModuleServer(query,  con, individual_tables, saved_gene_lists, filter_metadata, class_table_map)
}

shinyApp(ui, server)
