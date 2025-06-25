# app.R

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

# options(shiny.host = "0.0.0.0")
# options(shiny.port = 8180)

# Source Modules
source("./modules/home_module.R")
source("./modules/query_module.R")
source("./modules/download_module.R")
query <- "query_mod"
home <- "home_mod"
download <- "download"

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
desired_order <- c(
  "Human Phenotypes",
  "Mouse Assays",
  "Gene Identifiers",
  "Gene Group",
  "Alias & Previous Gene Symbols",
  "Genomic Sequence Info",
  "Gene Constraint Metrics",
  "Expression",
  "FUSIL",
  "Human Cellular Assays",
  "Protein Classification",
  "Protein-Protein Interactions",
  "Functional Annotations",
  "Comparitive Genomics"
)
class_table_map <- class_table_map[desired_order]

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
  navbar_options = navbar_options(
    bg    = "#063970",  # bootstrap class bg-primary
    theme = "dark"      # makes text/icons light
  ),
  ##### ========================= Custom CSS ========================= #####
  tags$head(
    tags$link(rel = "stylesheet", type = "text/css", href = "styles.css")
  ),
  ##### ========================= Home Page ========================= #####
  nav_panel("Home",
            homeModuleUI(home)
  ),
  ##### ========================= Query Page ========================= #####
  nav_panel("Query",
            queryModuleUI(query, individual_tables, filter_metadata, class_table_map)
  ),
  ##### ========================= Download Page ========================= #####
  nav_panel("Download",
            downloadModuleUI(download)
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
  homeModuleServer(home)
  downloadModuleServer(download, con)
}

shinyApp(ui, server)
