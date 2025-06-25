# home_module.R

# Module UI function
homeModuleUI <- function(id) {
  ns <- NS(id)
  tagList(
    uiOutput(ns("query_page_info"))
  )
}

# Module Server function
homeModuleServer <- function(id) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    output$query_page_info <- renderUI({
      tagList(
        # Centered, bold, dark-royal-blue title
        tags$h3(
          "Welcome to the Essential Genes Explorer (EGEx)",
          style = "text-align: center; margin-bottom: 1rem; font-weight: 700; color: #467EB2;"
        ),
        
        # About text on the left (4 cols) and EGEx graphic on the right (8 cols)
        fluidRow(
          column(
            width = 4,
            tags$h5(tags$b("Overview"), style = "text-align: left; margin-bottom: 1rem; font-weight: 700; color: #467EB2;"),
            tags$p(
              "The Essential Genes Explorer (EGEx) app is a comprehensive and user-oriented database of human essential genes that integrates multiple sources of experimental evidence at different organisational levels as well as curated phenotypes.  
              EGEx supports flexible querying and visualisation of essentiality as a spectrum of gene intolerance to variation rather than a binary state, allowing for a more refined analysis of gene function and constraint.  
              EGEx goes beyond data aggregation: it offers an integrated analysis environment featuring customised gene lists, enrichment analysis testing, exploratory plots, and exportable figures to support hypothesis generation and downstream reporting.",
              style = "text-align: justify;"
            ),
            tags$h5(tags$b("Features"), style = "text-align: left; margin-bottom: 1rem; font-weight: 700; color: #467EB2;"),
            tags$p(tags$b("Query:"), "Query the EGEx database. Utilise the comprehensive set of filters to find matching genes and save them.",  style = "text-align: justify;"),
            tags$p(tags$b("Enrichment analysis:"), "Peform Odds Ratio Analysis using saved gene lists. Optionally, define a custom set of background genes to test against.",  style = "text-align: justify;"),
            tags$p(tags$b("Visualise:"), "Visualise, compare and constrast different datasets and gene lists.",  style = "text-align: justify;"),
            tags$h5(tags$b("Related publications"), style = "text-align: left; margin-bottom: 1rem; font-weight: 700; color: #467EB2;"),
            tags$ul(
              tags$li(
                HTML("<a href='https://doi.org/10.1101/2025.01.28.25321201' target='_blank'>The spectrum of gene intolerance to variation: Insights from a rare disease cohort.</a> medRxiv 2025.")
              ),
              tags$li(
                HTML("<a href='https://doi.org/10.1101/2024.01.12.24301168' target='_blank'>Lethal phenotypes in Mendelian disorders.</a> medRxiv 2024.")
              ),
              tags$li(
                HTML("<a href='https://doi.org/10.1007/s00335-023-09984-1' target='_blank'>Essential genes: a cross-species perspective.</a> Mamm Genome 2023.")
              ),
              tags$li(
                HTML("<a href='https://doi.org/10.1186/s13073-022-01118-7' target='_blank'>Mendelian gene identification through mouse embryo viability screening.</a> Genome Med 2022.")
              ),
              tags$li(
                HTML("<a href='https://doi.org/10.1038/s41467-020-14284-2' target='_blank'>Human and mouse essentiality screens as a resource for disease gene discovery.</a> Nat Commun 2020.")
              )
            )
          ),
          column(
            width = 8,
            tags$div(
              style = "text-align: center;",
              tags$img(
                src    = "EGEx-graphic-June25.png",
                alt    = "EGEx graphic",
                style  = "max-width: 100%; height: auto;"
              )
            )
          )
        ),
        tags$hr(),
        
        # Centered disclaimer
        tags$div(
          style = "text-align: left;",
          tags$p(tags$b("Disclaimer:"), "The app and the information contained within it is intended for research purposes only. Neither the site contributors nor the site hosts accept any responsibility for any loss or damage resulting from reliance on the information provided."),
        ),
        # Centered QMUL link
        tags$a(
          href   = "https://www.qmul.ac.uk",
          "Developed by Queen Mary University of London (qmul.ac.uk)",
          target = "_blank",
          style  = "display: block; text-align: center; margin-bottom: 1rem;"
        ),
        
        # Centered QMUL logo
        tags$div(
          style = "text-align: center;",
          tags$img(
            src   = "QMUL-logo.jpg",
            alt   = "QMUL logo",
            style = "max-width: 200px; height: auto;"
          )
        )
      )
    })
  })
}