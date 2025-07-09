# app.R

library(shiny)
library(DT)
library(plotly)
library(dplyr)
library(tidyr)
library(ggplot2)
library(rlang)

# source helper functions
source("R/api_helpers.R")    # API query functions
source("R/summaries.R")      # summary & plot helpers

ui <- navbarPage("AIC Explorer",
                 
                 ## 1) About tab
                 tabPanel("About",
                          fluidRow(
                            column(4, img(src = "cover.jpg", height = "150px")),
                            column(8,
                                   h3("AIC Explorer"),
                                   p("Query the Art Institute of Chicago API for artworks, artists, or exhibitions."),
                                   p("Download raw data or explore prebuilt summaries and plots."),
                                   tags$a(href = "https://api.artic.edu/docs/", "API documentation")
                            )
                          ),
                          hr(),
                          h4("Tabs"),
                          tags$ul(
                            tags$li(strong("Data Download:"), "Fetch raw data and save as CSV."),
                            tags$li(strong("Data Exploration:"), "View extended summaries and interactive plots.")
                          )
                 ),
                 
                 ## 2) Data Download tab
                 tabPanel("Data Download",
                          sidebarLayout(
                            sidebarPanel(
                              radioButtons("entity","Choose data to fetch:",
                                           c("Artworks","Artists","Exhibitions")),
                              conditionalPanel("input.entity=='Artworks'",        
                                               textInput("art_id","Artwork ID (blank for list):",""),
                                               numericInput("art_page","Page:",1,1),
                                               numericInput("art_limit","Limit:",50,1,500),
                                               textInput("art_fields","Fields (comma sep):",
                                                         "id,title,artist_title,date_display,classification_titles,dimensions,image_id")
                              ),
                              conditionalPanel("input.entity=='Artists'",
                                               textInput("artist_id","Artist ID:",""),
                                               numericInput("artist_page","Page:",1,1),
                                               numericInput("artist_limit","Limit:",50,1,500),
                                               textInput("artist_fields","Fields:",
                                                         "id,title,birth_date,death_date,nationality")
                              ),
                              conditionalPanel("input.entity=='Exhibitions'",
                                               numericInput("exh_page","Page:",1,1),
                                               numericInput("exh_limit","Limit:",50,1),
                                               textInput("exh_fields","Fields:",
                                                         "id,title,date_start,date_end,place")
                              ),
                              actionButton("fetch","Fetch Data"), hr(),
                              uiOutput("col_selector"),
                              uiOutput("row_selector"),
                              downloadButton("download","Download CSV")
                            ),
                            mainPanel(
                              DTOutput("raw_table")
                            )
                          )
                 ),
                 
                 ## 3) Data Exploration tab (Updated)
                 tabPanel("Data Exploration",
                          sidebarLayout(
                            sidebarPanel(
                              selectInput("summary_type","Choose summary:",
                                          choices = c(
                                            "Contingency by Classification"       = "contingency_classification",
                                            "Year Summary by Classification"      = "numeric_summary_year_by_class",
                                            "Dimension Summary by Classification" = "numeric_summary_dimensions",
                                            "Summary by Decade"                   = "numeric_summary_by_decade"
                                          )
                              ),
                              selectInput("plot_type","Choose plot:",
                                          choices = c(
                                            "Bar Count by Classification" = "plot_count_by_class",
                                            "Year Boxplot by Classification" = "plot_year_boxplot",
                                            "Heatmap Year vs Classification" = "plot_heatmap_year_class",
                                            "Year Histogram" = "plot_year_histogram"
                                          )
                              )
                            ),
                            mainPanel(
                              tableOutput("summary_tbl"),
                              plotlyOutput("plotly_obj")
                            )
                          )
                 )
)

server <- function(input, output, session) {
  # Reactive: fetch raw data
  raw_data <- eventReactive(input$fetch, {
    req(input$fetch > 0)
    switch(input$entity,
           "Artworks" = {
             f <- strsplit(input$art_fields, ",")[[1]]
             if (nzchar(input$art_id)) get_artwork_details(input$art_id, fields = f)
             else get_artworks(page = input$art_page, limit = input$art_limit, fields = f)
           },
           "Artists" = {
             f <- strsplit(input$artist_fields, ",")[[1]]
             if (nzchar(input$artist_id)) get_artist_details(input$artist_id, fields = f)
             else get_artists(page = input$artist_page, limit = input$artist_limit, fields = f)
           },
           "Exhibitions" = {
             f <- strsplit(input$exh_fields, ",")[[1]]
             get_exhibitions(page = input$exh_page, limit = input$exh_limit, fields = f)
           }
    )
  })
  
  # Data Download logic
  output$col_selector <- renderUI({ df <- raw_data(); req(df)
  selectInput("cols","Columns:", names(df), names(df), multiple = TRUE)
  })
  output$row_selector <- renderUI({ df <- raw_data(); req(df)
  sliderInput("row_sel","Rows:", 1, nrow(df), value = c(1, min(100, nrow(df))))
  })
  output$raw_table <- renderDT({ df <- raw_data(); req(df)
  df[input$row_sel[1]:input$row_sel[2], input$cols, drop = FALSE]
  })
  output$download <- downloadHandler(
    filename = function() paste0(input$entity, ".csv"),
    content = function(file) { df <- raw_data(); write.csv(df[input$row_sel[1]:input$row_sel[2], input$cols, drop = FALSE], file, row.names = FALSE) }
  )
  
  # Data Exploration logic
  summary_tbl <- reactive({ match.fun(input$summary_type)(raw_data()) })
  output$summary_tbl <- renderTable({ summary_tbl() })
  
  output$plotly_obj <- renderPlotly({ p <- match.fun(input$plot_type)(raw_data()); ggplotly(p) })
}

# Run the app
shinyApp(ui = ui, server = server)
