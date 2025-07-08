# app.R

library(shiny)
library(DT)
library(plotly)
library(dplyr)
library(tidyr)
library(ggplot2)

# source your helper functions
# source("R/api_helpers.R")    # get_artworks(), get_artwork_details(), etc.
# source("R/summaries.R")      # contingency... and plot helpers

source("api_helpers.R")    # get_artworks(), get_artwork_details(), etc.
source("summaries.R")      # contingency... and plot helpers


ui <- navbarPage("AIC Explorer",
                 
                 ## 1) About tab
                 tabPanel("About",
                          fluidRow(
                            column(4,
                                   img(src = "artic_logo.png", height = "150px")
                            ),
                            column(8,
                                   h3("AIC Explorer"),
                                   p("This Shiny app lets you query the Art Institute of Chicago API,"),
                                   p("return tidy data frames of artworks, artists, or exhibitions,"),
                                   p("and explore them via tables, summaries, and interactive plots."),
                                   tags$a(href = "https://api.artic.edu/docs/", "API documentation")
                            )
                          ),
                          hr(),
                          h4("Tabs"),
                          tags$ul(
                            tags$li(strong("Data Download:"), "Fetch raw data, subset rows & columns, and download as CSV."),
                            tags$li(strong("Data Exploration:"), "Build contingency tables, numeric summaries, and four types of plots (including a heatmap).")
                          )
                 ),
                 
                 ## 2) Data Download tab
                 tabPanel("Data Download",
                          sidebarLayout(
                            sidebarPanel(
                              radioButtons("entity", "Choose data to fetch:",
                                           choices = c("Artworks", "Artists", "Exhibitions")),
                              conditionalPanel(
                                condition = "input.entity == 'Artworks'",
                                textInput("art_id", "Optional artwork ID (leave blank for list):", ""),
                                numericInput("art_page", "Page:", 1, min = 1),
                                numericInput("art_limit", "Limit:", 50, min = 1, max = 1000),
                                textInput("art_fields", "Fields (comma sep):", 
                                          "id,title,artist_title,date_display,classification_titles,image_id")
                              ),
                              conditionalPanel(
                                condition = "input.entity == 'Artists'",
                                textInput("artist_id", "Optional artist ID (blank for list):", ""),
                                numericInput("artist_page", "Page:", 1, min = 1),
                                numericInput("artist_limit", "Limit:", 50, min = 1, max = 1000),
                                textInput("artist_fields", "Fields (comma sep):",
                                          "id,title,birth_date,death_date,nationality")
                              ),
                              conditionalPanel(
                                condition = "input.entity == 'Exhibitions'",
                                numericInput("exh_page", "Page:", 1, min = 1),
                                numericInput("exh_limit", "Limit:", 50, min = 1, max = 1000),
                                textInput("exh_fields", "Fields (comma sep):",
                                          "id,title,date_start,date_end,place")
                              ),
                              actionButton("fetch", "Fetch Data"),
                              hr(),
                              uiOutput("col_selector"),     # dynamic: choose which columns to keep
                              sliderInput("row_sel", "Rows to keep (1 to N):", 1, 1, value = c(1,1)),
                              downloadButton("download", "Download CSV")
                            ),
                            mainPanel(
                              DTOutput("raw_table")
                            )
                          )
                 ),
                 
                 ## 3) Data Exploration tab
                 tabPanel("Data Exploration",
                          sidebarLayout(
                            sidebarPanel(
                              uiOutput("dataset_selector"), # dynamic: pick which fetched dataset
                              uiOutput("cat_selector"),     # dynamic: choose categorical var
                              uiOutput("num_selector"),     # dynamic: choose quantitative var
                              selectInput("plot_type", "Plot type:",
                                          choices = c("Bar","Boxplot","Heatmap","Scatter")),
                              uiOutput("facet_selector")    # dynamic: only shows for heatmap/scatter
                            ),
                            mainPanel(
                              tabsetPanel(
                                tabPanel("Contingency", tableOutput("ctable")),
                                tabPanel("Summary", tableOutput("num_summary")),
                                tabPanel("Plot", plotlyOutput("plotly"))
                              )
                            )
                          )
                 )
)

server <- function(input, output, session) {
  
  # 1) Fetch raw data
  raw_data <- eventReactive(input$fetch, {
    switch(input$entity,
           "Artworks"    = {
             flds <- strsplit(input$art_fields, ",")[[1]]
             if (nzchar(input$art_id)) {
               get_artwork_details(input$art_id, fields = flds)
             } else {
               get_artworks(
                 page   = input$art_page,
                 limit  = input$art_limit,
                 fields = flds
               )
             }
           },
           "Artists"     = {
             flds <- strsplit(input$artist_fields, ",")[[1]]
             if (nzchar(input$artist_id)) {
               get_artist_details(input$artist_id, fields = flds)
             } else {
               get_artists(
                 page   = input$artist_page,
                 limit  = input$artist_limit,
                 fields = flds
               )
             }
           },
           "Exhibitions" = {
             flds <- strsplit(input$exh_fields, ",")[[1]]
             get_exhibitions(
               page   = input$exh_page,
               limit  = input$exh_limit,
               fields = flds
             )
           }
    )
  }, ignoreNULL = FALSE)
  
  # 2) Dynamic column selector & row slider
  output$col_selector <- renderUI({
    df <- raw_data()
    req(df)
    cols <- names(df)
    selectInput("cols", "Select columns to keep:", choices = cols,
                selected = cols, multiple = TRUE)
  })
  observeEvent(raw_data(), {
    df <- raw_data()
    updateSliderInput(session, "row_sel",
                      max = nrow(df), value = c(1, min(100, nrow(df)))
    )
  })
  
  # 3) Render raw table subset
  output$raw_table <- renderDT({
    df <- raw_data()
    req(df)
    df2 <- df[input$row_sel[1]:input$row_sel[2], input$cols, drop = FALSE]
    datatable(df2, options = list(pageLength = 10))
  })
  
  # 4) Download handler
  output$download <- downloadHandler(
    filename = function() {
      paste0(input$entity, "_data.csv")
    },
    content = function(file) {
      df <- raw_data()
      df2 <- df[input$row_sel[1]:input$row_sel[2], input$cols, drop = FALSE]
      write.csv(df2, file, row.names = FALSE)
    }
  )
  
  # 5) Dataset selector for exploration
  output$dataset_selector <- renderUI({
    req(raw_data())
    selectInput("which_ds", "Data for exploration:",
                choices = c("Raw Data" = "raw"), selected = "raw")
  })
  ds <- reactive({
    if (input$which_ds == "raw") raw_data()
    # (could add precomputed summaries here)
  })
  
  # 6) Dynamic var selectors
  output$cat_selector <- renderUI({
    df <- ds()
    req(df)
    nums <- vapply(df, is.numeric, logical(1))
    cats <- names(df)[!nums]
    selectInput("cat_var", "Categorical variable:", choices = cats)
  })
  output$num_selector <- renderUI({
    df <- ds()
    req(df)
    nums <- vapply(df, is.numeric, logical(1))
    selectInput("num_var", "Numeric variable:", choices = names(df)[nums])
  })
  
  # 7) Facet selector (only for heatmap or scatter)
  output$facet_selector <- renderUI({
    req(input$plot_type)
    if (input$plot_type %in% c("Heatmap", "Scatter")) {
      df <- ds()
      req(df)
      nums <- vapply(df, is.numeric, logical(1))
      cats <- names(df)[!nums]
      selectInput("facet_var", "Facet by (categorical):", choices = cats)
    }
  })
  
  # 8) Contingency & summary tables
  output$ctable <- renderTable({
    df <- ds()
    req(df, input$cat_var)
    df %>% 
      count(!!sym(input$cat_var)) %>%
      rename(Count = n)
  })
  output$num_summary <- renderTable({
    df <- ds()
    req(df, input$cat_var, input$num_var)
    df %>%
      group_by(!!sym(input$cat_var)) %>%
      summarize(
        n      = n(),
        mean   = mean(!!sym(input$num_var), na.rm = TRUE),
        median = median(!!sym(input$num_var), na.rm = TRUE),
        sd     = sd(!!sym(input$num_var), na.rm = TRUE)
      )
  })
  
  # 9) Plotly plot
  output$plotly <- renderPlotly({
    df <- ds()
    req(df, input$cat_var, input$num_var, input$plot_type)
    
    p <- switch(input$plot_type,
                "Bar"      = {
                  df %>% count(!!sym(input$cat_var)) %>%
                    ggplot(aes_string(x = input$cat_var, y = "n", fill = input$cat_var)) +
                    geom_col() + theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
                    labs(y = "Count", title = "Bar Chart")
                },
                "Boxplot"  = {
                  ggplot(df, aes_string(x = input$cat_var, y = input$num_var, fill = input$cat_var)) +
                    geom_boxplot() + theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
                    labs(title = "Boxplot")
                },
                "Heatmap"  = {
                  req(input$facet_var)
                  df2 <- df %>%
                    unnest_longer(input$facet_var, values_to = "facet_val") %>%
                    count(!!sym(input$facet_var), !!sym(input$cat_var))
                  ggplot(df2, aes_string(x = input$cat_var, y = input$facet_var, fill = "n")) +
                    geom_tile() +
                    labs(title = "Heatmap", fill = "Count")
                },
                "Scatter" = {
                  req(input$facet_var)
                  ggplot(df, aes_string(x = input$num_var, y = input$cat_var, color = input$facet_var)) +
                    geom_point(alpha = 0.7) +
                    labs(title = "Scatter Plot")
                }
    )
    
    ggplotly(p)
  })
  
}

shinyApp(ui, server)


shiny::runGitHub(
  repo     = "Project2_API",
  username = "mhongji"
)

