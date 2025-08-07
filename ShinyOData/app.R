library(shiny)
library(ggplot2)
library(dplyr)
library(readr)
library(scales)

# 📂 Load list of datasets
datasets <- readRDS(here::here("data", "cbs_datasets_clean.rds"))
dataset_lookup <- c(
  "Bestande koopindex"     = "koop_index",
  "Nieuwe en bestaande"    = "koop_nieuwbestaand",
  "Waarde onroerend goed"  = "koop_onroerend",
  "Koopprijzen per COROP"  = "koop_regio_corop",
  "Koopprijzen per regio"  = "koop_regio_prijzen",
  "Regio (koop)"           = "koop_regio",
  "Type koopwoningen"      = "koop_type",
  "Uitgaven en bezit"      = "koop_uitgavenbezit"
)

ui <- fluidPage(
  titlePanel("Interactieve CBS woningmarkt visualisatie"),
  
  fluidRow(
    column(4,
           selectInput(
             inputId = "dataset_name",
             label = "Kies een dataset:",
             choices = dataset_lookup,
             selected = names(dataset_lookup)[1]
           ),
           uiOutput("var_selector"),
           uiOutput("periode_selector"),
           uiOutput("facet_selector"),
    )
  ),
  
  fluidRow(
    column(12,
           plotOutput("tijdgrafiek")
    )
  )
)

server <- function(input, output, session) {
  
  # ⛏️ Reactieve selectie van het juiste dataframe
  selected_data <- reactive({
    req(input$dataset_name)
    datasets[[input$dataset_name]]
  })
  
  # 📊 UI: Selecteer een numerieke variabele
  output$var_selector <- renderUI({
    df <- selected_data()
    num_vars <- names(df)[sapply(df, is.numeric)]
    selectInput("variabele", "Kies een variabele:", choices = num_vars)
  })
  
  # 📆 UI: Selecteer PeriodeType
  output$periode_selector <- renderUI({
    df <- selected_data()
    if (!"PeriodeType" %in% names(df)) return(NULL)
    selectInput("periode_type", "Kies type periode:", choices = unique(df$PeriodeType))
  })
  
  output$facet_selector <- renderUI({
    df <- selected_data()
    if (ncol(df) < 2) return(NULL)
    
    second_col <- names(df)[2]
    values <- unique(df[[2]])
    
    selectInput(
      "facet_values",
      label = paste0("Kies facetten uit: ", second_col),
      choices = values,
      selected = NULL,
      multiple = TRUE
    )
  })
  
  # 📈 Plot
  output$tijdgrafiek <- renderPlot({
    df <- selected_data()
    req(input$variabele, input$periode_type)
    
    filtered <- df %>%
      filter(PeriodeType == input$periode_type)
    
    # Optional: filter on user-selected facets
    if (!is.null(input$facet_values) && length(input$facet_values) > 0) {
      facet_col <- names(df)[2]
      filtered <- filtered %>%
        filter(.data[[facet_col]] %in% input$facet_values)
    }
    
    p <- ggplot(filtered, aes_string(x = "TijdLabel", y = input$variabele)) +
      geom_line(group = 1, color = "#0072B2") +
      geom_point(color = "#D55E00") +
      scale_y_continuous(labels = label_comma(big.mark = ".")) +
      labs(
        title = paste("Ontwikkeling van", input$variabele),
        x = "Periode",
        y = input$variabele
      ) +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
    
    # Add facets only if selection > 1
    if (!is.null(input$facet_values) && length(input$facet_values) > 1) {
      facet_col <- names(df)[2]
      p <- p + facet_wrap(as.formula(paste("~", facet_col)))
    }
    
    p
  })
}

# 🚀 Start de app
shinyApp(ui = ui, server = server)