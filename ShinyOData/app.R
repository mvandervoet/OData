library(shiny)
library(ggplot2)
library(dplyr)
library(readr)
library(scales)

# 📂 Load list of datasets
datasets <- readRDS("data/cbs_datasets_clean.rds")
# 🧹 Remove ID column from all dataframes (if it exists)
datasets <- lapply(datasets, function(df) {
  if ("ID" %in% names(df)) df <- df %>% select(-ID)
  df
})
dataset_lookup <- c(
  "Prijsindex, aantal verkopen, ontwikkelingen en gemiddelde verkoopprijzen van bestaande koopwoningen in Nederland"     = "koop_index",
  "Bestaande koopwoningen, prijsindex 2020=100, prijsontwikkeling verkochte bestaande koopwoningen gemiddelde verkoopprijs, woningtype"      = "koop_type",
  "Prijsindex koopwoningen, prijsindex 2020=100, prijsontwikkeling Verkochte koopwoningen gemiddelde verkoopprijs"           = "koop_regio",
  "Prijsindex koopwoningen, prijsindex 2020=100, prijsontwikkeling Verkochte koopwoningen gemiddelde verkoopprijs"  = "koop_regio_corop",
  "Gemiddelde verkoopprijzen van bestaande koopwoningen Nederland, landsdelen, provincies en gemeenten"  = "koop_regio_prijzen",
  "Gemiddelde WOZwaarde van woningen verblijfsobjecten met een woonfunctie, eigendom, regio"    = "koop_woz",
  "Prijsindex, verkrijgen en bezitten koopwoning uitgaven eigenaar verwerven en bezitten nieuwe koopwoning"      = "koop_uitgavenbezit",
  "Prijsindex koopwoningen, prijsindex 2020=100, prijsontwikkeling Verkochte koopwoningen, gemiddelde verkoopprijs, aantal"    = "koop_nieuwbestaand",
  "Waarde onroerende zaken van woningen en nietwoningen. Naar Landsdeel, Provincie, COROP, Gemeente."  = "koop_onroerend"
)

ui <- fluidPage(
  titlePanel("Interactieve CBS woningmarkt visualisatie"),
  
  fluidRow(
    column(6,
           selectInput(
             inputId = "dataset_name",
             label = "Kies een dataset:",
             choices = dataset_lookup,
             width = "100%",
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
    selectInput("variabele", "Kies een grafiek:", choices = num_vars, width = "100%")
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
    
    second_col <- names(df)[1]
    values <- unique(df[[1]])
    
    selectInput(
      "facet_values",
      label = paste0("Filter data: ", second_col),
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
      facet_col <- names(df)[1]
      filtered <- filtered %>%
        filter(.data[[facet_col]] %in% input$facet_values)
    }
    
    facet_col <- names(df)[1]
    n_unique <- length(unique(filtered[[facet_col]]))
    n_rows <- nrow(filtered)
    
    if (n_unique < n_rows) {
      p <- ggplot(filtered, aes_string(x = "TijdLabel", y = input$variabele, color = facet_col, group = facet_col)) +
        geom_line() +
        geom_point() +
        scale_y_continuous(labels = label_comma(big.mark = ".")) +
        labs(
          title = paste("Ontwikkeling van", input$variabele),
          x = "Periode",
          y = input$variabele
        ) +
        theme_minimal() +
        theme(axis.text.x = element_text(angle = 45, hjust = 1))
      
      # # Add facets only if selection > 1
      # if (!is.null(input$facet_values) && length(input$facet_values) > 1) {
      #   facet_col <- names(df)[1]
      #   p <- p + facet_wrap(as.formula(paste("~", facet_col)))
      # }
      
      p
    } else {
      p <- ggplot(filtered, aes_string(x = "TijdLabel", y = input$variabele, color = facet_col, group = facet_col)) +
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
      
      # # Add facets only if selection > 1
      # if (!is.null(input$facet_values) && length(input$facet_values) > 1) {
      #   facet_col <- names(df)[1]
      #   p <- p + facet_wrap(as.formula(paste("~", facet_col)))
      # }
      
      p
    }
    
    
    
  })
}

# 🚀 Start de app
shinyApp(ui = ui, server = server)