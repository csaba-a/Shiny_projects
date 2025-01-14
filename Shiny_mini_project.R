# R Shiny Dashboard for Healthcare Data Analysis
# Dataset: Kaggle Public Health Dataset (public dataset)

library(shiny)
library(shinydashboard)
library(ggplot2)
library(dplyr)
library(plotly)
library(DT)
library(tidyr)
library(viridis)
library(shinycssloaders)

# Load Data (Kaggle Public Health Dataset example dataset)
health_data <- read.csv('path/to/kaggle/public_health_dataset.csv')

# UI
ui <- dashboardPage(
  dashboardHeader(title = "Healthcare Data Dashboard"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Overview", tabName = "overview", icon = icon("dashboard")),
      menuItem("Data Explorer", tabName = "data_explorer", icon = icon("table")),
      menuItem("Visualizations", tabName = "visualizations", icon = icon("chart-bar")),
      menuItem("Map", tabName = "map", icon = icon("map-marker-alt"))
    )
  ),
  dashboardBody(
    tabItems(
      tabItem(tabName = "overview",
              fluidRow(
                box(title = "Dataset Info", width = 6, solidHeader = TRUE, status = "primary",
                    verbatimTextOutput("dataset_info")),
                box(title = "Quick Summary", width = 6, solidHeader = TRUE, status = "primary",
                    verbatimTextOutput("summary_info"))
              )
      ),
      tabItem(tabName = "data_explorer",
              fluidRow(
                box(title = "Data Table", width = 12, status = "info",
                    withSpinner(DTOutput("data_table")))
              )
      ),
      tabItem(tabName = "visualizations",
              fluidRow(
                box(title = "Select State", width = 4, selectInput("state", "State:", choices = unique(health_data$LocationAbbr))),
                box(title = "Select Condition", width = 4, selectInput("condition", "Condition:", choices = unique(health_data$Topic)))
              ),
              fluidRow(
                box(title = "Trend Over Time", width = 12, withSpinner(plotlyOutput("trend_plot")))
              )
      ),
      tabItem(tabName = "map",
              fluidRow(
                box(title = "State Map", width = 12, withSpinner(plotlyOutput("state_map")))
              )
      )
    )
  )
)

# SERVER
server <- function(input, output) {
  
  output$dataset_info <- renderPrint({
    str(health_data)
  })
  
  output$summary_info <- renderPrint({
    summary(health_data)
  })
  
  output$data_table <- renderDT({
    datatable(health_data)
  })
  
  output$trend_plot <- renderPlotly({
    filtered_data <- health_data %>%
      filter(LocationAbbr == input$state, Topic == input$condition) %>%
      group_by(YearStart) %>%
      summarise(mean_value = mean(DataValue, na.rm = TRUE))
    
    p <- ggplot(filtered_data, aes(x = YearStart, y = mean_value)) +
      geom_line(color = "blue") +
      labs(title = "Healthcare Trends", x = "Year", y = "Value") +
      theme_minimal()
    
    ggplotly(p)
  })
  
  output$state_map <- renderPlotly({
    state_data <- health_data %>%
      filter(LocationAbbr == input$state) %>%
      group_by(Topic) %>%
      summarise(mean_value = mean(DataValue, na.rm = TRUE))
    
    p <- ggplot(state_data, aes(x = Topic, y = mean_value, fill = mean_value)) +
      geom_col() +
      scale_fill_viridis_c() +
      coord_flip() +
      labs(title = "State Health Overview", x = "Condition", y = "Average Value") +
      theme_minimal()
    
    ggplotly(p)
  })
}

# Run the application 
shinyApp(ui = ui, server = server)
