library(shiny)
library(shinydashboard)
library(ggplot2)
library(dplyr)
library(plotly)

# Simulated epilepsy data
set.seed(123)
epilepsy_data <- data.frame(
  patient_id = 1:1000,
  age = sample(18:80, 1000, replace = TRUE),
  gender = sample(c("Male", "Female"), 1000, replace = TRUE),
  seizure_frequency = sample(0:30, 1000, replace = TRUE),
  medication = sample(c("A", "B", "C", "D"), 1000, replace = TRUE),
  comorbidity = sample(c("None", "Depression", "Anxiety", "Migraine"), 1000, replace = TRUE),
  last_seizure_date = as.Date("2025-01-14") - sample(1:365, 1000, replace = TRUE)
)

ui <- dashboardPage(
  dashboardHeader(title = "Epilepsy Dashboard"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Overview", tabName = "overview", icon = icon("dashboard")),
      menuItem("Patient Data", tabName = "patient_data", icon = icon("user")),
      menuItem("Seizure Analysis", tabName = "seizure_analysis", icon = icon("chart-line"))
    )
  ),
  dashboardBody(
    tabItems(
      tabItem(tabName = "overview",
              fluidRow(
                infoBoxOutput("total_patients_box"),
                infoBoxOutput("avg_seizure_freq_box"),
                infoBoxOutput("most_common_comorbidity_box")
              ),
              fluidRow(
                box(plotlyOutput("age_distribution_plot"), width = 6),
                box(plotlyOutput("medication_distribution_plot"), width = 6)
              )
      ),
      tabItem(tabName = "patient_data",
              fluidRow(
                box(
                  title = "Patient Filters",
                  selectInput("gender_filter", "Gender:", c("All", "Male", "Female")),
                  sliderInput("age_filter", "Age Range:", min = 18, max = 80, value = c(18, 80)),
                  selectInput("medication_filter", "Medication:", c("All", "A", "B", "C", "D"))
                ),
                box(
                  title = "Patient Data Table",
                  DT::dataTableOutput("patient_table")
                )
              )
      ),
      tabItem(tabName = "seizure_analysis",
              fluidRow(
                box(
                  title = "Seizure Frequency Analysis",
                  plotlyOutput("seizure_freq_plot")
                ),
                box(
                  title = "Last Seizure Date Distribution",
                  plotlyOutput("last_seizure_plot")
                )
              ),
              fluidRow(
                box(
                  title = "Comorbidity vs Seizure Frequency",
                  plotlyOutput("comorbidity_plot")
                )
              )
      )
    )
  )
)

server <- function(input, output) {
  output$total_patients_box <- renderInfoBox({
    infoBox(
      "Total Patients", nrow(epilepsy_data), icon = icon("users"),
      color = "blue"
    )
  })
  
  output$avg_seizure_freq_box <- renderInfoBox({
    infoBox(
      "Avg Seizure Frequency", round(mean(epilepsy_data$seizure_frequency), 2),
      icon = icon("bolt"), color = "yellow"
    )
  })
  
  output$most_common_comorbidity_box <- renderInfoBox({
    infoBox(
      "Most Common Comorbidity", 
      names(which.max(table(epilepsy_data$comorbidity))),
      icon = icon("heartbeat"), color = "red"
    )
  })
  
  output$age_distribution_plot <- renderPlotly({
    p <- ggplot(epilepsy_data, aes(x = age)) +
      geom_histogram(binwidth = 5, fill = "skyblue", color = "black") +
      labs(title = "Age Distribution", x = "Age", y = "Count")
    ggplotly(p)
  })
  
  output$medication_distribution_plot <- renderPlotly({
    med_data <- epilepsy_data %>% 
      group_by(medication) %>% 
      summarise(count = n())
    
    p <- ggplot(med_data, aes(x = medication, y = count, fill = medication)) +
      geom_bar(stat = "identity") +
      labs(title = "Medication Distribution", x = "Medication", y = "Count")
    ggplotly(p)
  })
  
  filtered_data <- reactive({
    data <- epilepsy_data
    if (input$gender_filter != "All") {
      data <- data[data$gender == input$gender_filter,]
    }
    data <- data[data$age >= input$age_filter[1] & data$age <= input$age_filter[2],]
    if (input$medication_filter != "All") {
      data <- data[data$medication == input$medication_filter,]
    }
    data
  })
  
  output$patient_table <- DT::renderDataTable({
    DT::datatable(filtered_data(), options = list(pageLength = 10))
  })
  
  output$seizure_freq_plot <- renderPlotly({
    p <- ggplot(epilepsy_data, aes(x = seizure_frequency)) +
      geom_histogram(binwidth = 1, fill = "lightgreen", color = "black") +
      labs(title = "Seizure Frequency Distribution", x = "Seizure Frequency", y = "Count")
    ggplotly(p)
  })
  
  output$last_seizure_plot <- renderPlotly({
    p <- ggplot(epilepsy_data, aes(x = last_seizure_date)) +
      geom_histogram(binwidth = 7, fill = "lightpink", color = "black") +
      labs(title = "Last Seizure Date Distribution", x = "Date", y = "Count")
    ggplotly(p)
  })
  
  output$comorbidity_plot <- renderPlotly({
    p <- ggplot(epilepsy_data, aes(x = comorbidity, y = seizure_frequency, fill = comorbidity)) +
      geom_boxplot() +
      labs(title = "Comorbidity vs Seizure Frequency", x = "Comorbidity", y = "Seizure Frequency")
    ggplotly(p)
  })
}

shinyApp(ui, server)
