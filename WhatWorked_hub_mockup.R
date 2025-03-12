library(shiny)
library(shinydashboard)
library(ggplot2)
library(DT)
library(shinyWidgets)
library(dplyr)
library(plotly)
library(shinyBS)

# Dummy data representing trial results
set.seed(123)
dummy_trials <- data.frame(
  TrialID = paste0("T", 1:10),
  DateCompleted = seq(as.Date("2023-01-01"), by = "month", length.out = 10),
  
  TreatmentSize = sample(25:100, 10, replace = TRUE),
  ControlSize = sample(25:100, 10, replace = TRUE),
  EffectSize = round(rnorm(10, 0.2, 0.1), 2),
  StandardError = round(runif(10, 0.05, 0.1), 2),
  PercentageChange = round(runif(10, 5, 20), 1),
  Cost = sample(10:100, 10, replace = TRUE)
)
colnames(dummy_trials)[8]<-" Cost (£)"
dummy_trials$SampleSize = dummy_trials$TreatmentSize + dummy_trials$ControlSize
dummy_trials$ValueForMoney <- round(dummy_trials$` Cost (£)`/dummy_trials$EffectSize,2)

# UI #####
ui <- dashboardPage(
  skin = "red",
  dashboardHeader(title = span(icon("graduation-cap"), "WhatWorked Education")),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Impact Dashboard", tabName = "dashboard", icon = icon("chart-line")),
      menuItem("Trial Results", tabName = "scorecards", icon = icon("table")),
      menuItem("Intervention Comparison", tabName = "comparison", icon = icon("balance-scale")),
      menuItem("Context Matcher", tabName = "context", icon = icon("filter")),
      menuItem("Implementation Planner", tabName = "planner", icon = icon("clipboard-check")),
      menuItem("Educator Resources", tabName = "resources", icon = icon("book"))
    )
  ),
  dashboardBody(
    tabItems(
      # Dashboard Tab ----
      tabItem(tabName = "dashboard", tags$head(
        tags$style(HTML("
            .box.box-solid.box-primary > .box-header {
              background-color: black !important;
              color: white !important;
            }
          "))
      ),
              fluidRow(
                box(title = "Effect Size Trends", status = "primary", solidHeader = TRUE,
                    plotlyOutput("effect_size_trend"), width = 6),
                box(title = "Impact Heatmap", status = "primary", solidHeader = TRUE,
                    plotOutput("impact_heatmap"), width = 6)
              )
      ),
      
      # Trial Results Tab ----
      tabItem(tabName = "scorecards", tags$head(
        tags$style(HTML("
            .box.box-solid.box-primary > .box-header {
              background-color: black !important;
              color: white !important;
            }
          "))
      ),
              fluidRow(
                box(title = "Trial Effectiveness Summary", status = "primary", solidHeader = TRUE,
                    DTOutput("trial_table"), width = 12)
              )
      ),
      
      # Intervention Comparison Calculator
      tabItem(tabName = "comparison",
              fluidRow(
                box(title = "Intervention Comparison", status = "primary", solidHeader = TRUE, width = 12,
                    plotlyOutput("comparison_chart"))
              )
      ),
      
      # Context Matcher
      tabItem(tabName = "context",
              fluidRow(
                box(title = "Context Matcher", status = "primary", solidHeader = TRUE, width = 12,
                    selectInput("school_type", "School Type:", choices = c("Urban", "Suburban", "Rural")),
                    selectInput("socioeconomic_status", "Socioeconomic Status:", choices = c("Low", "Medium", "High")),
                    selectInput("school_size", "School Size:", choices = c("Small", "Medium", "Large")),
                    actionButton("match_context", "Find Relevant Studies", class = "btn btn-primary")
                ),
                box(title = "Matched Studies", status = "danger", solidHeader = TRUE, width = 12,
                    DTOutput("matched_studies"))
              )
      ),
      
      # Implementation Planner
      tabItem(tabName = "planner",
              fluidRow(
                box(title = "Implementation Planner", status = "primary", solidHeader = TRUE, width = 12,
                    uiOutput("planner_steps"))
              )
      ),
      
      # Educator Resources Tab ----
      tabItem(tabName = "resources",
              fluidRow(
                box(title = "Actionable Playbooks", status = "primary", solidHeader = TRUE, width = 4,
                    p("One-page implementation guides to help you apply evidence-based strategies effectively.")),
                box(title = "Introductory Videos", status = "primary", solidHeader = TRUE, width = 4,
                    p("Videos explaining key statistical concepts in an accessible way.")),
                box(title = "Community Forums", status = "primary", solidHeader = TRUE, width = 4,
                    p("Join discussions, share successes, and learn best practices from other educators."))
              )
      )
    )
  )
)

# Server ####
server <- function(input, output) {
  # Effect Size Trends ----
  output$effect_size_trend <- renderPlotly({
    p <- ggplot(dummy_trials, aes(x = DateCompleted, y = EffectSize, label = TrialID)) +
      geom_line(color = "black", size = 0.5) +
      geom_point(aes(size = SampleSize), color = "red", alpha = 0.7) +
      theme_minimal() +
      labs(title = "Effect Size Over Time", x = "Date", y = "Effect Size")
    ggplotly(p)
  })
  
  output$impact_heatmap <- renderPlot({
    ggplot(dummy_trials, aes(x = DateCompleted, y = TrialID, fill = PercentageChange)) +
      geom_tile() +
      scale_fill_gradient(low = "white", high = "red") +
      theme_minimal() +
      labs(title = "Impact Heatmap", x = "Date", y = "Trial ID")
  })
  
  # Trial Effectiveness Table ----
  output$trial_table <- renderDT({
    datatable(dummy_trials, options = list(pageLength = 5))
  })
  
  # Intervention Comparison Chart
  output$comparison_chart <- renderPlotly({
    p <- ggplot(dummy_trials, aes(x = TrialID, y = EffectSize, fill = ValueForMoney )) +
      geom_bar(stat = "identity", color = "black") +
      scale_fill_gradient(low = "white", high = "red") +
      theme_minimal() +
      labs(title = "Effect Size vs Value for Money", x = "Trial", y = "Effect Size", fill = "Value for Money")
    ggplotly(p)
  })
  
  # Context Matcher Logic
  output$matched_studies <- renderDT({
    req(input$match_context)
    matched <- dummy_trials %>% sample_n(5)
    datatable(matched, options = list(pageLength = 5))
  })
  
  # Implementation Planner Steps
  output$planner_steps <- renderUI({
    tagList(
      selectInput("intervention_choice", "Select an Evidence-Based Intervention:", choices = dummy_trials$TrialID),
      textInput("resources", "Required Resources:"),
      dateInput("milestone_1", "Set First Implementation Milestone:"),
      textAreaInput("data_collection", "Plan for Data Collection:", "Describe how you'll measure impact..."),
      actionButton("save_plan", "Save Implementation Plan", class = "btn btn-success")
    )
  })
}

######
shinyApp(ui, server)
