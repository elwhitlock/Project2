# Elle Whitlock
# Building the app for project 2 
# ST 558


# packages needed
library(shiny)
library(shinyalert)
library(tidyverse)

# so we can reference data
source("static.R")

# Define UI 
ui <- fluidPage(
  "Mobile Data",
  sidebarLayout(
    sidebarPanel(
      h2("Select how you want to subset the data"),
      selectInput("gender_select", label = "Select gender",
                  choices = c("Both", 
                              unique(mobile_data$gender)),
                  selected = "Both"),
      selectInput("syst_select", label = "Select operating system",
                  choices = c("Both", 
                              unique(mobile_data$op_system)),
                  selected = "Both"), 
      h2("Choose the first numeric variable to summarize on"),
      radioButtons("num_var1",
                   label = "Select variable",
                   choiceNames  = c("App Usage Time (min/day)",
                                    "Screen On Time (hours/day)",
                                    "Battery Drain (mAh/day)",
                                    "Number of Apps Installed",
                                    "Data Usage (MB/day)",
                                    "Age"),
                   choiceValues = c("app_usage",
                                    "screen_on",
                                    "battery_drain",
                                    "num_apps",
                                    "data_usage",
                                    "age"), 
                   selected = "app_usage"),
      h2("Subset on your first numeric variable"),

      actionButton("subset_data","Subset the data")
    ),
))


server <- function(input, output, session) {
  
  # We need the slider to update based on the numeric variable selected
  output$num_slide1 <- renderUI({
    slide_min1 <- min(mobile_data[[input$numvar1]])
    slide_max1 <- max(mobile_data[[input$numvar1]])
    sliderInput("num_slide1",
                "Set values",
                min = slide_min1,
                max = slide_max1,
                value = c(slide_min1, slide_max1)
    )
  })
  
  
  output$num_slide1 <- renderUI({
    slide_min1 <- min(mobile_data[[input$numvar1]])
    slide_max1 <- max(mobile_data[[input$numvar1]])
    sliderInput("num_slide1",
                "Set values",
                min = slide_min1,
                max = slide_max1,
                value = c(slide_min1, slide_max1)
    )
  })
}


# Run the application 
shinyApp(ui = ui, server = server)
