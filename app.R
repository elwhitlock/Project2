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
                   choiceValues = numeric_vars,
                   selected = "app_usage"),
      h2("Subset on your first numeric variable"),
      uiOutput("slide1"),
      h2("Choose the first numeric variable to summarize on"),
      radioButtons("num_var2",
                   label = "Select variable",
                   choiceNames  = c("App Usage Time (min/day)",
                                    "Screen On Time (hours/day)",
                                    "Battery Drain (mAh/day)",
                                    "Number of Apps Installed",
                                    "Data Usage (MB/day)",
                                    "Age"),
                   choiceValues = numeric_vars,
                   selected = "age"),
      h2("Subset on your second numeric variable"),
      uiOutput("slide2"),
      actionButton("subset_data","Subset the data")
    ),
    mainPanel()
))


server <- function(input, output, session) {
  
  # We need the sliders to update based on the numeric variables selected
  # first make sure that they can not input the same variable twice 
  # I used HW7 for reference on this
  
  # update first numeric variable
  observeEvent(input$num_var2, {
    num_var2 <- input$num_var2
    num_var1 <- input$num_var1
    choices <- numeric_vars
    if (num_var2 != num_var1){
      choices <- choices[-which(choices == num_var2)]
      updateRadioButtons(session,
                         "num_var1",
                          choices = choices,
                          selected = num_var1)
    }
  })
  
  # update second numeric variable
  observeEvent(input$num_var1, {
    num_var2 <- input$num_var2
    num_var1 <- input$num_var1
    choices <- numeric_vars
    if (num_var2 != num_var1){
      choices <- choices[-which(choices == num_var1)]
      updateRadioButtons(session,
                         "num_var2",
                          choices = choices,
                          selected = num_var2)
    }
  })
  
  # slider 1
  output$slide1 <- renderUI({
    slide_min1 <- min(mobile_data[[input$num_var1]])
    slide_max1 <- max(mobile_data[[input$num_var1]])
    sliderInput("slide1",
                "Set values",
                min = slide_min1,
                max = slide_max1,
                value = c(slide_min1, slide_max1)
    )
  })
  
  # slider 2
  output$slide2 <- renderUI({
    slide_min2 <- min(mobile_data[[input$num_var2]])
    slide_max2 <- max(mobile_data[[input$num_var2]])
    sliderInput("slide1",
                "Set values",
                min = slide_min2,
                max = slide_max2,
                value = c(slide_min2, slide_max2)
    )
  })
}


# Run the application 
shinyApp(ui = ui, server = server)
