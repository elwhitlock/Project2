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
                              "Female",
                              "Male"),
                  selected = "Both"),
      selectInput("syst_select", label = "Select operating system",
                  choices = c("Both", 
                              "Android",
                              "iOS"),
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
                   choiceValues = num_vars,
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
                   choiceValues = num_vars,
                   selected = "age"),
      h2("Subset on your second numeric variable"),
      uiOutput("slide2"),
      actionButton("sub_data","Subset the data")
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
    choices <- num_vars
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
    choices <- num_vars
    if (num_var2 != num_var1){
      choices <- choices[-which(choices == num_var1)]
      updateRadioButtons(session,
                         "num_var2",
                          choices = choices,
                          selected = num_var2)
    }
  })
  
  # slider 1
  # notice we use min and max functions to define bounds
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
  # same layout as above, slider 1
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
  
  # next we need to deal with the button being clicked to subset the data
  # told to use reactive() or reactiveValues()
  
  mobile_data_new <- reactive({
    
    # categorical variables
    # gender subset first
    # if not both then filter accordingly
    if(input$gender_select != "Both"){
      df <- mobile_data |>
        filter(gender == input$gender_select)
    }
    
    # operating system subset next
    # same as above, gender
    if(input$syst_select != "Both"){
      df <- df |>
        filter(op_system == input$syst_select)
    }
    
    # numeric variables
    # need to only allow the range specified by the slider, which has a min and max
    # .data tells us to refer to the df we are piping on
    df <- df |>
      filter(.data[[input$num_var1]] >= input$slide1[1],
             .data[[input$num_var1]] <= input$slide1[2],
             .data[[input$num_var2]] >= input$slide2[1],
             .data[[input$num_var2]] <= input$slide2[2])
    
    # return
    df
  })
  
  #observeEvent(input$sub_data,{})
}


# Run the application 
shinyApp(ui = ui, server = server)
