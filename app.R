# Elle Whitlock
# Building the app for project 2 
# ST 558


# packages needed
library(shiny)
library(shinyalert)
library(tidyverse)
library(DT)

# so we can reference data
source("static.R")

# Define UI 
ui <- fluidPage(
  "Mobile Data",
  sidebarLayout(
    
    # side panel
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
      
      radioButtons("num_var1",
                   label = "Select first numeric variable",
                   choices = num_vars,
                   selected = "app_usage"),
      
      uiOutput("slide1"),
      
      radioButtons("num_var2",
                   label = "Select second numeric variable",
                   choices = num_vars,
                   selected = "age"),
      
      uiOutput("slide2"),
      
      actionButton("sub_data","Subset the data")
    ),
    
    # main panel with three tabs
    mainPanel(
      tabsetPanel(
        # for this panel I used outside resources to learn about HTML
        tabPanel("About",
                 img(src = "phone.jpg", width="50%")
                 ),
        
        # using provided code
        tabPanel("Data Download",
                 dataTableOutput("data_tbl"),
                 downloadButton("down_data","Download the data")),
        
        tabPanel("Data Exploration",
                 
                 # choose what analysis to view
                 selectInput("view_type", label = "Do you want to look at plots of summaries?",
                             choices = c("Plots", 
                                         "Summaries"),
                             selected = "Plots"),
                 
                 # based on choice
                 
                 # plot
                 # note I did a lot of plot exploration in static code, I am limiting to only three types
                 conditionalPanel(
                   condition = "input.view_type == 'Plots'",
                   radioButtons("plot_type", label = "What type of plot?",
                               choices = c("Scatter",
                                           "Violin",
                                           "Hexbin")),
                                selected = "Scatter",
                 
                  # follow-up inputs
                  uiOutput("plot_followups"),
                  plotOutput("explore_plot")),
                 
                  # summary
                  conditionalPanel(
                    condition = "input.view_type == 'Summaries'",
                    radioButtons("sum_type", label = "Would you like numeric or categorical summary?",
                                 choices = c("Numeric",
                                             "Categorical"),
                                 selected = "Scatter"),
                    
                    # follow-up inputs
                    uiOutput("summary_followups"),
                    tableOutput("summary_out"))
        )
      )
    )
  )
)


server <- function(input, output, session) {
  # SIDEBAR
  
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
  # same layout as above, slider 2
  output$slide2 <- renderUI({
    slide_min2 <- min(mobile_data[[input$num_var2]])
    slide_max2 <- max(mobile_data[[input$num_var2]])
    sliderInput("slide2",
                "Set values",
                min = slide_min2,
                max = slide_max2,
                value = c(slide_min2, slide_max2)
    )
  })
  
  # next we need to deal with the button being clicked to subset the data
  # told to use reactive() or reactiveValues()
  
  mobile_data_new <- reactive({

    df<-mobile_data
    
    # categorical variables
    # gender subset first
    # if not both then filter accordingly
    if(input$gender_select != "Both"){
      df <- df |>
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

    df
  })
  
  #observeEvent(input$sub_data,{})
  
  #  DATA DOWNLOAD
  
  # create data table based on subset
  output$data_tbl <- renderDataTable(
    mobile_data_new())
  
  # allows someone to download data, based on provided link
  output$down_data <- downloadHandler(
      filename = function() {
        paste('mobile_subset','.csv', sep='')
      },
      content = function(file) {
        write.csv(mobile_data_new(), file)
      }
    )
  
  # DATA EXPLORATION
  
  }


# Run the application 
shinyApp(ui = ui, server = server)
