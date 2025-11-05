# Elle Whitlock
# Building the app for project 2 
# ST 558


# packages needed
library(shiny)
library(shinyalert)
library(tidyverse)
library(DT)
library(ggplot2)

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
        # includes image and link!
        tabPanel("About",
                 HTML("<h2>About This App and Key Features</h2>
                        <p> This interactive Shiny dashboard explores mobile device 
                        usage patterns. In the side panel you can subset the data by gender, 
                        operating system, and by modifying the range of values for any two
                        numeric variables. However you must hit the button to update the data! 
                        In the download tab, the data can be downloaded in a csv format.
                        Finally in the exploration tab you can learn about the data. You
                        can choose between looking at plots, or summaries. The plots include
                        six different types in which you can choose what variable(s) you
                        want to look at. For summaries you can create statistics with
                        respect to numeric variables, and contingency tables with respect to 
                        categorical variables.
                        </p>
                        
                        <img src='phone.jpg' width='45%'>
                        
                        <h2>About the Data</h2>
                        <p> This app uses 
                        <a href='https://www.kaggle.com/datasets/meetnagadia/mobile-device-usage-and-user-behavior'
                        target='_blank'>
                        Mobile Device Usage and User Behavior Data Set
                        </a>.Mobile Device Usage and User Behavior Data Set
                        this data includes demographic data such as gender and age. 
                        It includes information about the users device such as the 
                        operating system and device model. Data on their device use 
                        was also collected such as number of apps installed, app usage 
                        time (min/day), battery consumption (milliamp-hours/day), 
                        screen on time (hours/day), and data usage (megabytes/day). 
                        They also classified user behavior on a five number scale ranging 
                        form light to extreme usage. This data is simulated, based on trends,
                        not raw observed data.
                        </p>
                    "),
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
                 conditionalPanel(
                   condition = "input.view_type == 'Plots'",
                   radioButtons("plot_type", label = "What type of plot?",
                               choices = c("Scatter",
                                           "Bar",
                                           "Density",
                                           "Violin",
                                           "Box",
                                           "Hexbin")),
                                selected = "Scatter",
                 
                    # follow-up inputs
                    uiOutput("plot_followups"),
                    # make plot
                    plotOutput("explore_plot")),
                 
                  # summary
                  conditionalPanel(
                    condition = "input.view_type == 'Summaries'",
                    radioButtons("sum_type", label = "Would you like numeric or categorical summary?",
                                 choices = c("Numeric",
                                             "Categorical"),
                                 selected = "Numeric"),
                    
                    # follow-up inputs
                    uiOutput("summary_followups"),
                    # make table
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
  # told to use reactive() I will use in conjunction with isolate()
  mobile_data_new <- reactive({
    
    if (input$sub_data == 0)
      return()
    
    isolate({
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
  })

  
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
  
  # plots
  
  # if statements to check type and ask for follow-up inputs
  # taglist() lets us return multiple UI elements
  # note this code only allows for one facet as per static code
  output$plot_followups <- renderUI({
    
    # scatter
    if (input$plot_type == "Scatter") {
      tagList(
        selectInput("sc_x", "X variable (numeric)", choices = num_vars),
        selectInput("sc_y", "Y variable (numeric)", choices = num_vars),
        selectInput("sc_color", "Color by (categorical)",
                    choices = c("None", cat_vars), selected = "None")
      )
    
    # box  
    } else if (input$plot_type == "Box") {
      tagList(
        selectInput("bx_x", "Group (categorical)", choices = cat_vars),
        selectInput("bx_y", "Y variable (numeric)", choices = num_vars),
        selectInput("bx_facet", "Facet by (optional)",
                    choices = c("None", cat_vars), selected = "None")
      )
   
    # bar   
    } else if (input$plot_type == "Bar") {
      tagList(
        selectInput("bar_x", "Categorical variable", choices = cat_vars),
        selectInput("bar_fill", "Fill by (optional)",
                    choices = c("None", cat_vars), selected = "None"),
      )
     
    # density   
    } else if (input$plot_type == "Density") {
      tagList(
        selectInput("dn_x", "Numeric variable", choices = num_vars),
        selectInput("dn_fill", "Fill by (categorical)", choices = cat_vars),
      )
      
    # violin  
    } else if (input$plot_type == "Violin") {
      tagList(
        selectInput("vl_x", "Group (categorical)", choices = cat_vars),
        selectInput("vl_y", "Y variable (numeric)", choices = num_vars),
        selectInput("vl_facet", "Facet by (optional)",
                    choices = c("None", cat_vars), selected = "None")
      )
      
    # Hexbin
    } else if (input$plot_type == "Hexbin") {
      tagList(
        selectInput("hx_x", "X (numeric)", choices = num_vars),
        selectInput("hx_y", "Y (numeric)", choices = num_vars),
        selectInput("hx_facet", "Facet by (optional)",
                    choices = c("None", cat_vars), selected = "None")
      )
    }
  })
  
  
  # if statements to check type and generate plot
  # based on code from static 
  output$explore_plot <- renderPlot({
    
    df <- mobile_data_new()
    
    # scatter
    if (input$plot_type == "Scatter") {
      if (input$sc_color != "None"){
      ggplot(df, aes(x = .data[[input$sc_x]], y = .data[[input$sc_y]],
                     color = .data[[input$sc_color]] )) +
          geom_point() +
          labs(x = input$sc_x, 
               y = input$sc_y,
               # custom title (will be used in plots below too)
               title = paste0("Scatterplot of ", input$sc_y, " vs ", input$sc_x)) 
      } 
      else {
        ggplot(df, aes(x = .data[[input$sc_x]], y = .data[[input$sc_y]])) + 
          geom_point() +
          labs(x = input$sc_x, 
               y = input$sc_y,
               # custom title (will be used in plots below too)
               title = paste0("Scatterplot of ", input$sc_y, " vs ", input$sc_x)) 
      }
      
    # box
    } else if (input$plot_type == "Box") {
      g <- ggplot(df, aes(x = .data[[input$bx_x]], y = .data[[input$bx_y]])) +
        geom_boxplot(aes(fill = .data[[input$bx_x]])) +
        labs(x = input$bx_x, y = input$bx_y, title = paste0("Boxplot of ", input$bx_y, " by ", input$bx_x)) 
      if (input$bx_facet != "None"){
        # conditional for facet, will add it on if it exists, same for other plots that use it
        # as.formula means it is converts it from the past text string
        g <- g + facet_wrap(as.formula(paste("~", input$bx_facet)))
      }
      g
      
    # bar  
    } else if (input$plot_type == "Bar") {
      g <- ggplot(df, aes(x = .data[[input$bar_x]])) +
        geom_bar(aes(fill = .data[[input$bar_x]])) +
        labs(x = input$bar_x, y = "Count", title = "Bar Chart") 
      if (input$bar_fill != "None"){
        g <- ggplot(df, aes(x = .data[[input$bar_x]], fill = .data[[input$bar_fill]])) +
        geom_bar(position = "stack") +
        labs(x = input$bar_x, y = "Count", title = paste0("Bar chart of ", input$bar_x))}
      g
      
    # density  
    } else if (input$plot_type == "Density") {
      ggplot(df, aes(x = .data[[input$dn_x]], fill = .data[[input$dn_fill]])) +
        geom_density() +
        labs(x = input$dn_x, y = "Density", title = paste0("Density of ", input$dn_x, " by ", input$dn_fill)) 
    
    # violin  
    } else if (input$plot_type == "Violin") {
      g <- ggplot(df, aes(x = .data[[input$vl_x]], y = .data[[input$vl_y]])) +
        geom_violin() +
        labs(x = input$vl_x, y = input$vl_y, title = paste0("Violin plot of ", input$vl_y, " by ", input$vl_x)) 
      if (input$vl_facet != "None"){
      g <- g + facet_wrap(as.formula(paste("~", input$vl_facet)))
      }
      g
    
    # hexbin  
    } else if (input$plot_type == "Hexbin") {
      g <- ggplot(df, aes(x = .data[[input$hx_x]], y = .data[[input$hx_y]])) +
        geom_hex(bins = 25) +
        labs(x = input$hx_x, y = input$hx_y, title = paste0("Hexbin plot of ", input$hx_y, " vs ", input$hx_x)) 
      if (input$hx_facet != "None"){
      g <- g + facet_wrap(as.formula(paste("~", input$hx_facet)))
      }
      g
    }
  })
  
  # summaries
  
  # if statements to check type and ask for follow-up inputs
  output$summary_followups <- renderUI({
    
    if (input$sum_type == "Numeric") {
      tagList(
        selectInput("sum_num", "Choose a numeric variable to summarize", choices = num_vars),
        selectInput("sum_grp", "Choose a catergorical variable to group by (optional)",
                    choices = c("None", cat_vars), selected = "None"))

    } else {
      tagList(
      selectInput("sum_cat", "Choose a categorical variable to summarize",
                  choices = cat_vars),
      selectInput("sum_cat2", "Choose a second categorical variable to summarize by (optional)",
                  choices = c("None", cat_vars), selected = "None"))
    }
  })
  
  # if statements to check type and generate summary
  output$summary_out <- renderTable({
    
    df <- mobile_data_new()
    
    # numeric summary
    if (input$sum_type == "Numeric") {
       # grouped numeric summary
       if (input$sum_grp != "None") {
        df |>
          group_by(.data[[input$sum_grp]]) |>
          summarise(
            mean = mean(.data[[input$sum_num]]),
            sd = sd(.data[[input$sum_num]]),
            median = median(.data[[input$sum_num]]),
            IQR = IQR(.data[[input$sum_num]]))
         
      # not grouped numeric summary   
      } else {
        df |>
          summarise(
            mean = mean(.data[[input$sum_num]]),
            sd = sd(.data[[input$sum_num]]),
            median = median(.data[[input$sum_num]]),
            IQR = IQR(.data[[input$sum_num]]))
      }
      
    # categorical summaries
    } else {
      
      # two-way table
      if (input$sum_cat2 != "None") {
        tbl <- table(df[[input$sum_cat]], df[[input$sum_cat2]])
        
      # one-way table     
      } else {
        tbl <- table(df[[input$sum_cat]])
      }
    }
  })
  
}


# Run the application 
shinyApp(ui = ui, server = server)
