# Loading appropriate packages
library(rsconnect)
library(ggplot2)
library(shinyalert)
library(tidyverse)
library(shiny)
library(dplyr)
library(DT)
library(shinycssloaders)

# Project 2: R Shiny App

# Define UI
ui <- fluidPage(
  titlePanel("Melbourne Housing Market: Clearance Data from 2016"), # App title
  
  sidebarLayout(
    sidebarPanel(
      h2("Select Categorical Variables:"), # Sidebar title/header
      
      selectInput("type", "Home Type:", # Categorical Variable
                  choices = unique(data$Type), 
                  selected = "House"),
      
      selectInput("region", "Region:", # Categorical Variable
                  choices = unique(data$Regionname), 
                  selected = "North Metropolitan"),
      
      selectInput("council", "City Council:", # Categorical Variable
                  choices = unique(data$CouncilArea), 
                  selected = "Yarra City Council"),
      
      selectInput("suburb", "Suburb:", # Categorical Variable
                  choices = unique(data$Suburb), 
                  selected = "Abbotsford"),
      
      selectInput("seller", "Seller:", # Categorical Variable
                  choices = unique(data$SellerG), 
                  selected = "Biggin"),
      
      h2("Select a Numeric Variable:"), # Sidebar title/header
      
      selectInput("num_var", "Number of Rooms or Home Price:", 
                  choices = c("Rooms", "Price"), # Numeric Variables
                  selected = "Rooms"),
      
      uiOutput("room_slider"),
      uiOutput("price_slider"),
      
      actionButton("submit", "Submit"), # Reactive button
      downloadButton("download_data", "Download Data") # Reactive button
      
      ),
    
    mainPanel(
      tabsetPanel(
        tabPanel("About", # Getting a baseline design for my "About" tab
                 h3("About this App"),
                 h4("This R Shiny app allows the user to investigate clearence data from Mebourne's housing market in 2016."),
                 h4("This data can be found at https://www.kaggle.com/datasets/anthonypino/melbourne-housing-market."),
                 h4("Details about the chosen variables can also be found on the listed site."),
                 h4("Use the sidebar to filter data by catagories and download data."),
                 imageOutput("mel_img") # Melbourne Skyline
                 ),
        
        tabPanel("Data Download", # Tab that allows user to subset and download data
                 h3("Steps to Download Data"),
                 h4("1) Subset the data using the categorical and numeric variable found on sidebar."),
                 h4("2) Hit the Submit button to preview selected data."),
                 h4("3) When you're ready to download data, hit the Download Data button!."),
                 DT::dataTableOutput("table")
                 ),
        
        tabPanel("Data Exploration", 
                 tabsetPanel(
                   tabPanel("Categorical Summaries", # Categorical summaries 
                            h3("Categorical summaries based on selected categorical variables."),
                            h4("1) Choose a variable below (Suburb or SellerG) then subset those variables on the sidebar."),
                            h4("2) Hit the Submit button to update summary!"),
                             selectInput("cat_summary_var", "Choose Variables:", choices = c("Suburb", "SellerG")),
                             selectInput("cat_mod_var", "Subset by:", choices = c("None", "Suburb", "SellerG")),
                             verbatimTextOutput("cat_summary_output")
                             ),
                   
                   tabPanel("Numerical Summaries", # Numeric summaries
                            h3("Numerical summaries (mean, median, sd, IQR) based on selected variables below."),
                            h4("Hit the Submit button to update summary"),
                             selectInput("num_summary_var", "Choose Variables:", choices = c("Price", "Rooms", "YearBuilt", "Car")),
                             selectInput("num_group_var", "Group by:", choices = c("None", "Type", "Suburb")),
                             verbatimTextOutput("num_summary_output")
                            ),
                   
                   tabPanel("Graphs", # EDA graphs from user selected variables that could match those I have created
                            h3("Graph's Exploratory Data Analysis."),
                            h4("1) Select variables plot on the X and Y axis."),
                            h4("2) Select a vaiable to fill by."),
                            h4("3) Select the appropriate graph to vaisualizes your selected variables."),
                            h4("4) Hit the Submit button to visualize you selected data!"),
                             selectInput("x_var", "X-axis:", choices = c("Type", "Regionname", "Suburb", "SellerG", "Price", "Rooms", "CouncilArea")),
                             selectInput("y_var", "Y-Axis:", choices = c("Type", "Regionname", "Suburb", "SellerG", "Price", "Rooms", "CouncilArea")),
                             selectInput("color_var", "Fill by:", choices = c("None", "Type", "Regionname", "SellerG", "Rooms", "CouncilArea")),
                             selectInput("graph_type", "Select Graph Type:", choices = c("Scatter Plot", "Ridge Plot", "Bar Plot", "Bin Plot", "Density Plot", "Histogram")),
                             withSpinner(plotOutput("plot_output"), type = 1)
                            ),
                   )
                 )
        )
      )
    )
  )

# Define server logic
server <- function(input, output, session) {
  
  # Reactive subset of data based on selected inputs
  reactive_data <- eventReactive(input$submit, { # updates when Submit button is pushed
    data |>
      filter(Type == input$type,
             Regionname == input$region,
             CouncilArea == input$council,
             Suburb == input$suburb,
             SellerG == input$seller,
             between(get(input$num_var), input$min_value, input$max_value))
  })
  
  # Reactive slider ranges based on selected numeric variable
  observe({
    variable <- input$num_var
    min_val <- min(data[[variable]])
    max_val <- max(data[[variable]])
    
    # Update sliders based on the selected numeric variable's range
    output$room_slider <- renderUI({
      sliderInput("min_value", paste("Minimum", variable), min = min_val, max = max_val, value = min_val, step = 1)
    })
    
    output$price_slider <- renderUI({
      sliderInput("max_value", paste("Maximum", variable), min = min_val, max = max_val, value = max_val, step = 1)
    })
  })
  
  # Render Melbourne Image
  output$mel_img <- renderImage({ 
    filename <- normalizePath(file.path("www","image.jpg"))
    
    list(src = filename,
         contentType = 'image/jpg',
         width = 600,
         height = 800,
         alt = "Melbourne Skyline")
  }, deleteFile = FALSE)
  
  
  #Rendering data table in the Download tab
  output$table <- DT::renderDataTable({
    req(reactive_data())
    reactive_data()
  })
  
  # Saving user specified subset data to a csv file
  output$download_data <- downloadHandler(
    filename = function() {
      paste("data_subset", Sys.Date(), ".csv", sep = "")
      },
    content = function(file) {
      write.csv(reactive_data(), file)
    }
  )
  
  # Categorical summaries for Exploration Tab
  output$cat_summary_output <- renderPrint({
    req(input$cat_summary_var)
    if (input$cat_mod_var == "None") {
      reactive_data() |>
        group_by(.data[[input$cat_summary_var]]) |>
        summarise(counts = n())
    } else {
      reactive_data() |>
        group_by(.data[[input$cat_summary_var]], .data[[input$cat_mod_var]]) |>
        summarise(counts = n())
    }
  })
  
  # Numerical summaries for Exploration Tab
  output$num_summary_output <- renderPrint({
    req(input$num_summary_var, input$num_group_var)
    if (input$num_group_var == "None") {
      reactive_data() |>
        summarise(across(all_of(input$num_summary_var),
                         list(mean = mean, median = median, sd = sd, IQR = IQR)))
    } else {
      reactive_data() |>
        group_by(.data[[input$num_group_var]]) |>
        summarise(across(all_of(input$num_summary_var),
                         list(mean = mean, median = median, sd = sd, IQR = IQR)))
    }
  })
  
  # EDA graphs from user selected variables that could match those I have created
  output$plot_output <- renderPlot({
    req(input$x_var, input$y_var, input$color_var, input$graph_type)
    
    data <- reactive_data()
    
    # Debugging: Print selected variables
    print(input$x_var)
    print(input$y_var)
    print(input$color_var)
    
    # Adjust plot aesthetics based on color_var
    if (input$color_var != "None") {
      p <- ggplot(data, aes_string(x = input$x_var, y = input$y_var, color = input$color_var))
    } else {
      p <- ggplot(data, aes_string(x = input$x_var, y = input$y_var))
    }
    
    # Generate plot based on graph type
    plot_type <- switch(input$graph_type,
                        
                        "Scatter Plot" = p + 
                          geom_point() +
                          labs(x = input$x_var, y = input$y_var) +
                          theme_minimal(),
                        
                        "Ridge Plot" = ggplot(reactive_data(), aes_string(
                          x = input$x_var, y = input$y_var, fill = input$color_var)) +
                          geom_density_ridges_gradient(scale = 3, rel_min_height = 0.01) +
                          scale_fill_viridis_c(name = input$color_var) +
                          labs(x = input$x_var, y = input$y_var) +
                          theme_minimal(),
                        
                        "Bar Plot" = ggplot(reactive_data(), aes_string(x = input$x_var, fill = input$color_var)) +
                          geom_bar(position = "stack") +
                          labs(x = input$x_var, y = "Count") +
                          theme_minimal(),
                        
                        "Density Plot" = ggplot(reactive_data(), aes_string(x = input$x_var, fill = input$color_var)) +
                          geom_density(alpha = 0.5) +
                          labs(x = input$x_var, y = "Density") +
                          theme_minimal(),
                        
                        "Histogram" = ggplot(reactive_data(), aes_string(x = input$x_var, fill = input$color_var)) +
                          geom_histogram(binwidth = 50000, color = "black", alpha = 0.7) + # Adjust `binwidth` as needed
                          labs(x = input$x_var, y = "Count") +
                          scale_fill_discrete(name = input$color_var) +
                          theme_minimal() +
                          theme(axis.text.x = element_text(angle = 45, hjust = 1)),
                        
                        "Bin Plot" = ggplot(reactive_data(), aes_string(x = input$x_var, y = input$y_var, fill = input$color_var)) +
                          geom_bin2d(bins = 30) +
                          scale_fill_viridis_c() +
                          labs(x = input$x_var, y = input$y_var) +
                          theme_minimal()
                        )
    
    plot_type
  })
}

# Run the application 
shinyApp(ui = ui, server = server)
