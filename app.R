# Loading appropriate packages
library(rsconnect)
library(ggplot2)
library(shinyalert)
library(tidyverse)
library(shiny)
library(dplyr)
library(DT)

# Define UI
ui <- fluidPage(
  titlePanel("Melbourne Housing Market: Clearance Data from 2016"),
  
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
      
      actionButton("submit", "Submit"),
      downloadButton("download_data", "Download Subset Data")
      
      ),
    
    mainPanel(
      tabsetPanel(
        tabPanel("About", # Getting a baseline design for my "About" tab
                 h3("About this App"),
                 p("This R Shiny app allows the user to investigate clearence data from Mebourne's housing market in 2016."),
                 p("This data can be found at https://www.kaggle.com/datasets/anthonypino/melbourne-housing-market. Details about the chosen variables can also be found on this website."),
                 p("Use the sidebar to filter data by catagories and download data."),
                 imageOutput("mel_img") # Melbourne Skyline
                 ),
        
        tabPanel("Data Download",
                 DT::dataTableOutput("table")
                 ),
        
        tabPanel("Data Exploration", )
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
      sliderInput("min_value", paste("Minimum", variable), min = min_val, max = max_val, value = min_val, step = 1) # Displaying two values on slider axis
    })
    
    output$price_slider <- renderUI({
      sliderInput("max_value", paste("Maximum", variable), min = min_val, max = max_val, value = max_val, step = 1) # Displaying two values on slider axis
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
  
  # Saving reactive subset data to a csv file
  output$download_data <- downloadHandler(
    filename = function() {
      paste("data_subset", Sys.Date(), ".csv", sep = "")
      },
    content = function(file) {
      write.csv(reactive_data(), file)
    }
  )
}

# Run the application 
shinyApp(ui = ui, server = server)
