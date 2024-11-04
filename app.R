# Loading appropriate packages
library(shiny)
library(rsconnect)
library(ggplot2)
library(shinyalert)
library(tidyverse)

library(shiny)
library(dplyr)

# Define UI
ui <- fluidPage(
  titlePanel(""),
  
  sidebarLayout(
    sidebarPanel(
      h2("Select Categorical Variables:"), # Sidebar title/header
      
      selectInput("type", "Home Type:", 
                  choices = unique(data$Type), 
                  selected = "House"),
      
      selectInput("region", "Region:", 
                  choices = unique(data$Regionname), 
                  selected = "North Metropolitan"),
      
      selectInput("council", "City Council:", 
                  choices = unique(data$CouncilArea), 
                  selected = "Yarra City Council"),
      
      selectInput("suburb", "Suburb:", 
                  choices = unique(data$Suburb), 
                  selected = "Abbotsford"),
      
      h2("Select a Numeric Variable:"), # Sidebar title/header
      
      selectInput("num_var", "Number of Rooms or Home Price:", 
                  choices = c("Rooms", "Price"),
                  selected = "Rooms"),
      
      uiOutput("numeric_slider1"),
      uiOutput("numeric_slider2"),
      
      actionButton("submit", "Submit")
    ),
    
    mainPanel(
      tableOutput("")
    )
  )
)

# Define server logic
server <- function(input, output, session) {
  
}

# Run the application 
shinyApp(ui = ui, server = server)
