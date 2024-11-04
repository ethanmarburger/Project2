# Loading appropriate packages
library(shiny)
library(rsconnect)
library(ggplot2)
library(shinyalert)
library(tidyverse)

# Reading in Melbourne Houseing data (Full dataset)

data <- read_csv("Melbourne_housing_FULL.csv")
head(data)

#