#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#
#class 5103
#assignment #3
#created by Derek Halkes
#version 2
#last modified date: 3/22/2022

#libraries needed
library(tidyverse)
library(shiny)
library(shinydashboard)
library(mapdata)

#importing raw data
bees <- read_csv("honeybees.csv")
states <- map_data("state")

#changing data type for "year" column
#bees$year = as.integer(bees$year)

#changing State to lower case for joins
bees$StateName = tolower(bees$StateName)

#merging state map data with bee data
state_map <- left_join(states, bees, by = c("region" = "StateName"))

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    headerPanel("Honey bee data for years 1998-2017"),

    sidebarPanel(
        selectInput("select", "Choose a dataset:",
                    choices = c("# of colonies" = "numcol",
                                "yield per colony" = "yieldpercol",
                                "price per lb." = "priceperlb",
                                "total production" = "totalprod")),
        sliderInput("slider", "Years:",
                    min = state_map$year[1],  # The min value is the first available year
##if i change max input to 20 (which is the number of years in data)
##then the slider converts to decimals
                    max = state_map$year[14],  # The max value is the last available year
                    value = c(state_map$year[1], state_map$year[14]),  # The default selected values are the min to max years i.e the whole data
                    sep = ""),
        mainPanel(
    
            verbatimTextOutput("summary"),
            
            #output
            tableOutput("view")    
    
        )
        )
)

# Define server logic required to draw an output
server <- function(input, output){

    # Return the requested dataset ----
    datasetInput <- reactive({
        switch(input$select,
               "# of colonies" = "numcol",
               "yield per colony" = "yieldpercol",
               "price per lb." = "priceperlb",
               "total production" = "totalprod")
    })
    
    # Generate a summary of the dataset ----
    output$summary <- renderPrint({
        dataset <- datasetInput()
        summary(dataset)
    })
    
    # Show the years selected with slider
    output$view <- renderTable({
        head(datasetInput(), n = input$slider)
    })
}    

# Run the application 
shinyApp(ui = ui, server = server)
