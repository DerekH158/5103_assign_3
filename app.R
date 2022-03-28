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
#version 3
#last modified date: 3/28/2022

#libraries needed
library(tidyverse)
library(shiny)
library(shinydashboard)
library(rsconnect)
#library(mapdata)

#importing raw data
bees <- read_csv("honeybees.csv")
#states <- map_data("state")

#removing unneeded columns
bees2 <- bees[c(1:10)]

#reordering 2 of the columns
bees2 <- bees2[with(bees2, order(StateName, year)), ]

#renaming columns
bees3 <- rename(bees2, c("No. of colonies" = "numcol",
                "Yield per colony" = "yieldpercol",
                "Total production" = "totalprod",
                "Price per lb." = "priceperlb",
                "Production value" = "prodvalue"))

#changing State to lower case for joins
#bees$StateName = tolower(bees$StateName)

#merging state map data with bee data
#state_map <- left_join(states, bees, by = c("region" = "StateName"))

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Honeybee data in the US"),

sidebarLayout(
    
sidebarPanel(
    
    # Create a new Row in the UI for selectInputs
    fluidRow(
        column(4,
               selectInput("stt",
                           "State:",
                           c("All",
                             unique(as.character(bees3$StateName))))
        )),
    fluidRow(
        column(4,
               selectInput("year",
                           "Year:",
                           c("All",
                             unique(as.character(bees3$year))))
        ),
    ),
),
mainPanel(
    # Create a new row for the table.
   DT::dataTableOutput("table")
    
,
#spot for barplot 1
    plotOutput("beeplot1")  

#secondary barplot
    plotOutput("beeplot2")

)

))

# Define server logic required to draw a histogram
server <- function(input, output) {

#table output
    output$table <- DT::renderDataTable(DT::datatable({
        data <- bees3
        if (input$stt != "All") {
            data <- data[data$StateName == input$stt,]
        }
        if (input$year != "All") {
            data <- data[data$year == input$year,]
        }
        data
    }))
    
#barplot
   output$beeplot1 <- renderPlot({
        
#render barplot
        barplot(bees2[,inputtotalprod], 
                main=input$totalprod,
                ylab="Total bee production",
                xlab="Year")}

#barplot #2
    output$beeplot2 <- renderPlot({
#secondary barplot
plot(x = year, y = bees2$priceperlb,
     main="Price per pound by year",
     xlab="Year",
     ylab="Price per lb.",
     col="blue")}
            
    

# Run the application 
shinyApp(ui = ui, server = server)
