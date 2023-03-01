

library(shiny)
library(dplyr)
library(tidyverse)
read_delim("UAH-lower-troposphere-long.csv.bz2")
ui <- fluidPage(

    titlePanel("Global Temperature Data"),

    sidebarLayout(
        sidebarPanel(
        
        
        ),

        mainPanel(
           plotOutput("distPlot")
        )
    )
)


server <- function(input, output) {

    output$distPlot <- renderPlot({
        x    <- faithful[, 2]
        bins <- seq(min(x), max(x), length.out = input$bins + 1)

        hist(x, breaks = bins, col = 'darkgray', border = 'white',
             xlab = 'Waiting time to next eruption (in mins)',
             main = 'Histogram of waiting times')
    })
}

shinyApp(ui = ui, server = server)
