

library(shiny)
library(dplyr)
library(tidyverse)
globe = read_delim("UAH-lower-troposphere-long.csv.bz2")
ui <- fluidPage(
    titlePanel("Global Temperature Data"),
    
    mainPanel(
      tabsetPanel(
        tabPanel("About", p("This app uses satellite temperature data from ", strong("UAH")),
                 p("Temperature ", em("temp"), " is measured as deviation (deg C) from 1991-2020 baseline"),
                 p("The dataset contains ", nrow(globe), " observations and ", ncol(globe), " variables
                    Here is a small (random) sample of data:"), dataTableOutput("sample")),
        tabPanel("Plot",
                 sidebarLayout(sidebarPanel(p("yup")), mainPanel(p("nope")))),
        tabPanel("Tables",
                 sidebarLayout(
                   sidebarPanel(
                     p("This panel displays average temperature over different time periods: ", 
                       em("months"), " , ", em("years"), ", and ", em("decades")),
                     radioButtons("range", label = "Average over:",
                                  c(""))),
                        
                   mainPanel(p("Temperature range ", min(globe$temp), " - ", max(globe$temp)),
                             tableOutput("tableone"))
                 )
      )
    )
    )
)


server <- function(input, output) {
  output$sample <- renderDataTable({
    globe %>%
      sample_n(5)
  })
  
  
output$tableone = renderDataTable({
  globe %>% 
    
    arrange(desc())
})

  
  
}

shinyApp(ui = ui, server = server)
