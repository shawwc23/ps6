

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
                 sidebarLayout(sidebarPanel(p("You can analyze the global temperature for different regions. 
                                              Select the regions you are interested in. You see a monthly 
                                              scatterplot and the corresponding trend lines."),
                                            radioButtons("colors", label = "Palette:",
                                                         c("standard", "set2")),
                                            checkboxGroupInput("reg", "Select regions",
                                                               choices = unique(globe$region),
                                                               selected = "globe")), 
                               mainPanel(plotOutput(),
                                         textOutput()))),
        tabPanel("Tables",
                 sidebarLayout(
                   sidebarPanel(
                     p("This panel displays average temperature over different time periods: ", 
                       em("months"), ", ", em("years"), ", and ", em("decades")),
                     radioButtons("range", label = "Average over:",
                                  c("month",
                                    "year",
                                    "decade"
                                    ))),
                   mainPanel(textOutput("vals"),
                             dataTableOutput("tableone"))
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
  
  df1 = reactive({
    if(input$colors == "standard") {
      cols = c(1)
    }
  })
  
  output$plotone = renderPlot({
    p1 = df1()
    p1
  })
  
  df = reactive({
      if(input$range == "year") {
          data = globe %>% 
            group_by(year) %>% 
            summarise(ave_temp = mean(temp)) %>% 
            arrange(year)
          } else if(input$range == "month") {
          data = globe %>% 
              group_by(year, month) %>% 
              summarise(ave_temp = mean(temp)) %>% 
              arrange(year)
          } else {
          data = globe %>% 
             mutate(decade = floor(year/10)*10) %>% 
             group_by(decade) %>% 
             summarize(ave_temp = mean(temp))
          }
      themin = min(data$ave_temp)
      themax = max(data$ave_temp)
      data
  })
  output$vals = renderText({
    paste("Temperature range ", df$themin, " to ", df$themax)
  })
  output$tableone <- renderDataTable({
      d1 = df()
      d1
  })

}

shinyApp(ui = ui, server = server)
