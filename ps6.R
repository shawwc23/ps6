

library(shiny)
library(dplyr)
library(tidyverse)
globe = read_delim("UAH-lower-troposphere-long.csv.bz2")
ui <- fluidPage(titlePanel("Global Temperature Data"),
                
                mainPanel(tabsetPanel(
                  tabPanel(
                    "About",
                    p("This app uses satellite temperature data from ", strong("UAH")),
                    p(
                      "Temperature ",
                      em("temp"),
                      " is measured as deviation (deg C) from 1991-2020 baseline"
                    ),
                    p(
                      "The dataset contains ",
                      nrow(globe),
                      " observations and ",
                      ncol(globe),
                      " variables
                    Here is a small (random) sample of data:"
                    ),
                    tableOutput("sample")
                  ),
                  tabPanel("Plot",
                           sidebarLayout(
                             sidebarPanel(
                               p(
                                 "You can analyze the global temperature for different regions.
                                              Select the regions you are interested in. You see a monthly
                                              scatterplot and the corresponding trend line."
                               ),
                               checkboxInput("fit", "Add line of best fit", value = FALSE),
                               radioButtons("colors", label = "Palette:",
                                            c("standard", "set2")),
                               uiOutput("check")),
                             mainPanel(plotOutput("plotone"),
                                     textOutput("vals1"))
                             
                           )),
                  tabPanel("Tables",
                           sidebarLayout(
                             sidebarPanel(
                               p(
                                 "This panel displays average temperature over different time periods: ",
                                 em("months"),
                                 ", ",
                                 em("years"),
                                 ", and ",
                                 em("decades")
                               ),
                               radioButtons("range", label = "Average over:",
                                            c("month",
                                              "year",
                                              "decade"))
                             ),
                             mainPanel(textOutput("vals"),
                                       tableOutput("tableone"))
                           ))
                )))


server <- function(input, output) {
  output$sample <- renderTable({
    globe %>%
      sample_n(5)
  })
  
  output$check = renderUI({
    checkboxGroupInput("reg", "Select Regions",
                       choices = c(unique(globe$region)), selected = "globe")
  })
  
  b = reactive({
    o = globe %>% 
      filter(region %in% input$reg)
    o
  })
  
  output$plotone = renderPlot({
    p <- ggplot(b(), aes(year, temp)) +
      ggtitle("Temperature deviation against time") +
      geom_point(aes(color = region)) +
      theme(legend.position = "right") +
      ylab("Temperature: deviation from 1991-2020 baseline, deg C")

    if (input$colors == "standard") {
      p = p + scale_color_hue(h = c(180, 300))
    } else {
      p = p + scale_color_hue(h = c(1, 180))
    }
    
    if (input$fit == TRUE) {
      p <- p + geom_smooth(method = "lm")
    }
    p
  })
  
  num = reactive({
    b() %>% 
      summarise(n = n())
  })
  
  output$vals1 = renderText({
    paste("Time period 1978 - 2023 . In total, ",
          num(),
          " non-missing observations")
  })
  
  df = reactive({
    if (input$range == "year") {
      data = globe %>%
        group_by(year) %>%
        summarise(ave_temp = round(mean(temp), 2)) %>%
        arrange(year)
    } else if (input$range == "month") {
      data = globe %>%
        group_by(year, month) %>%
        summarise(ave_temp = round(mean(temp), 2)) %>%
        arrange(year)
    } else {
      data = globe %>%
        mutate(decade = floor(year / 10) * 10) %>%
        group_by(decade) %>%
        summarize(ave_temp = round(mean(temp), 2))
    }
    data
  })
  
  dfmax = reactive({
    df() %>%
      pull(ave_temp) %>%
      max()
  })
  
  dfmin = reactive({
    df() %>%
      pull(ave_temp) %>%
      min()
  })
  
  output$vals = renderText({
    paste("Temperatures range from ", dfmin(), " to ", dfmax())
  })
  
  output$tableone <- renderTable({
    d1 = df()
    d1
  })
  
}

shinyApp(ui = ui, server = server)
