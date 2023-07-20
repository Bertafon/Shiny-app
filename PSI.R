# Computer Systems Design - project
# Norbert Burmistrzak 2020

library(shiny)
library(readxl)
library(ggplot2)
library(shinythemes)

df <- read_excel('data.xlsx', sheet = "data")

text = "The application shows graphs of economic indicators for Italy, which I used to prepare a paper on macroeconomics.
        The user can select the variable and the date range for which he will see the data."

indicators = c("Savings (%GDP)","Current account (%GDP)","Foreign direct investment","Import","Export")

ui <- fluidPage(  
                  
  theme = shinytheme("flatly"),
  titlePanel("Macroeconomic indicators for Italy"), 
                   
    sidebarLayout(
      sidebarPanel(
        selectInput("plot", label = "Select a chart:",
                    choices = indicators, 
                    selected = "1"),
        sliderInput("years", label = "Years range:",
                    min = 1998, max = 2018, value = c(1998,2018), step = 1), 
        br(),
        helpText(text)
    ),  
                      
      mainPanel(
          plotOutput("df"), 
          br(),
          dataTableOutput('table')
          
      )))

server <- function(input, output) { 

    output$df <- renderPlot({
      
      x <- switch(input$plot, 
                   "Savings (%GDP)" = df$Savings,
                   "Current account (%GDP)" = df$CA,
                   "Foreign direct investment" = df$FDI,
                   "Import" = df$Import, 
                   "Export" = df$Export)
                   
      
      y <- switch(input$plot, 
                  "Savings (%GDP)" = NULL,
                  "Current account (%GDP)" = NULL,
                  "Foreign direct investment" = "billions USD" ,
                  "Import" = "billions USD", 
                  "Export" = "billions USD")
      
      ggplot(df, aes(Year, x)) +
        geom_line(data = df) +
        labs(title = input$plot) +
        scale_x_continuous(limits=c(input$years[1], input$years[2])) +
        xlab(NULL) + 
        ylab(y)
      
    })
    output$table <- renderDataTable(df, options = list(pageLength = 5, lengthMenu = c(5,10,15)))
    
  }
shinyApp(ui = ui, server = server)
