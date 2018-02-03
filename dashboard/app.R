#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#
#install.packages('rjson')
#install.packages('lubridate')
#install.packages('DT')
#install.packages('plotly')
library(RCurl)
library(stringr)
library(lubridate)
library(shiny)
library(DT)
require(rjson)
require(dplyr)
require(plotly)


url = 'https://demo-next.datascience.com/deploy/deploy-anomalous-scara-arm-position-detector-380392-v1/'
hdr=c(`Cookie`=paste0('datascience-platform=',Sys.getenv('MODEL_CREDENTIAL')), `Content-Type`="application/json")

json = toJSON(list(array = c(1,2,3,4,5,6,7,8,9,1,1,2,3,4,5,6,7,8,9,1,1,2,3,4,5,6,7,8,9,1,1,2,3,4,5,6,7,8,9,1,1,2,3,4,5,6,7,8,9,1), 
                   num_periods = 5))

get_prediction <- function(data_stream, num_periods) {
  
  model_input = toJSON(list(array = data_stream, 
                            num_periods = num_periods))
  req <- httr::POST(url,body = model_input,
                    add_headers(
                      `Content-Type` = 'application/json',
                      `Cookie` = paste0('datascience-platform=',Sys.getenv('MODEL_CREDENTIAL'))))
  
  return(fromJSON(httr::content(req, as = "text")))
  
}


get_data <- function() {
  return(runif(5, 0, 20))
}






liveish_data <- reactive({
  invalidateLater(100)
  stream_data = get_data()
  model_predictions = get_predictions(stream_data, 5)
})
# Define UI for application that draws a histogram
ui <- fluidPage(
   
   # Application title
   titlePanel("Old Faithful Geyser Data"),
   
   # Sidebar with a slider input for number of bins 
   sidebarLayout(
      sidebarPanel(
         sliderInput("bins",
                     "Number of bins:",
                     min = 1,
                     max = 50,
                     value = 30)
      ),
      
      # Show a plot of the generated distribution
      mainPanel(
         plotOutput("distPlot")
      )
   )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
   
   output$distPlot <- renderPlotly({
      # generate bins based on input$bins from ui.R
     x <- length(liveish_data())
     y <- liveish_data()
     data <- data.frame(x, random_y)
     
     p <- plot_ly(data, x = ~x, y = ~y, type = 'scatter', mode = 'lines')
   })
}

# Run the application 
shinyApp(ui = ui, server = server)

