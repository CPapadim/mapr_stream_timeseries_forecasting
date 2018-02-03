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

library(RCurl)
library(stringr)
library(lubridate)
library(shiny)
library(DT)
require(rjson)
require(dplyr)
require(plotly)


url = 'https://demo-next.datascience.com/deploy/deploy-anomalous-scara-arm-position-detector-380392-v1/'
#url = 'https://jenkins.datascience.com/job/datascienceinc/job/platform/api/json?tree=jobs[url]'
json = toJSON(list(array = c(1,2,3,4,5,6,7,8,9,1,1,2,3,4,5,6,7,8,9,1,1,2,3,4,5,6,7,8,9,1,1,2,3,4,5,6,7,8,9,1,1,2,3,4,5,6,7,8,9,1), 
                   num_periods = 5))

hdr=c(`Cookie`=paste0('datascience-platform=',Sys.getenv('MODEL_CREDENTIAL')), `Content-Type`="application/json")
req <- postForm(url,
         .opts=list(httpheader=hdr, postfields=json))


h <- new_handle()
#handle_setopt(h, copypostfields = "moo=moomooo");
handle_setform(h,
               array = '[1,2,3,4,5,6,7,8,9,1,1,2,3,4,5,6,7,8,9,1,1,2,3,4,5,6,7,8,9,1,1,2,3,4,5,6,7,8,9,1,1,2,3,4,5,6,7,8,9,1]',
               num_periods = '5')

#,
#'Cookie' = paste0('datascience-platform=',Sys.getenv('MODEL_CREDENTIAL'))
handle_setheaders(h,
                  'Content-Length' = '20000'
                  'Content-Type' = "application/json",
                  'Cookie' = paste0('datascience-platform=',Sys.getenv('MODEL_CREDENTIAL'))
)
req <- curl_fetch_memory(url, handle = h)

req <- httr::POST(url,body = json,
                 #set_cookies(`datascience-cookie` = Sys.getenv('MODEL_CREDENTIAL')),
                 add_headers(
                             `Content-Type` = 'application/json',
                             `Cookie` = paste0('datascience-platform=',Sys.getenv('MODEL_CREDENTIAL'))))

fromJSON(httr::content(req, as = "text"))


req <- POST("http://api.scb.se/OV0104/v1/doris/sv/ssd/START/PR/PR0101/PR0101A/KPIFastM2", 
            body = '{ "query": [], "response": { "format": "json" } }')

branch_dat = fromJSON(httr::content(req, as = "text"))[[2]]

liveish_data <- reactive({
  invalidateLater(100)
  httr::GET(...)
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
   
   output$distPlot <- renderPlot({
      # generate bins based on input$bins from ui.R
      x    <- faithful[, 2] 
      bins <- seq(min(x), max(x), length.out = input$bins + 1)
      
      # draw the histogram with the specified number of bins
      hist(x, breaks = bins, col = 'darkgray', border = 'white')
   })
}

# Run the application 
shinyApp(ui = ui, server = server)

