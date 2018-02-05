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
#install.packages('dygraphs')
#pip install awscli --upgrade --user
#export PATH=~/.local/bin:$PATH
# Set these and aws cli will know to use them:
#AWS_ACCESS_KEY_ID
#AWS_SECRET_ACCESS_KEY
library(dygraphs)
#library(RCurl)
library(stringr)
library(lubridate)
library(shiny)
library(DT)
require(rjson)
require(dplyr)
require(plotly)
library(httr)

#s3write_using(iris, FUN = write.csv,
#              bucket = "ds-cloud-cso",
#              object = "mapr-demo/tmp.csv")

#s3write_using(iris_hold, FUN = write.csv,
#              bucket = "ds-cloud-cso",
#              object = "mapr-demo/tmp.csv")


#scan(con,n,what="char(0)",sep="\n",quiet=TRUE,...)
data_url = 's3://ds-cloud-cso/mapr-demo/part-00000-45866095-f76d-4f6c-ba2d-a07f0ab2dc04.csv'
#data_url = 's3://ds-cloud-cso/mapr-demo/tmp.csv'

s3_command = paste0('~/.local/bin/aws s3 cp ', data_url, ' -') # The dash at the end creates a stream rather than downloading
#s3_command = paste0('~/.local/bin/aws s3api get-object --bucket ds-cloud-cso --key mapr-demo/part-00000-45866095-f76d-4f6c-ba2d-a07f0ab2dc04.csv --range bytes=0-99999 my_data_range')
s3_data_stream = pipe(s3_command, open = 'r')
#csv_dat = read.csv(s3_data_stream)
readLines(s3_data_stream, n=1) #Skip header

#scan(s3_data_stream, n=1, skip = 5, what= "char(0)", sep = "\n", quiet = TRUE)



# Strategy
# Pull 1024 bytes at a time.
# Use readline to read and plot each line.
# When we get to the last line pull from s3 again, keeping track of where we left off.
# Need to include some ovelapping bytes because last line will likely be broken in half.
# Readline on new file, and ignore all lines until the one where we left off.


#s3_command = paste0('~/.local/bin/aws s3api get-object --bucket ds-cloud-cso --key mapr-demo/part-00000-45866095-f76d-4f6c-ba2d-a07f0ab2dc04.csv --range bytes=0-200 my_data_range')
#s3_data_stream = pipe(s3_command, open = 'r')
#read.csv('my_data_range')[4,6]
#scan(s3_data_stream, n=1, skip = 5, what= "char(0)", sep = "\n", quiet = TRUE)

#hold = read.csv('my_data_range')


#data_url = 'https://ds-cloud-cso.s3.amazonaws.com/mapr-demo/part-00000-45866095-f76d-4f6c-ba2d-a07f0ab2dc04.csv'
#data_url = 'https://ds-cloud-cso.s3.amazonaws.com/mapr-demo/tmp.csv'
#s3_data_stream = url(data_url, blocking = FALSE, open = 'r')
#s3_data_stream = url(data_url)
#scan(s3_data_stream, n=151, what= "char(0)", sep = "\n", quiet = TRUE)


#url = 'https://demo-next.datascience.com/deploy/deploy-anomalous-scara-arm-position-detector-380392-v1/'
url = 'https://mapr-demo.datascience.com/deploy/deploy-scara-robot-anomaly-detector-24864-v1/'
hdr=c(`Cookie`=paste0('datascience-platform=',Sys.getenv('MODEL_CREDENTIAL_2')), `Content-Type`="application/json")

json = toJSON(list(array = c(1,2,3,4,5,6,7,8,9,1,1,2,3,4,5,6,7,8,9,1,1,2,3,4,5,6,7,8,9,1,1,2,3,4,5,6,7,8,9,1,1,2,3,4,5,6,7,8,9,1), 
                   num_periods = 5))

get_prediction <- function(data_stream, num_periods) {
  
  model_input = toJSON(list(array = data_stream, 
                            num_periods = num_periods))
  req <- httr::POST(url,body = model_input, config(ssl_verifypeer = 0L),
                    add_headers(
                      `Content-Type` = 'application/json',
                      `Cookie` = paste0('datascience-platform=',Sys.getenv('MODEL_CREDENTIAL'))))
  
  return(fromJSON(httr::content(req, as = "text", encoding = 'UTF-8')))
  
}


get_data <- function() {
  return(runif(100, 0, 5)) 
}



predictions_all <- vector('numeric')
liveish_data <- reactive({
  invalidateLater(100)
  data_stream = get_data()
  model_predictions = get_prediction(data_stream, 100)
  predictions_all <<- c(predictions_all, model_predictions)
  #line = readLines(s3_data_stream, n=1)
  #predictions_all <<- c(predictions_all, strsplit(line, ',')[[1]][10])
  if (length(predictions_all) > 500) {
    predictions_all <<- tail(predictions_all, 500)
  }
  predictions_all
})
# Define UI for application that draws a histogram
ui <- fluidPage(
  
  #Disable graying out while refreshing realtime plots
  tags$style(type="text/css",
             ".recalculating {opacity: 1.0;}"
  ),
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
         dygraphOutput("distPlot"),
         plotlyOutput("meterPlot")
      )
   )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
   
   output$distPlot <- renderDygraph({
      # generate bins based on input$bins from ui.R
     x <- c(1:length(liveish_data()))
     y <- liveish_data()
     #data <- data.frame(x, y)
     data <- ts(y, x)
     dygraph(data)
     #p <- plot_ly(data, x = ~x, y = ~y, type = 'scatter', mode = 'lines')
   })
   
   output$meterPlot <- renderPlotly({
     
     h = 0.24
     k = 0.5
     r = 0.15
     
     my_raw_value = 100
     theta = my_raw_value * 180/300
     theta = theta * pi / 180
     x = h + r*cos(theta)
     y = k + r*sin(theta)
     path = paste('M 0.235 0.5 L' , x, y, 'L 0.245 0.5 Z')
     base_plot <- plot_ly(
       type = "pie",
       values = c(40, 10, 10, 10, 10, 10, 10),
       labels = c("-", "0", "20", "40", "60", "80", "100"),
       rotation = 108,
       direction = "clockwise",
       hole = 0.4,
       textinfo = "label",
       textposition = "outside",
       hoverinfo = "none",
       domain = list(x = c(0, 0.48), y = c(0, 1)),
       marker = list(colors = c('rgb(255, 255, 255)', 'rgb(255, 255, 255)', 'rgb(255, 255, 255)', 'rgb(255, 255, 255)', 'rgb(255, 255, 255)', 'rgb(255, 255, 255)', 'rgb(255, 255, 255)')),
       showlegend = FALSE
     )
     base_plot <- add_trace(
       base_plot,
       type = "pie",
       values = c(50, 10, 10, 10, 10, 10),
       labels = c("Error Log Level Meter", "Debug", "Info", "Warn", "Error", "Fatal"),
       rotation = 90,
       direction = "clockwise",
       hole = 0.3,
       textinfo = "label",
       textposition = "inside",
       hoverinfo = "none",
       domain = list(x = c(0, 0.48), y = c(0, 1)),
       marker = list(colors = c('rgb(255, 255, 255)', 'rgb(232,226,202)', 'rgb(226,210,172)', 'rgb(223,189,139)', 'rgb(223,162,103)', 'rgb(226,126,64)')),
       showlegend= FALSE
     )
     a <- list(
       showticklabels = FALSE,
       autotick = FALSE,
       showgrid = FALSE,
       zeroline = FALSE)
     
     b <- list(
       xref = 'paper',
       yref = 'paper',
       x = 0.23,
       y = 0.45,
       showarrow = FALSE,
       text = '50')
     
     base_chart <- layout(
       base_plot,
       shapes = list(
         list(
           type = 'path',
           path = path,
           xref = 'paper',
           yref = 'paper',
           fillcolor = 'rgba(44, 160, 101, 0.5)'
         )
       ),
       xaxis = a,
       yaxis = a,
       annotations = b
     )
     
     
   })
}

# Run the application 
shinyApp(ui = ui, server = server)

