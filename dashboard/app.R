#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#
install.packages('rjson')
install.packages('lubridate')
install.packages('DT')
install.packages('plotly')
install.packages('dygraphs')
install.packages('flexdashboard')
#install.packages('ggvis')
install.packages("shinymaterial")

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
library(flexdashboard)
library(ggvis)
library(tidyr)
library(shinymaterial)



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


url = 'https://demo-next.datascience.com/deploy/deploy-anomalous-scara-arm-position-detector-380392-v3/'
#url = 'https://mapr-demo.datascience.com/deploy/deploy-scara-robot-anomaly-detector-24864-v3/'
hdr=c(`Cookie`=paste0('datascience-platform=',Sys.getenv('MODEL_CREDENTIAL_2')), `Content-Type`="application/json")

#json = toJSON(list(array = c(1,2,3,4,5,6,7,8,9,1,1,2,3,4,5,6,7,8,9,1,1,2,3,4,5,6,7,8,9,1,1,2,3,4,5,6,7,8,9,1,1,2,3,4,5,6,7,8,9,1)))

get_prediction <- function(data_stream) {
  
  model_input = toJSON(list(array = data_stream))
  req <- httr::POST(url,body = model_input, config(ssl_verifypeer = 0L),
                    add_headers(
                      `Content-Type` = 'application/json',
                      `Cookie` = paste0('datascience-platform=',Sys.getenv('MODEL_CREDENTIAL'))))
  
  return(fromJSON(httr::content(req, as = "text", encoding = 'UTF-8')))
  
}


get_data <- function(val_range) {
  return(runif(100, 0, val_range)) 
}


user_inp_hold <- reactiveValues(from_stream = FALSE)
predictions_all <- vector('numeric')
actual_all <- vector('numeric')
ap_diff <- vector('numeric')
liveish_data <- reactive({
  if (user_inp_hold$from_stream) {
    data_stream = get_data(5)
  } else {
    data_stream = get_data(25)
  }
  model_predictions = get_prediction(data_stream)
  predictions_all <<- c(predictions_all, model_predictions)
  actual_all <<- c(actual_all, mean(data_stream))
  ap_diff <<- c(ap_diff, (model_predictions - mean(data_stream)))
  #line = readLines(s3_data_stream, n=1)
  #predictions_all <<- c(predictions_all, strsplit(line, ',')[[1]][10])
  if (length(predictions_all) > 200) {
    predictions_all <<- tail(predictions_all, 200)
    actual_all <<- tail(actual_all, 200)
    ap_diff <<- tail(ap_diff, 200)
  }
  invalidateLater(100)
  list(predictions_all, actual_all, ap_diff)
})


# Define UI for application that draws a histogram
ui <- fluidPage(
  
  #Disable graying out while refreshing realtime plots
  tags$style(type="text/css",
             ".recalculating {opacity: 1.0;}",
             "#status_text {text-align:center;}",
             "#actpredPlot {height:300px !important;}",
             ".stream_switch div {display:inline;}",
             ".stream_switch p {display:inline; font-weight:bold;}",
             ".stream_switch h1 {display:inline; padding-right: 100px}"
  ),
   # Application title
  #tags$div(class = 'stream_switch',
    #h1("SCARA Robot Status"),
    #p('From Storage'),
    #uiOutput("from_stream"), #materialSwitch(inputId = "from_stream", label = "", status = "primary", right = TRUE),
    #p('From Stream')
  #),
   fluidRow(
     material_page(
       title = "SCARA Robot Status", 
       background_color = "#ffffff",
       nav_bar_color = "blue lighten-1",
       material_row(
         material_column(
           tags$div(class = 'stream_switch',
             br(),
             p('From Storage'),
             uiOutput("from_stream"),
             p('From Stream')
           )
         )
        )
      )
     ),
   fluidRow(
    column(width = 8,
       dygraphOutput("actpredPlot")
       
    ),
    column(width = 4,
       h3(textOutput("status_text")),
       gaugeOutput("gauge")
    )
    
   ),
  fluidRow(
    br(),
    br(),
    plotlyOutput("maintenance")
  )
)


# Define server logic required to draw a histogram
server <- function(input, output) {
   
  observeEvent(input$from_stream,
               {
                 user_inp_hold$from_stream <<- input$from_stream
               })
  output$from_stream = renderUI({
    material_switch(input_id = "from_stream", label = "", off_label = "", on_label = "",
                    initial_value = FALSE, color = NULL)
  })
  
  
   #output$actpredPlot <- renderPlotly({
   output$actpredPlot <- renderDygraph({

      # generate bins based on input$bins from ui.R
     x <- c(1:length(liveish_data()[[1]]))
     #y <- liveish_data()[[1]]
     #z <- liveish_data()[[2]]
     diff = liveish_data()[[3]]

     #data <- data.frame(x, y)
     #p <- plot_ly(data, x = ~x, y = ~y, type = 'scatter', mode = 'lines')
     #data <- cbind(ts(y, x), ts(z,x), ts(diff, x))
     #colnames(data) = c('pred', 'act', 'diff')
     data <- ts(diff, x)
     #colnames(data) = c('diff')

     ticker_func = "function(){ return  [{v: 0, label: '0'}, {v: 2, label: '2'}, {v: -2, label: '-2'}]; }"
     dy_plot = dygraph(data)  %>%
       dyOptions(drawGrid = FALSE, stemPlot = TRUE, drawXAxis = FALSE, 
                 rightGap = 20, strokeWidth = 2) %>%
       dyAxis('x', drawGrid = FALSE) %>%
       dyAxis('y', valueRange = c(-5, 5), axisLineWidth = 5.0, 
              axisLineColor = rgb(0.7,0.7,0.7),
              ticker = ticker_func) %>%
       dyLimit(-4, color = rgb(0.85, 0.4, 0.4), label = "Anomaly Threshold") %>% 
       dyLimit(4, color = rgb(0.85, 0.4, 0.4)) %>%
       dyLimit(0, color = rgb(0.85, 0.85, 0.85))

     })
   
   
  output$maintenance = renderPlotly({
    x <- c(1,2, 5, 8, 11, 12, 14, 15, 16, 17, 20, 22)
    y <- c(3, 1, 2, 1, 2, 4, 1, 2, 3, 1, 4, 1)
    p <- plot_ly(x = x, y = y, type = 'bar', height = 300) %>% 
      layout(title = 'Scheduled for Maintenance', 
             yaxis = list(autotick = F, range = c(0, 4), dtick = 2, title = "# Robots"),
             xaxis = list(showticklabels = FALSE))
  })
   output$gauge = renderGauge({
     x <- liveish_data()[[1]]
     perc_outlier <- round(100*(sum(x > 5) / length(x)), digits = 1)
     gauge(perc_outlier,
           min = 0, 
           max = 100, 
           sectors = gaugeSectors(success = c(0, 25), 
                                  warning = c(25, 50),
                                  danger = c(50, 100)),
           symbol = '%'
     )
   })
   
   output$status_text = renderText({
     x <- liveish_data()[[1]]
     perc_outlier <- round(100*(sum(x > 5) / length(x)), digits = 1)
     
     if (perc_outlier <= 25) {
       health_label = 'Healthy'
     } else if (perc_outlier <= 50) {
       health_label = 'Warning!'
     } else {health_label = 'Failure!!'}
     health_label
     
   })
   
  
}

# Run the application 
shinyApp(ui = ui, server = server)

