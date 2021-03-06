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
#install.packages('flexdashboard')
#install.packages("shinymaterial")


library(dygraphs)
library(stringr)
library(lubridate)
library(shiny)
library(DT)
require(rjson)
require(dplyr)
require(plotly)
library(httr)
library(flexdashboard)
library(tidyr)
library(shinymaterial)

anomaly_thresh = 40

###################
# S3 Connectivity #
###################

# pip install awscli --upgrade --user
# export PATH=~/.local/bin:$PATH
# Set these and aws cli will know to use them:
#    AWS_ACCESS_KEY_ID
#    AWS_SECRET_ACCESS_KEY

# Data URL
# data_url = 's3://ds-cloud-cso/mapr-demo/part-00000-45866095-f76d-4f6c-ba2d-a07f0ab2dc04.csv'

# Command to Get an S3 file
# s3_command = paste0('~/.local/bin/aws s3 cp ', data_url, ' -') # The dash at the end creates a stream rather than downloading

# Command to Get a piece of a file
# s3_command = paste0('~/.local/bin/aws s3api get-object --bucket ds-cloud-cso --key mapr-demo/part-00000-45866095-f76d-4f6c-ba2d-a07f0ab2dc04.csv --range bytes=0-99999 my_data_range')

# Open a connection to the s3 file
# s3_data_stream = pipe(s3_command, open = 'r')
# csv_dat = read.csv(s3_data_stream)


# Read one line
# readLines(s3_data_stream, n=1)

# Read a range of lines
# scan(s3_data_stream, n=1, skip = 5, what= "char(0)", sep = "\n", quiet = TRUE)

#########################
# S3 Connectivity - End #
#########################



url = Sys.getenv('MODEL_ENDPOINT')
hdr=c(`Cookie`=paste0('datascience-platform=',Sys.getenv('MODEL_CREDENTIAL')), `Content-Type`="application/json")

# For deploying dashboard
data_dir = '/tmp/mapr_stream_timeseries_forecasting/tmp/data/'
if (length(list.files(data_dir)) == 0) {
  # For testing in RStudio (paths are different in deploy sessions)
  data_dir = '~/mapr_stream_timeseries_forecasting/tmp/data/'
}

data_from_file = read.csv(paste0(data_dir, 'part-00000-45866095-f76d-4f6c-ba2d-a07f0ab2dc04.csv') , stringsAsFactors = FALSE)
data_from_file = data_from_file[ , -which(names(data_from_file) %in% c('X...scararobot.PositionCommand',
                                                           'X...scararobot.Ax_J1.TorqueFeedback',
                                                           'X...scararobot.Ax_J2.PositionCommand',
                                                           'X...scararobot.Ax_J2.TorqueFeedback',
                                                           'X...scararobot.Ax_J3.TorqueFeedback',
                                                           'X...scararobot.Ax_J6.TorqueFeedback',
                                                           'X...scararobot.ScanTimeAverage',
                                                           'X...scararobot.Ax_J6.PositionCommand',
                                                           'X...scararobot.Ax_J3.PositionCommand'))]

data_from_file = data_from_file[data_from_file["X...scararobot.speed"] != 0,]
agg_dat = data.frame('Timestamp' = format(as.POSIXct(gsub("T", " ", substr(data_from_file[,1], 1, 23))), "%Y-%m-%d %H:%M:%OS6"),
                     'Aggregate_Readigns' = data.frame(rowSums(data_from_file[,2:length(data_from_file)])))
colnames(agg_dat) = c('Timestamp', 'Aggregate_Readings')
agg_dat = agg_dat[order(agg_dat$Timestamp),]


get_prediction <- function(data_stream) {
  
  model_input = toJSON(list(array = data_stream))
  req <- httr::POST(url,body = model_input, config(ssl_verifypeer = 0L),
                    add_headers(
                      `Content-Type` = 'application/json',
                      `Cookie` = paste0('datascience-platform=',Sys.getenv('MODEL_CREDENTIAL'))))
  
  return(fromJSON(httr::content(req, as = "text", encoding = 'UTF-8')))
  
}


start_idx = 1
get_data <- function(start_idx, num_periods) {
  agg_dat[c(start_idx:(start_idx+num_periods-1)),2]
}


user_inp_hold <- reactiveValues(from_stream = FALSE)
predictions_all <- vector('numeric')
actual_all <- vector('numeric')
ap_diff <- vector('numeric')
num_periods = 100
liveish_data <- reactive({
  if (user_inp_hold$from_stream) {
    data_stream = get_data(start_idx, num_periods)
    actual = get_data(start_idx + num_periods, 1)
    
    # Get model prediction
    model_predictions <- NULL
    attempt <- 1
    while( is.null(model_predictions) && attempt <= 3 ) { # In case we have connection issues, try a few times then give up
                                                          # and move on to next data point
      attempt <- attempt + 1
      try({
        model_predictions = get_prediction(data_stream)
        
      })
    } 
    start_idx <<- start_idx + 1
    if (start_idx >= length(agg_dat[,1])-300) { # reset the stream when we get close to the end of the data
      start_idx <<- 1
    }

  } else {
    data_stream = 0
    model_predictions = 0
    actual = 0
  }
  predictions_all <<- c(predictions_all, model_predictions)
  actual_all <<- c(actual_all, actual)
  ap_diff <<- c(ap_diff, (model_predictions - actual))
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
             ".stream_switch h1 {display:inline; padding-right: 100px}",
             "#maintenance {height: 310px !important;}"
  ),
   fluidRow(
     material_page(
       title = "SCARA Robot Status", 
       background_color = "#ffffff",
       nav_bar_color = "blue lighten-1",
       material_row(
         material_column(
           tags$div(class = 'stream_switch',
             br(),
             p('Stream Off'),
             uiOutput("from_stream"),
             p('Stream On')
           )
         ),
         material_column(
           uiOutput("reset")
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
  
  observeEvent(input$reset_button,
               {
                 start_idx <<- 1
               })
  output$from_stream = renderUI({
    material_switch(input_id = "from_stream", label = "", off_label = "", on_label = "",
                    initial_value = user_inp_hold$from_stream, color = NULL)
  })
  
  output$reset = renderUI({
    material_button(input_id = "reset_button", label = "Reset", icon = "replay")
  })
  
   output$actpredPlot <- renderDygraph({
     tryCatch(
       {
           # Note:  tryCatch only recovers plot if the first line in this block
           # is a reactive value that does NOT cause an error
           # Otherwise the block won't ever update on its own
           
           x <- c(1:length(liveish_data()[[1]]))
           diff = liveish_data()[[3]]
           data <- ts(diff, x)
           

           ticker_func = paste0("function(){ return  [{v: 0, label: '0'}, {v: ", anomaly_thresh, ", label: ", anomaly_thresh, 
                                "}, {v: -", anomaly_thresh, ", label: '-", anomaly_thresh, "'}]; }")
           dy_plot = dygraph(data)  %>%
             dyOptions(drawGrid = FALSE, stemPlot = TRUE, drawXAxis = FALSE, 
                       rightGap = 20, strokeWidth = 2) %>%
             dyAxis('x', drawGrid = FALSE) %>%
             dyAxis('y', valueRange = c(-150, 150), axisLineWidth = 5.0, 
                    axisLineColor = rgb(0.7,0.7,0.7),
                    ticker = ticker_func) %>%
             dyLimit(-anomaly_thresh, color = rgb(0.85, 0.4, 0.4), label = "Anomaly Threshold") %>% 
             dyLimit(anomaly_thresh, color = rgb(0.85, 0.4, 0.4)) %>%
             dyLimit(0, color = rgb(0.85, 0.85, 0.85))
        }
       )
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
     
     tryCatch(
       {
         # Note:  tryCatch only recovers plot if the first line in this block
         # is a reactive value that does NOT cause an error
         # Otherwise the block won't ever update on its own
         diff <- liveish_data()[[3]]
         
         perc_outlier <- round(100*(sum(abs(diff) > anomaly_thresh) / length(diff)), digits = 1)
         gauge(perc_outlier,
               min = 0, 
               max = 100, 
               sectors = gaugeSectors(success = c(0, 25), 
                                      warning = c(25, 50),
                                      danger = c(50, 100)),
               symbol = '%'
         )
       }
     ) 
     
   })
   
   output$status_text = renderText({
     tryCatch(
       {
         # Note:  tryCatch only recovers plot if the first line in this block
         # is a reactive value that does NOT cause an error
         # Otherwise the block won't ever update on its own
         diff <- liveish_data()[[3]]
         perc_outlier <- round(100*(sum(abs(diff) > anomaly_thresh) / length(diff)), digits = 1)
         
         if (perc_outlier <= 25) {
           health_label = 'Healthy'
         } else if (perc_outlier <= 50) {
           health_label = 'Warning!'
         } else {health_label = 'Failure!!'}
         health_label
       }
     )
     
   })
   
  
}

# Run the application 
shinyApp(ui = ui, server = server)

