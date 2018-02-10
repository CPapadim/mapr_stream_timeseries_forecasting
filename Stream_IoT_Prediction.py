print ("Importing dependencies....")
import mapr_kafka_rest
import os
import time
import datetime
import json
import requests
import csv
import pandas as pd
import numpy as np

import tensorflow as tf
import shutil
import tensorflow.contrib.learn as tflearn
import tensorflow.contrib.layers as tflayers
from tensorflow.contrib.learn.python.learn import learn_runner
import tensorflow.contrib.metrics as metrics
import tensorflow.contrib.rnn as rnn
print("Import complete.\n")

with open('cred_file.txt', 'r') as f: # Keep the cred_file private!
    credential = f.readline().strip


def sensor_conversion(record):
    sensor_frame = pd.DataFrame()
    sensor_frame = sensor_frame.append(record,ignore_index=True)
    sensor_frame['TimeStamp']= pd.to_datetime(sensor_frame['TimeStamp'])#.dt.strftime('%Y-%m-%d %H:%M:%S.%f')
    sensor_frame.sort_values(['TimeStamp'], ascending=True)
    sensor_frame['Total']=sensor_frame.select_dtypes(include=['float64','float32']).apply(lambda row: np.sum(row),axis=1)
    if (not os.path.isfile("IoT_Data_From_Sensor.csv")):   
            sensor_frame.to_csv("IoT_Data_From_Sensor.csv")     #if csv is not there, create it
    else:
        with open('IoT_Data_From_Sensor.csv', 'a') as newFile:
            newFileWriter = csv.writer(newFile)
            newFileWriter.writerow(sensor_frame.tail(1))   #if csv is there, append new row to file
    return (sensor_frame)


def rnn_model(sensor_readings):
   
    # Send request to Tensorflow Model ReST API
    body = requests.post('https://demo-next.datascience.com/deploy/deploy-anomalous-scara-arm-position-detector-380392-v3/', 
        json={"array": sensor_readings},
        cookies={
            'datascience-platform': credential
        },
    )
    
    return json.loads(body.text)[0]


#IoT_Demo_Topic = '/user/user01/iot_stream:sensor_record' 

def stream_data(topic_name, max):
    
    topic_partition = 0
    topic_offset = 0
    min_offset = 1
    
    df = pd.DataFrame()
    total_list_for_RNN = []
    num_periods = 100  #number of periods entered into batch
    
    for i in range(0,max):
        
        # Get message
        topic_offset = min_offset + i 
        message = mapr_kafka_rest.get_topic_message(topic_name, topic_partition, topic_offset)
        
        # Reconstitue the dataframe record by unpacking the message...
        msg_as_list  = json.loads(message)
        json_as_dict = msg_as_list[0]
        df_record = json_as_dict['value']
        df = df.append(sensor_conversion(df_record),ignore_index=True)
        df['TimePeriod'] = df.index + 1
        
        
        if len(df) < num_periods: # Wait until we've streamed at least 100 data points in our timeseries
            x1 = df["TimePeriod"].iloc[-1]
            y1 = int(df["Total"].iloc[-1])
            x2 = df["TimePeriod"].iloc[-1] + 1
            y2 = 0
            s_1.write(dict(x=x1,y=y1))
            s_2.write(dict(x=x2,y=y2))
            total_list_for_RNN.append((df["Total"].iloc[-1])) 
        else:
            total_list_for_RNN.append((df["Total"].iloc[-1]))
            total_metric_array = np.array(total_list_for_RNN)
            
            predicted_value = rnn_model(total_metric_array, num_periods)
            
            x1 = df["TimePeriod"].iloc[-1]
            y1 = int(df["Total"].iloc[-1])
            x2 = df["TimePeriod"].iloc[-1] + 1
            y2 = int(predicted_value)
            s_1.write(dict(x=x1,y=y1))
            s_2.write(dict(x=x2,y=y2))

            print ("Next timestamp aggregate metric prediction: " + str(predicted_value))
            if (predicted_value < 450) or (predicted_value > -200) :
                print ("Forecast does not exceed threshold for alert!\n")
            else:
                print ("Forecast exceeds acceptable threshold - Alert Sent!\n")
            del total_list_for_RNN[0]


IoT_Demo_Topic = '/user/user01/iot_stream:sensor_record' 
max = 402
DIR="/user/user01/rwTFmodel" 

stream_tokens = tls.get_credentials_file()['stream_ids']
token_1 = stream_tokens[0]   # I'm getting my stream tokens from the end to ensure I'm not reusing tokens
token_2 = stream_tokens[1]

stream_id1 = dict(token=token_1, maxpoints=60)
stream_id2 = dict(token=token_2, maxpoints=60)









