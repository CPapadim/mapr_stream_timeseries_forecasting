import os
import pandas as pd
import csv
import numpy as np
import random
import random

import tensorflow as tf
import shutil
import tensorflow.contrib.learn as tflearn
import tensorflow.contrib.layers as tflayers
from tensorflow.contrib.learn.python.learn import learn_runner
import tensorflow.contrib.metrics as metrics
import tensorflow.contrib.rnn as rnn


def rnn_model(array, num_periods):
    
    #Convert the input list to the format the model expects
    array = np.array([[[i] for i in array]])
    
    x_data = array.reshape(-1,num_periods,1)
    #print (x_data)
    tf.reset_default_graph()   #We didn't have any previous graph objects running, but this would reset the graphs

    inputs = 1            #number of vectors submitted
    hidden = 100          #number of neurons we will recursively work through, can be changed to improve accuracy
    output = 1            #number of output vectors

    X = tf.placeholder(tf.float32, [None, num_periods, inputs], name = "X")   #create variable objects
    y = tf.placeholder(tf.float32, [None, num_periods, output], name = "y")

    basic_cell = tf.contrib.rnn.BasicRNNCell(num_units=hidden, activation=tf.nn.relu)   #create our RNN object
    rnn_output, states = tf.nn.dynamic_rnn(basic_cell, X, dtype=tf.float32)               #choose dynamic over static

    learning_rate = 0.001   #small learning rate so we don't overshoot the minimum
    stacked_rnn_output = tf.reshape(rnn_output, [-1, hidden])           #change the form into a tensor
    stacked_outputs = tf.layers.dense(stacked_rnn_output, output)        #specify the type of layer (dense)
    outputs = tf.reshape(stacked_outputs, [-1, num_periods, output])          #shape of results

    loss = tf.reduce_sum(tf.square(outputs - y))    #define the cost function which evaluates the quality of our model
    optimizer = tf.train.AdamOptimizer(learning_rate=learning_rate)          #gradient descent method
    training_op = optimizer.minimize(loss)          #train the result of the application of the cost_function                                 

    init = tf.global_variables_initializer()      #initialize inputs
    saver = tf.train.Saver()                      #specify saver function
    DIR="model"                  #directory where trained TF model is saved

    with tf.Session() as sess:                    #start a new tensorflow session
        saver.restore(sess, os.path.join(DIR,"RWsensorTFmodel-1000"))    #restore model         
        y_pred = sess.run(outputs, feed_dict={X: x_data})      #load data from streams
        FORECAST = y_pred[:,(num_periods-1):num_periods]       #only print out the last prediction, which is the forecast for next period
        
    #Convert the forecast to a serializable format
    FORECAST = [i[0] for j in FORECAST for i in j]
    return (FORECAST)