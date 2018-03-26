# IoT Streaming Data and Deep Learning for Predictive Maintenance

![](https://s26.postimg.org/7u7u7uzxl/scara-robot-pred-maintenance-overview.png)

When dealing with hardware devices that drive a business, hardware failure can be a big cost. Corrective maintenance after hardware has failed can lead to unexpected downtimes of services, and inefficient maintenance operations. On the other hand, preventative maintenance can lead to higher costs when prevenatively servicing hardware that is in good condition.

A better approach is to use **predictive maintenance**. If we can monitor our hardware and predict when it will fail, we can schedule maintenance before the failure, avoiding unexpected downtimes, allowing for efficient maintenance scheduling, and saving on cost of unnecessary service.

In this project we use streaming IoT sensor data to track the state of a SCARA (Selective Compliance Assembly Robot Arm) Robot. We then train a **recurrent neural network model with Tensorflow** to detect anomalies that may indicate calibration errors and impending failure.

When anomalies are detected, alerts can be sent to trigger maintenance request before the anomalous readings lead to failures.

# Files

**Put data on MapR cluster.ipynb** - Put the data used in this project on the MapR Cluster.  Only needs to be done once for each MapR Cluster

**Sensor_ETL.ipynb** - Uses DataScience.com Platform ot kicks off an ETL job on the MapR Cluster with the sensor data.  Transforms data from XML to CSV and repartitions it from multiple files to a single file.

**Historical_Analysis_and_Model_Training.ipynb** - Explore data, engineer features, and train and serialize a tensorflow model on historical ('healthy') SCARA Robot data from the sensor stream.  Model is trained on DataScience.com container rather than the MapR Cluster since the neural network can be trained on data batches and doesn't require the full data at once.

**deploy-model.py** - Deploy the serialized tensorflow model on DataScience.com ReST API

> Input:<br>
>> array of the last 100 readings of the sensor
  
> ```
{"array: [1,2,3,4,5,1,2,3,4,5,1,2,3,4,5,1,2,3,4,5,1,2,3,4,5,1,2,3,4,5,1,2,3,4,5,1,2,3,4,5,1,2,3,4,5,1,2,3,4,5,1,2,3,4,5,1,2,3,4,5,1,2,3,4,5,1,2,3,4,5,1,2,3,4,5,1,2,3,4,5,1,2,3,4,5,1,2,3,4,5,1,2,3,4,5,1,2,3,4,5]}
```

> Output:<br>
>> model prediction for what the next sensor reading should be if the SCARA Robot is behaving properly and is healthy
   
**model/  directory** - Contains the saved model weights of the tensorflow model

**dasboard/ directory** - Contains a health dashboard that shows how 




