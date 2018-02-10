# IoT Streaming Data and Deep Learning for Predictive Maintenance

![](https://s26.postimg.org/7u7u7uzxl/scara-robot-pred-maintenance-overview.png)

When dealing with hardware devices that drive a business, hardware failure can be a big cost. Corrective maintenance after hardware has failed can lead to unexpected downtimes of services, and inefficient maintenance operations. On the other hand, preventative maintenance can lead to higher costs when prevenatively servicing hardware that is in good condition.

A better approach is to use **predictive maintenance**. If we can monitor our hardware and predict when it will fail, we can schedule maintenance before the failure, avoiding unexpected downtimes, allowing for efficient maintenance scheduling, and saving on cost of unnecessary service.

In this project we use streaming IoT sensor data to track the state of a SCARA (Selective Compliance Assembly Robot Arm) Robot. We then train a **recurrent neural network model with Tensorflow** to detect anomalies that may indicate calibration errors and impending failure.

When anomalies are detected, alerts can be sent to trigger maintenance request before the anomalous readings lead to failures.
