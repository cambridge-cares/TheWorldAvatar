# An agent to represent and query UK wind data reported by the Met Office.
### Authors
* [Toby Latcham](tjl47@cam.ac.uk)
* [Sophie Hall](sh2000@cam.ac.uk)
* [Feroz Farazi](msff2@cam.ac.uk)

The agent processes UK Mean Wind Data downloaded as a CSV file from Met Office Integrated Data Archive System (MIDAS) to represent it in the World Avatar Knowledge Graph (KG) to make it accessible and queryable. The data describes the mean wind speed and direction and maximum gust speed, direction and time reported in the context of sensors installed in different locations all over the UK.

Python modules developed for data instantiation and query are briefly described below.

* wind_sensor_instantiation.py: This script instantiates wind sensor-generated time series data attached to geospatial reference within the KG using the [TimeSeriesClient].

* wind_sensor_query.py: This script queries the KG and visualises the wind data using the Digital Twin Visualisation Framework ([DTVF]). To interact with the [TheWorldAvatar] (TWA) code base (mainly) written in Java, the [py4jps] Python wrapper is used.

Note: Running any python file available here will require the change of directory to the <root> folder named RenewableEnergyAgents and the use of the agent folder called MetOfficeWindSensorAgent. For example, to run wind_sensor_instantiation.py use the following command:
         python -m MetOfficeWindSensorAgent.wind_sensor_instantiation