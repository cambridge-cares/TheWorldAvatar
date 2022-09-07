# An agent to represent and query solar data reported by the Newcastle Urban Observatory.
### Authors
* [Toby Latcham](tjl47@cam.ac.uk)
* [Sophie Hall](sh2000@cam.ac.uk)
* [Feroz Farazi](msff2@cam.ac.uk)

The agent processes wind data including wind direction, wind speed and wind gust downloaded as a CSV file from the Newcastle Urban Observatory to represent it in the World Avatar Knowledge Graph (KG) to make it accessible and queryable. The data is reported in the context of sensors positioned around Newcastle.

The data is available at https://urbanobservatory.ac.uk and accessible via REST APIs. Python modules developed for data instantiation and query are briefly described below.

* solar_sensor_instantiation.py: This script instantiates wind sensor-generated time series data attached to geospatial reference within the KG using the [TimeSeriesClient].

* solar_sensor_query.py: This script queries the KG and visualises the wind data using the Digital Twin Visualisation Framework ([DTVF]). To interact with the [TheWorldAvatar] (TWA) code base (mainly) written in Java, the [py4jps] Python wrapper is used.

Note : Running any python file available here will require the change of directory to the <root> folder named RenewableEnergyAgents and the use of the agent folder called UrbanObservatoryWindSensorAgent. For example, to run wind_sensor_instantiation.py use the following command:
        python -m UrbanObservatoryWindSensorAgent.wind_sensor_instantiation