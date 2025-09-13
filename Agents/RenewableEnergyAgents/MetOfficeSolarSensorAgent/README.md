# An agent to represent and query UK solar data reported by the Met Office.
### Authors
* [Toby Latcham](mailto:tjl47@cam.ac.uk)
* [Sophie Hall](mailto:sh2000@cam.ac.uk)
* [Feroz Farazi](mailto:msff2@cam.ac.uk)

The agent processes UK solar radiation data downloaded as a CSV file from the Met Office Integrated Data Archive System (MIDAS) to represent it in the World Avatar Knowledge Graph (KG) to make it accessible and queryable. The data is reported in the context of sensors installed in different locations all over the UK.

Python modules developed for data instantiation and query are briefly described below.

* solar_sensor_instantiation.py: This script instantiates solar sensor-generated time series data attached to geospatial reference within the KG using the [TimeSeriesClient].

* solar_sensor_query.py: This script queries the KG and visualises the solar data using the Digital Twin Visualisation Framework ([DTVF]). To interact with the [TheWorldAvatar] (TWA) code base (mainly) written in Java, the [py4jps] Python wrapper is used.

Note : Running any python file available here will require the change of directory to the <root> folder named RenewableEnergyAgents and the use of the agent folder called MetOfficeSolarSensorAgent. For example, to run solar_sensor_instantiation.py use the following command:
        python -m MetOfficeSolarSensorAgent.solar_sensor_instantiation