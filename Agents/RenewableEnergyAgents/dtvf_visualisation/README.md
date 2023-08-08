# Renewable Energy Agents Visualisation

The folder contains css, data, and Docker setup files for the Renewable Energy Agents. These agents form part of the `agent` stack at CMCL.

The "Dockerfile" file contains the instructions to build an image; before making any changes to it, please consult the system administrators at CMCL (Michael Hillman <mdhillman@cmclinnovations.com>, Owen Parry <oparry@cmclinnovations.com>).

## Purpose

The Renewable Energy Agents exist to parse and add solar and wind sensor data as well as solar and wind energy consumption data to the Knowledge Graph (KG). The agents do this via the `*_instantiation.py` script. For example, MetOfficeSolarSensorAgent employs `solar_sensor_instantiation.py` and MetOfficeWindSensorAgent employs `wind_sensor_instantiation.py`. Each script adds data to the KG endpoint specified within the properties file. This is done using the TimeSeries format so the TimeSeriesClient class from the JPS Base Library is also used to check for and, where needed, instantiate TimeSeries links within the KG.

The agents also query the KG and output the sensor location to GeoJSON files and energy generation or consumption data in JSON files via the `*_query.py` script. For example, MetOfficeSolarSensorAgent employs `solar_sensor_query.py` and MetOfficeWindSensorAgent employs `wind_sensor_query.py`. Each script outputs to the `queried_data` directory. The Renewable Energy Agents visualisation renders the data to an interactive Graphical User Interface (GUI).

## Visualisation

Follow the instructions provided in the [main readme file] of project to generate the GUI.

[main readme file]: ../README.md