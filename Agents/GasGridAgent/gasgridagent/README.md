# gasgridagent

The folder contains resources and visualisation subfolders, and python modules for creating instances in
the Knowledge Graph and querying the instances.

##Run:
+ generate_visualisation_data.py for querying the Knowledge Graph, generating json, geojson and timeseries data and saving it under the path of visualisation/data/gasgridassets.

## Purpose

The Gas Grid agent exists to download, parse, and add live gas flow data to the Knowledge Graph. It does this via the `input_flow_data.py` script; once started, every 12 minutes this script will download a flow data CSV, parse it, then upload it to the KG endpoint specified within the properties file. This is done using the new TimeSeries format so the TimeSeriesClient class from the JPS Base Library is also used to check for and, where needed, instantiate new TimeSeries links within the KG.

This agent also contains scripts to query the KG and output the location of Terminals, Offtakes, and Pipes to local GeoJSON files. These scripts are run once per day (via cron), and output to the `/var/www/html/gas-grid` directory. 

##Requirements:
+ The Python code of this Agent requires the `py4jps` library, this is currently downloaded from the external PyPi website. This means that if a specific version of the Python Wrapper, or JPS Base Library is needed, the `py4jps` library needs to be rebuilt and reuploaded to PyPi first.
+ The KG endpoints, and TimeSeries database used by this agent are specified within the `resources/gasgridagent.properties` file.
