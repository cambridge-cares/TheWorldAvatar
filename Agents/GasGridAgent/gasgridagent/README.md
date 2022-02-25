# gasgridagent

The folder contains resources and visualisation subfolders, and python modules for creating instances in
the Knowledge Graph and querying the instances.

##Run:
+ generate_visualisation_data.py for querying the Knowledge Graph, generating json, geojson and timeseries data and saving it under the path of visualisation/data/gasgridassets.

##Requirements:
+ The Python code of this Agent requires the `py4jps` library, this is currently downloaded from the external PyPi website. This means that if a specific version of the Python Wrapper, or JPS Base Library is needed, the `py4jps` library needs to be rebuilt and reuploaded to PyPi first.
+ The KG endpoints, and TimeSeries database used by this agent are specified within the `resources/gasgridagent.properties` file.
