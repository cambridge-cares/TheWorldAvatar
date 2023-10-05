# Feature Info Agent - Sample

This directory contains a small set of sample data, configurations, and scripts that can be used to spin up a [TWA Stack](https://github.com/cambridge-cares/TheWorldAvatar/tree/main/Deploy/stacks/dynamic/stack-manager) instance with a copy of the Feature Info Agent, and enough data to test it in a typical runtime environment.

Sample data used in this example is generated and no regard has been given for correctness. It is not suggested that people use this example as a starting point for their own projects.

## Spinning up the sample stack

To launch the sample stack, follow the below steps:

1. If local changes to the FeatureInfoAgent have been made, build a local copy of its image.
   - This can be done using the `build.sh` script from the `Agents/FeatureInfoAgent` directory.
2. Add your Mapbox API credentials to the `sample/visualisation` directory.
   - Username in a file named `mapbox_username`.
   - API token in file named `mapbox_api_key`. 
3. Run the `launch.sh` script within this directory.
   - This requires a `PASSWORD` argument to set the password for GeoServer and PostGIS.
4. The script will pause and wait for user input whilst it spins up containers, hit `ENTER` once this is complete. 

## What's in the sample stack

The sample stack contains a CSV of generated data on castles within the UK, with a very simple CSV ontology and OBDA mapping files. At the end of the `launch.sh` a request is made to the FIA to generate some sample time series data based on the IRIs the aforementioned data will generate; the time series consists of three dependent data series per castle with daily time values from now-15 days to now+15 days. 