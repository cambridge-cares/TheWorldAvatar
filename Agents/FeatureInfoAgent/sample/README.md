# Feature Info Agent - Sample

This directory contains a small set of sample data, configurations, and scripts that can be used to spin up a [TWA Stack](https://github.com/cambridge-cares/TheWorldAvatar/tree/main/Deploy/stacks/dynamic/stack-manager) instance with a copy of the Feature Info Agent, and enough data to test it in a typical runtime environment.

## Spinning up the sample stack

To launch the sample stack, follow the below steps:

1. If local changes to the FeatureInfoAgent have been made, build a local copy of its image.
   - This can be done using the `build.sh` script from the `Agents/FeatureInfoAgent` directory.
2. Run the `launch.sh` script within this directory.
   - This requires a `PASSWORD` argument to set the password for GeoServer and PostGIS.
3. The script will pause and wait for user input whilst it spins up containers, hit `ENTER` once this is complete. 