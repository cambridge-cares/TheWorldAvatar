# Setting up the agent
1. Copy `sg_landuse.csv` to the [resources folder] of OSM agent.
2. Replace the [config.properties](https://github.com/cambridge-cares/TheWorldAvatar/blob/main/Agents/OSMAgent/osmagent/src/main/resources/config.properties) in OSM agent's [resource folder] with the `config.properties` in this folder.
3. Spin up OSM agent by following its [README](https://github.com/cambridge-cares/TheWorldAvatar/blob/main/Agents/OSMAgent/README.md)

# Running the agent
Two files are required for automating the running of the OSM agent
- `run_agent.py`: script for automating running the OSM agent, provided in this directory
- a CSV file containing the WKT string, stored as `wk` inside the CSV, of the polygon geometries to run
  - See example in `sg_partitions.csv`, which is the Singapore geometry partitioned into 12 smaller polygons

To run `run_agent.py` in the background, run the following command:
`nohup python3 run_agent.py csv > ./nohup.out 2>&1 &`,
where `csv` is the file path to the CSV file (e.g. `sg_partitions.csv`) containing the geometries to run. This will run the OSM agent in the background for the geometries inside the `csv`. The logs will be written to `nohup.out`.

It is advised to split any large polygon one wishes to run into smaller polygons (such as the case for Singapore with `sg_partitions.csV`) to speed up running and preventing any timeout issues.

[resources folder]: https://github.com/cambridge-cares/TheWorldAvatar/tree/main/Agents/OSMAgent/osmagent/src/main/resources