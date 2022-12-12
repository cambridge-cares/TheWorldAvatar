# Description

The `Electricityconsumption` agent is an input agent which read data from the `.xslx`, pickle and `.nc` files placed in the `./Data` folder, and instantiates it according to the [Ontogasgrid](https://github.com/cambridge-cares/TheWorldAvatar/tree/main/GasGrid) ontology in the [TheWorldAvatar](https://github.com/cambridge-cares/TheWorldAvatar) knowledge graph. could instantiate the Electricity Consumption data, numbers of meters data into the Blazegraph

&nbsp;
# 1. Setup

tbc...

&nbsp;
# 2. Using the Agent

## A few preparation

- The `LSOA_domestic_elec_2010-20.xlsx` file will be read by this agent, which should be placed under `./Data` folder.
- (Provisional) The query and update endpoint are specified at the beginning of `./agent/dataretrieval/readings.py`. The default setting is as follows:
> DEF_NAMESPACE = "ontogasgrid"
> LOCAL_KG = "http://localhost:8080/blazegraph"
> QUERY_ENDPOINT= UPDATE_ENDPOINT = LOCAL_KG + "/namespace/" + DEF_NAMESPACE + "/sparql"

## Run the agent
Run the `./agent/dataretrieval/readings.py` file

&nbsp;
# Authors
Jieyang Xu (jx309@cam.ac.uk), Feroz Farazi (msff2@cam.ac.uk) Dec 2022


(Parts of the agent leverage code initially developed by Daniel Nurkowski (danieln@cmclinnovations.com) and Tom Savage (trs53@cam.ac.uk))
<!-- Links -->
<!-- websites -->