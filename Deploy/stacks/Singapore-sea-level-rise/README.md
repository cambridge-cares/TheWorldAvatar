# Singapore Sea-Level Rise
This repository contains the instructions, directory structure and configurations required to deploy Singapore stack for Sea-Level-Rise analysis which builds on top of the existing [Augmented Singapore](https://github.com/cambridge-cares/TheWorldAvatar/tree/dev-singapore-stack/Deploy/stacks/Singapore). 

## Data
Comprehensive data collated can be found in the [CARES dropbox link](https://www.dropbox.com/scl/fo/s4youc2epx7quqapolgw6/AH_IAMDhH9FppOosYpKd3zs?rlkey=4ab335m057bkv64zs7e8xdn20&dl=0). 

### Cultural Sites and Trees
Cultural sites - Historic Sites, Monuments, Museums, Tourist Attractions
Trees - Trees, Heritage trees 

Cultural Sites and Trees data are retrieved from [Singapore's Open Data Portal](https://beta.data.gov.sg/).

### Digital Elevation Model
SRTM Digital Elevation Model 2000 are retrieved from [USGS EROS Archive - Digital Elevation](https://www.usgs.gov/centers/eros/science/usgs-eros-archive-digital-elevation-shuttle-radar-topography-mission-srtm-1#overview).

### Landplots
Landplots are retrieved from [Singapore's Open Data Portal](https://beta.data.gov.sg/)'s URA Master Plan 2019.

### Population
Singapore population distribution 2000 is retrieved from [Facebook Data For Good](https://dataforgood.facebook.com/dfg/tools/high-resolution-population-density-maps).

### Street network data
Street network data is retrieved from OpenStreetMap through [Geofabrik](https://download.geofabrik.de/).

### Sea Level Rise
Sea level rise projections are provided by the courtesy of Prof. Benjamin Horton and Dr. Timothy A. Shaw from Nanyang Technological University, Earth Observatory of Singapore, published under [`Shaw, T.A., Li, T., Ng, T. et al. Deglacial perspectives of future sea level for Singapore. Commun Earth Environ 4, 204 (2023)`](https://doi.org/10.1038/s43247-023-00868-5).

## Agent Setup
### OSMAgent
[OSMAgent](https://github.com/cambridge-cares/TheWorldAvatar/tree/main/Agents/OSMAgent) is run to match OSM entities with Singapore's CityGML buildings. For remaining buildings not matched to any OSM entities, OSMAgent will match with the underlying landplot usage. 

### BuildingFloorAgent
[BuildingFloorAgent](https://github.com/cambridge-cares/TheWorldAvatar/tree/main/Agents/BuildingFloorAgent) derives number of floors for 3D Buildings using three methods 
- HDB property Information
- OpenStreetMap
- Building floors from building height

The purpose of this agent is to augment buildings with more accurate data on the number of floors for the subsequent calculcations of GFA and construction cost. To run this agent: 
- Prerequisite: OSM data and ontop mapping added by OSM agent
- Download [HDBPropertyInformation.csv](https://www.dropbox.com/scl/fi/3pgkir5zfcbhq8dliv1kr/HDBPropertyInformation.csv?rlkey=5lmb49cjqgvyrx7rcxtos1l41&dl=0) and place it in ```./buildingfloordata```
- Modify [sea-level.json] to include this agent within services, the following example also includes gfaagent for the next step
```json
{
    "services": {
        "includes": [
            "visualisation",
            "feature-info-agent",
            "filter-agent",
            "sealevelimpactagent",
            "buildingflooragent", // modified
            "gfaagent" // modified 
        ],
```
- Restart stack-manager to spin up this agent
- Execute 
```
curl -X POST http://localhost:3838/buildingflooragent/
```
- Check contents of ```gfa_floors.floors```, the number of rows should equate the number of buildings
### GFAAgent
[GFAAgent](https://github.com/cambridge-cares/TheWorldAvatar/tree/dev-sea-level-rise-singapore/Agents/GFAAgent) computes the Gross Floor Area (GFA) and the construction cost of buildings. 

- Prequisites: 
    1) Floors data added by BuildingFloorAgent
    2) Building geometries in citydb
    3) OSM usage information added by OSM agent
- Modify [sea-level.json] to include this agent within services (not needed if already spun up)
```json
{
    "services": {
        "includes": [
            "visualisation",
            "feature-info-agent",
            "filter-agent",
            "sealevelimpactagent",
            "buildingflooragent", // modified
            "gfaagent" // modified 
        ],
```
- Execute the following
```
curl -X POST http://localhost:3838/gfaagent/gfa
```
- Make sure the table `gfa_floors.gfa` is populated with data
- Then execute
```
curl -X POST http://localhost:3838/gfaagent/cost
```

### SeaLevelImpactAgent
[SeaLevelImpactAgent](https://github.com/cambridge-cares/TheWorldAvatar/tree/main/Agents/SeaLevelImpactAgent) assess the affected cultural sites, buildings, road networks and landplot, given a sea level rise projections.

## Deployment 
### Starting the stack-manager
To deploy the Singapore's Sea Level Rise tech stack, a default stack-name `sea-level` is pre-configured.

In the [stack-manager](stack-manager) directory, run the command below to spin up the tech stack,
```
./stack.sh start sea-level
```

### Uploading data
Once the stack-manager is fully spin up. In the [stack-data-uploader](stack-data-uploader) directory, replace the content of the directory with data from [CARES dropbox link](https://www.dropbox.com/scl/fo/s4youc2epx7quqapolgw6/AH_IAMDhH9FppOosYpKd3zs?rlkey=4ab335m057bkv64zs7e8xdn20&dl=0), run the command to upload data:
```
./stack.sh start sea-level
```

## Authors
Shin Zert Phua (shinzert.phua@cares.cam.ac.uk), May 2024

[sea-level.json]: ./stack-manager/inputs/config/sea-level.json