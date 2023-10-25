# Visualisation of the UK Base World

This directory contains the documentation, configuration files, and associated scripts for a visualisation of The World Avatar's base world (focussing on assets within the United Kingdom). Whilst other data and capabilities related to the base world may exist elsewhere in The World Avatar, this documentation only covers the steps needed to acquire, upload, and visualise data used in the live UK Base World visualisation currently available from [The World Avatar's website](https://theworldavatar.io).

This documentation was written in August of 2023. The data available from the listed sources may have changed since this time, hopefully the processes are still applicable to any new data sets.

## Gathering data

Data for this visualisation has been gathered from the below sources; the original raw files and the processed files have been archived at CMCL on their Pavilion file server. Hopefully this process is repeatable with future versions of these data sets, if not then the archived data can be used as a fall-back. If the visualisation is updated with future versions of these data, the raw and processed versions of said files should also be archived.

As a base world visualisation, more data sources will be added in future; as and when they are, they should be documented within this page, or in supplementary files linked to from this one.

Several of the data sources are presented as examples of the stack data uploader and can be found separately at their [documentation page](https://github.com/cambridge-cares/TheWorldAvatar/tree/760-pylon-stack-and-visualisation-needs-updating-to-use-latest-version-of-stack-and-vis/Deploy/stacks/dynamic/stack-data-uploader#datasets-and-subsets). Currently some of these only exist in a branch for `pylons-and-veg` viz.

### Digest of UK Energy Statistics (DUKES)

Once a year, the UK government publishes a Digest of UK Energy Statistics (DUKES);  note this was formally published by the Department for Business, Energy and Industrial Strategy (BEIS) before it was dissolved, subsequent publications should be from the new Department for Energy Security and Net Zero (DESNZ).

Read the associated [DUKES Data](./docs/data-dukes.md) page for details on how the DUKES data was acquired and processed.

### National Grid 
National Grid publish shapefiles of their whole network including lines, pylons and substations. There are links to :Peach download page in the relevant `data` subdirectory. There is a good number of files (20) to download but they are all backed up on pavilion. Individual links for each file and where they go is given in the *Running the Stack* section of this page

### UKPN
Similarly to above, download links are in each of the relevant Uk Power Networks subdirectory and are backed up on pavilion. Links also given in the *Running the Stack* section

### Forestry
Shapefiles are obtained from [national forestry inventory 2020](https://data-forestry.opendata.arcgis.com/datasets/eb05bd0be3b449459b9ad0692a8fc203_0/explore?location=55.089693%2C-2.724655%2C6.98) and backed upon pavilion. The feature info agent is used to cross reference with the power lines and determine whether or not they intersect.

## Uploading data

Now that we have a clean CSV for each data set, we can spin up an instance of the stack (see [here](https://github.com/cambridge-cares/TheWorldAvatar/tree/main/Deploy/stacks/dynamic/stack-manager) for details on how to do this) then run the data uploader to get our data into a relational database. Before trying to upload data, the [uploader's documentation](https://github.com/cambridge-cares/TheWorldAvatar/tree/main/Deploy/stacks/dynamic/stack-data-uploader) is considered required reading; this file will not detail the generic upload process.

A number of pre-written configuration files (separated by data set) are linked to below, note that these (along with the data set CSVs) will need to be copied into the correct directories before running the uploader.

Once the data uploader has finished running, you should be able to log into the GeoServer web dashboard and preview the layers (and feature locations within them).

### Power

The below files relate to the aforementioned DUKES 2023 data set.

* [Uploader config](./inputs/config/dukes_2023.json)
* [Ontop mapping](./inputs/data/uk_base_world//dukes_2023.obda)
  * Note that at the time of writing, this mapping utilises TBoxes that do not appear within the OntoEIP ontology. Nothing in the mapping contradicts the ontology, but the existing ontology does not contain enough concepts to cover all of the concepts provided by DUKES. 
* [OntoEIP ontology](https://github.com/cambridge-cares/TheWorldAvatar/tree/main/JPS_Ontology/ontology/ontoeip)
  * Note that when uploading the ontology files, you may need to rename any Turtle files with the `.ttl` extension. The stack data uploader assumes that `.owl` files are using the XML format, if an `.owl` file is using Turtle then this will cause errors during upload.


## Creating a visualisation

A visualisation has also been created for the UK Base World, the `visualisation/webspace` directory contains the files required and are copied into a `dtvf-base-image` container for hosting during the start-up process. As with all DTVF visualisations, the `data.json` file defines the data to be loaded on the visualisation, and in what grouping. Users running the visualisation in a new location may need to adjust the URLs listed in this file.

For more information on how visualisations are created and configured using the DTVF, please read its [documentation page](https://github.com/cambridge-cares/TheWorldAvatar/tree/main/web/digital-twin-vis-framework).

Note that this __may__ require building a local copy of the `dtvf-base-image` Docker image. If so, please run the `build.sh` script from within the `/web/digital-twin-vis-framework/library` directory.

### Feature info agent

To support metadata for the visualisation, the stack for this visualisation has been configured to also launch an instance of the [Feature Info Agent](https://github.com/cambridge-cares/TheWorldAvatar/tree/main/Agents/FeatureInfoAgent). The appropriate configuration file and query files have been created and will be copied into the relevant directories when using the `./scripts/start.sh` script to launch the stack (see below).

### Grafana dashboard

In addition, this stack will also launch a Grafana container to host associated dashboards. Whilst empty at first, the `./scripts/start.sh` script will upload pre-configured data source and dashboard definitions to provide a number of default analytic dashboards.

## Running the stack

The UK Base World visualisation has been put together as a single stack with no requirements on any external services (outside of standard JavaScript libraries). Both the data required for the visualisation, and the visualisation itself are hosted within the stack instance. For more information on the stack, read the [documentation here](https://github.com/cambridge-cares/TheWorldAvatar/tree/main/Deploy/stacks/dynamic/stack-manager).

For ease of use, a script has been provided to spin up the stack and upload the relevant data, this removes the need for users to manually populate files and directories within the `stack-manager` directory. Note that this script uses standard bash but does **require the installation of the `jq` package** on the host machine to parse JSON responses from the Grafana HTTP API.

Note that this script has been developed assuming a first-time spin up condition, it has not been tested in the case that some containers or volumes already exist; it would likely need temporary, local only changes in that situation.

To run the script and bring up a local instance of the UK Base World visualisation, follow the below steps. To get copies of the required data files, please see the data sections above or contact CMCL for archived copies.

**Note:** running the script to deploy this stack will remove any existing stack manager configurations; please backup any existing ones beforehand.

1. If required, run the `build.sh` script from within the `/web/digital-twin-vis-framework/library` directory.
   - This will build a local copy of the visualisation hosting image, in case the current branch contains a new version that hasn't been pushed yet.
2. Navigate to the `uk-base-world` directory.
3. Add your Mapbox credentials:
   - Add your username to a file at `./visualisation/mapbox_username`
   - Add your API key to a file at `./visualisation/mapbox_api_key`
4. Add the data files: 
   **NB** the entire config and data folders can be copied directly into the `inputs` directory from `\\pavilion\all\projects\uk-base-world-with-pylons-and-veg\data`
   - Add the processed DUKES 2023 CSV to the `./inputs/data/uk_base_world/dukes_2023` directory.
   - Add the OntoEIP OWL and TTL files to the `./inputs/data/uk_base_world/ontoeip` directory (note that these need to be in a flat structure, no subdirectories).
   - Add forestry data: download shapefiles from [National Forest Inventory Woodland GB 2020](https://data-forestry.opendata.arcgis.com/datasets/eb05bd0be3b449459b9ad0692a8fc203_0/explore?location=55.208415%2C-2.724655%2C6.98) and copy into `./inputs/data/forestry/vector` subdirectory.
   - Add relevant shapefiles for National Grid vector data: cables, ohls, poles, substations and towers.
      - [132kV Transformers](https://connecteddata.nationalgrid.co.uk/dataset/132kv_gm_full_nged_area_4_dnos_in_esri_shapefile_shp_format), [66kV Ground Mounted Substations](https://connecteddata.nationalgrid.co.uk/dataset/66kv_gm_full_nged_area_4_dnos_in_esri_shapefile_shp_format), [33kV Transformers](https://connecteddata.nationalgrid.co.uk/dataset/33kv-transformers), [11kV Transformers](https://connecteddata.nationalgrid.co.uk/dataset/11kv-transformers) 
       into `./inputs/data/national_grid/vector/substations` 

       - [132kV Towers](https://connecteddata.nationalgrid.co.uk/dataset/132kv_towers_full_nged_area_4_dnos_in_esri_shapefile_shp_format), [66kV Towers](https://connecteddata.nationalgrid.co.uk/dataset/66kv_towers_full_nged_area_4_dnos_in_esri_shapefile_shp_format), [33Kv Towers](https://connecteddata.nationalgrid.co.uk/dataset/33kv_towers_full_nged_area_4_dnos_in_esri_shapefile_shp_format), [11kV Towers](https://connecteddata.nationalgrid.co.uk/dataset/11kv_towers_full_nged_area_4_dnos_in_esri_shapefile_shp_format) 
       into `./inputs/data/national_grid/vector/towers` 

       - [132kV Poles](https://connecteddata.nationalgrid.co.uk/dataset/132kv_poles_full_nged_area_4_dnos_in_esri_shapefile_shp_format), [66kV Poles](https://connecteddata.nationalgrid.co.uk/dataset/66kv_poles_full_nged_area_4_dnos_in_esri_shapefile_shp_format), [33kV Poles](https://connecteddata.nationalgrid.co.uk/dataset/33kv_poles_full_nged_area_4_dnos_in_esri_shapefile_shp_format), [11kV Poles](https://connecteddata.nationalgrid.co.uk/dataset/11kv_poles_full_nged_area_4_dnos_in_esri_shapefile_shp_format) 
       into `./inputs/data/national_grid/vector/poles` 

       - [132kv Overhead Lines](https://connecteddata.nationalgrid.co.uk/dataset/132kv_ohl_full_nged_area_4_dnos_in_esri_shapefile_shp_format), [66kV Overhead Lines](https://connecteddata.nationalgrid.co.uk/dataset/66kv_ohl_full_nged_area_4_dnos_in_esri_shapefile_shp_format), [33kV Overhead Lines](https://connecteddata.nationalgrid.co.uk/dataset/33kv_ohl_full_nged_area_4_dnos_in_esri_shapefile_shp_format), [11kV Overhead Lines](https://connecteddata.nationalgrid.co.uk/dataset/11kv_ohl_full_nged_area_4_dnos_in_esri_shapefile_shp_format)
       into `./inputs/data/national_grid/vector/ohls`

       - [132kV Underground Cables](https://connecteddata.nationalgrid.co.uk/dataset/132kv_ug_full_nged_area_4_dnos_in_esri_shapefile_shp_format), [66kv Underground Cables](https://connecteddata.nationalgrid.co.uk/dataset/66kv_ug_full_nged_area_4_dnos_in_esri_shapefile_shp_format), [33kV Underground Cables](https://connecteddata.nationalgrid.co.uk/dataset/33kv_ug_full_nged_area_4_dnos_in_esri_shapefile_shp_format), [11kV Underground Cables](https://connecteddata.nationalgrid.co.uk/dataset/11kv_ug_full_nged_area_4_dnos_in_esri_shapefile_shp_format)
       into `./inputs/data/national_grid/vector/ohls`
   - Add relevant shapefiles for UKPN vector data. All of the below files should be copied into `./inputs/data/ukpn/vector/ohls`
       - [33kV Overhead Lines](https://ukpowernetworks.opendatasoft.com/explore/dataset/33kv-overhead-lines)
       - [132kV Overhead Lines](https://ukpowernetworks.opendatasoft.com/explore/dataset/132kv-overhead-lines) 
       - [132kV Poles & Towers](https://ukpowernetworks.opendatasoft.com/explore/dataset/132kv-poles-towers)
       - [33kV Poles & Towers](https://ukpowernetworks.opendatasoft.com/explore/dataset/ukpn-33kv-poles-towers)
       - [66kV Overhead Lines](https://ukpowernetworks.opendatasoft.com/explore/dataset/ukpn-66kv-overhead-lines-shapefile)
       - [HV Overhead Lines](https://ukpowernetworks.opendatasoft.com/explore/dataset/ukpn-hv-overhead-lines-shapefile)
       - [LV Overhead Lines](https://ukpowernetworks.opendatasoft.com/explore/dataset/ukpn-lv-overhead-lines-shapefile)
5. Run the script from the `uk-base-world` directory, passing a password for PostGIS and Geoserver:
   - Example command: `./scripts/start.sh PASSWORD=pickapassword`
   - If deploying behind an existing URL, the `HOST` parameter can be passed to auto-update the visualisation's client side files (e.g. `./scripts/start.sh PASSWORD=pickapassword HOST=https://theworldavatar.io/demo/uk-base-world`)
6. Confirm that the required data files are present by pressing the `Y` key.
7. Once prompted, wait for the stack to spin up, the data uploader should run automatically a few seconds after the manager has exited.
   - That stack is considered "spun up" once the stack-manager container has stopped (although there is some wiggle-room here if you're also spinning up containers that have lengthy service start-ups).
   - If running for the first time, this may take a while as Docker images will need to be downloaded.
8. Confirm the visualisation is working by visiting `localhost:38383/visualisation`

Stopping the stack (including the option to remove existing volumes), can be done by using the `stack.sh` script within the `scripts` directory; the name of the created stack will be `UKBASEWORLD`.

## Support

For any support in reproducing this visualisation, please contact the CMCL support team.

## Screenshot

<p align="center">
    <img src="./inputs/uk-base-world.jpg" alt="UK Base World visualisation, circa August 2023." width="66%"/>
</p>
<p align="center">
    <em>UK Base World visualisation, circa August 2023.</em>
</p>
