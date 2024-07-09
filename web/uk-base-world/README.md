# Visualisation of Augmented UK

This directory contains the documentation, configuration files, and associated scripts for a visualisation of The World Avatar's digital twin (focussing on assets within the United Kingdom). Whilst other data and capabilities related to the project may exist elsewhere in The World Avatar, this documentation only covers the steps needed to acquire, upload, and visualise data used in the deployed visualisation currently available from [The World Avatar's website](https://theworldavatar.io).

This documentation was written in August of 2023. The data available from the listed sources may have changed since this time, hopefully the processes are still applicable to any new data sets.

## Gathering data

Data for this visualisation has been gathered from the sources listed on the [Data](./docs/data.md) page; the original raw files, as well as any processed files, have also been archived at CMCL on the Pavilion file server. Hopefully this process is repeatable with future versions of these data sets, if not then the archived data can be used as a fall-back. If the visualisation is updated with future versions of these data, the raw and processed versions of said files should also be archived.

As a base world visualisation, more data sources will be added in future; as and when they are, they should be documented within the aforementioned page.

## Uploading data

Once the correct files for each data source have been acquired, we can spin up an instance of the stack (see [here](https://github.com/cambridge-cares/TheWorldAvatar/tree/main/Deploy/stacks/dynamic/stack-manager) for details on how to do this) then run the data uploader to get our data into a relational database. Before trying to upload data, the [uploader's documentation](https://github.com/cambridge-cares/TheWorldAvatar/tree/main/Deploy/stacks/dynamic/stack-data-uploader) is considered required reading; this file will not detail the generic upload process.

With each data set come a number of pre-written associated files (configurations, queries, styles etc.). These files are documented along with their corresponding data source on the [Data](./docs/data.md) page.

Once the data uploader has finished running, you should be able to log into the GeoServer web dashboard and preview the layers (and feature locations within them).

## Creating a visualisation

The project has now been updated to use the new `twa-vf` version 5. The `uploads` directory contains the files required to start a web visualisation using the docker image. As with all TWA-VF visualisations, the `data.json` file defines the data to be loaded on the visualisation, and in what grouping. Users running the visualisation in a new location may need to adjust the URLs listed in this file.

For more information on how visualisations are created and configured using the TWA-VF, please read its [documentation page](https://github.com/cambridge-cares/TheWorldAvatar/tree/main/web/digital-twin-vis-platform).

Note that this __may__ require building a local copy of the `twa-vf` Docker image. If so, please run the `docker compose build` command from within the `/web/twa-vis-platform/` directory.

### Feature info agent

To support metadata for the visualisation, the stack for this visualisation has been configured to also launch an instance of the [Feature Info Agent](https://github.com/cambridge-cares/TheWorldAvatar/tree/main/Agents/FeatureInfoAgent). The appropriate configuration file and query files have been created and need to be copied into the relevant directories.

### Grafana dashboard

In addition, this stack will also contain a Grafana container to host associated dashboards. Whilst empty at first, the pre-configured data source and dashboard definitions to provide a number of default analytic dashboards. Instructions on the configuration of the dashboard are in the following section.

## Running the stack

### Method 1 - Data stack with standalone viz app

The UK Base World visualisation has been put together as a single stack with no requirements on any external services. Both the data required for the visualisation, and the visualisation itself are hosted within the stack instance. For more information on the stack, read the [documentation here](https://github.com/cambridge-cares/TheWorldAvatar/tree/main/Deploy/stacks/dynamic/stack-manager).

To start a local instance of the augmented UK visualisation, follow the below steps. To get copies of the required data files, please see the data sections above or contact CMCL for archived copies.

Grafana config instructions will be added soon, ignore for now

N.B `/stack-manager/.../` is `TheWorldAvatar/Deploy/stacks/dynamic/stack-manager` and `/stack-data-uploader/.../` is `TheWorldAvatar/Deploy/stacks/dynamic/stack-data-uploader`

1. If required, run the `build.sh` script from within the `/web/digital-twin-vis-platform/` directory.
   - This will build a local copy of the visualisation hosting image, in case the current branch contains a new version that hasn't been pushed yet.
2. As usual, ensure that `geoserver_password`, `postgis_password` secrets are in the `stack-manager/inputs/secrets` folder. As well as these, you will need to add a `mapbox_username` and `mapbox_api_key` secrets, which should be retrieved from your mapbox account. Thirdly, create a `grafana_password` in the secrets directory.
3. Copy the main stack manager config to `./inputs/config/manager/augmented-uk.json` config file to `/stack-manager/inputs/config/`
4. Copy the custom service configs: `./inputs/config/manager/visualisation.json`  and ./inputs/config/manager/grafana.json to `/stack-manager/inputs/config/services`
5. Copy the `./inputs/config/manager/fia-queries` folder to `/stack-manager/inputs/data/`
6. Run the stack manager in the usual way at port 38383 with `./stack.sh start augmented-uk 38383` run from the `/stack-manager/` folder. Ensure all the containers spin up properly
7. Copy *all* the contents of the `./inputs/uploader/config` folder to `/stack-data-uploader/inputs/config/` (you can just delete the target config folder and replace it with the one here)
8. Similarly, replace `/stack-data-uploader/inputs/data/` folder with `./inputs/uploader/data`.
9. Add the data files as specified in according to the [data documentation](./docs/data.md). The links for each should also now be in the relevant subdirectory of `/stack-data-uploader/inputs/data/`.
10. Run the stack data-uploader in the usual way with `./stack.sh start augmented-uk` run from the `/stack-data-uploader/` folder. Check the logs and ensure that data uploads properly. This will take a while
11. Next copy the contents of `.uploads` to `../twa-vis-platform/uploads` and run `docker compose up` from `../twa-vis-platform` to start the standalone viz container
12. When the uploader has finished, confirm the visualisation is working by visiting `localhost:38384/`

Stopping the stack (including the option to remove existing volumes), can be done by using the `stack.sh` script within the `scripts` directory; the name of the created stack will be `augmented-uk`.

### Method 2 - Stack with viz included service

*To be added*

## Support

For any support in reproducing this visualisation, please contact the CMCL support team.

## Screenshot

![Augmented UK visualisation as of July 2024](screenshot.png)
