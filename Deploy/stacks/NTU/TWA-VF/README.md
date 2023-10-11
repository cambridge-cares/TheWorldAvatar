# NTU Ditigal Twin Visualisation

The NTU digital twin visualization has been developed to showcase an interactive UI for a knowledge graph. This graph integrates a 13-bus power network configuration, power consumption data, photovoltaic panel data, class schedules, and venue information specific to the NTU campus.

It is recommended that you read the through the [TWA-VF Overview](../docs/overview.md) and [Working with CesiumJS](../docs/cesium.md) sections of the documentation before playing around with this NTU ditigal twin.

<br/>

## Building the Image


The `docker-compose.yml` file contains the required configuration to build a Docker Image for the example visualisation. This uses the `twa-vf` image as a base then adds the contents of the `webspace` directory to a volume mounted at `/var/www/html` within the container.

- Files to be hosted must be contained within the `webspace` directory.
- A valid Mapbox username and API token must be provided (still required in Cesium visualisations).
- A connection to the internet is required to contact remote resources and use the mapping libraries.

Once the requirements have been addressed, the image can be built using the below commands, run from within this directory.

- To build the Image:
    - `docker compose -f docker-compose.yml build --force-rm`
- To generate a Container (i.e. run the Image):
    - `docker compose -f docker-compose.yml up -d --force-recreate`

<br/>
