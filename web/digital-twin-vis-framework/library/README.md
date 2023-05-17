# Developing the DTVF

This `/library` directory houses the Typescript, CSS, and HTML files that make up the framework along with some configuration files for compilation. When creating visualisations, the pre-compiled, hosted version of this framework should be used; files from this directory should not be copied into/directly linked to visualisation implementations.

## Requirements

To develop the DTVF the following sofware is required on the local machine:

* A working Docker installation.
* An installation of a [Typescript](https://www.typescriptlang.org/) compiler.
  * A docker image containing a configured compiler has been provided (see below).
* An installation of [Grunt](https://gruntjs.com/)
  * A docker image containing a configured compiler has been provided (see below).
  * 
In addition, developers should have a solid understanding of Docker, Typescript, and the mapping provider libraries (Mapbox & Cesium) before attempting to make any changes to the DTVF.

### Runtime Requirements

To function correctly, visualisations using this framework also needs to include the below JavaScript libraries. Note that at the time of writing, the DTVF is a client based library so each of these requirements is imported as a remote, client-side resource via the head section of the visualisation's `index.html` file.

* [Mapbox GL JS](https://docs.mapbox.com/mapbox-gl-js/api/) or [Cesium JS](https://cesium.com/platform/cesiumjs/)
  * Mapping library providers
* [JQuery & JQuery UI](https://jquery.com/)
  * JavaScript utilities (DOM traversal, events, HTTP calls etc.)
* [ChartJS](https://www.chartjs.org/)
  * Generates time series charts 
* [Moment & ChartJS Moment Adapter](https://momentjs.com/)
  * Add supports for date driven time series charts
* [Turf](https://turfjs.org/)
  * Calculate geometry centroids 
* [Hummingbird Treeview](https://github.com/hummingbird-dev/hummingbird-treeview)
  * Builds collapsible tree controls

## Architecture

The DTVF has been developed to use an architecture agnostic to any specific mapping provider wherever possible. To that end, a central `Manager` class is used as point of access, generic `DataGroup`, `DataSource`, and `DataLayer` classes define the data to be displayed by the visualisation, and a variety of UI handlers generate and control the custom visualisation controls.

Where mapping provider specific behaviour is required, concrete instances of the `MapHandler` class are used to directly interface with the JavaScript libraries provided by those providers. At the time of writing, these are the `MapHandler_Mapbox` and `MapHandler_Cesium` classes.

## Process

When launched from a correctly integrated HTML file, the DTVF follows a set pattern of initialisation and integration with the chosen mapping provider. At the time of writing, the general flow of events is as follows:

* UI elements are created from the static HTML in the visualisation's index file
* An instance of the manager class is initialised
* User name & API keys are registered
* Data source & layer definitions are loaded
* A blank map is initialised by calling the provider's libraries
* Default content of the side panel is cached so it can be returned to later
* Any custom icons or links are loaded
* Data source and layer definitions are converted to provider specific sources and layers
* New sources and layers are added to the map for display
* Event handlers are registered to capture mouse movement/clicks

## Environment

A number of configuration files are also present that allow the generation of a Docker container that can be used as a live development environment. The container houses [TypeScript](https://www.typescriptlang.org/) and [Grunt](https://gruntjs.com/) installations. These can be used for active development, compile-time checks/linting, or generation of deployable files.

To spin up the container and use it as a development environment via VSCode...

- Run the `docker-compose -f docker/docker-compose.yml up develop` command from within this directory.
- Select the `Remote Explorer` menu within VSCode.
  - You many need to install the `Docker` extension if you haven't done so already.
- Select the `Attach to Container` button for the `dtvf-devel` container.
- Once a new VSCode window appears (and the loading completes), you can now open a file/folder within the container and begin development.
- The entire `library` directory has been set up to share between your local machine and the container. This means that any changes you make within the container, will persist to your local filesystem.

## Compilation

To compile and combine the DTVF library into deployable files:

- Run the `docker-compose -f docker/docker-compose.yml up compile` command from within this directory.
- The container will compile and process the typescript files for deployment, then shutdown.
  - If successful, single JS and CSS files will be generated within the `output` directory.
    - In addition, a `help` directory is created housing the user-facing manual files. 
  - These can then be uploaded to a web server to make them publicly available.
    - To upload these to the KG website server, please contact the support team at CMCL.

## Deployment of Base Image

This directory also houses Docker configuration files to create an base image that can be used to create individual visualisations. A volume containing the webspace files simply needs to be added (as a volume) to the `/var/www/html/` directory; a example of this can be see in the Docker configuration files for the example visualisations.

To build and deploy the base DTVF image, so that others can use it to create visualisations:

- Run the `docker-compose -f docker/docker-compose.yml build deploy` command from within this directory.
- The image should then be built and tagged as `ghcr.io/cambridge-cares/dtvf-base-image:latest`
- Use the `docker push ghcr.io/cambridge-cares/dtvf-base-image:latest` command to upload the image.
  - This requires having set up a GitHub token and storing it using the `docker login` command. 

Note that the DTVF base image is simply a pre-configured web server, with a convenient location for a volume. It does not contain the compiled versions of the DTVF (although these _can_ be added if desired).

## Support

For support on developing the DTVF, please contact the support team at [CMCL](mailto:support@cmclinnovations.com).