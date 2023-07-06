# Developing the DTVF

This `/library` directory houses the Typescript, CSS, and HTML files that make up the framework along with some configuration files for compilation. The result of the build process is a Docker image that contains the compiled DTVF files along with an Apache web server; this allow users to then attach their content via a Docker volume mounted to `/var/www/html`.
<br/>

## Development Requirements

To develop the DTVF the following software is required on the local machine:

* A working Docker installation.
* An installation of a [Typescript](https://www.typescriptlang.org/) compiler.
  * A docker image containing a configured compiler has been provided (see below).
* An installation of [Grunt](https://gruntjs.com/)
  * A docker image containing a configured compiler has been provided (see below).
  
In addition, developers should have a solid understanding of Docker, Typescript, and the mapping provider libraries (Mapbox & Cesium) before attempting to make any changes to the DTVF.

The final JS and CSS files will be built into the `output` directory, ready for local testing by running the `npm install && tsc && grunt package` command from this directory.

<br/>

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

<br/>

## Architecture

The DTVF has been developed to use an architecture agnostic to any specific mapping provider wherever possible. To that end, a central `Manager` class is used as point of access, generic `DataGroup`, `DataSource`, and `DataLayer` classes define the data to be displayed by the visualisation, and a variety of UI handlers generate and control the custom visualisation controls.

Where mapping provider specific behaviour is required, concrete instances of the `MapHandler` class are used to directly interface with the JavaScript libraries provided by those providers. At the time of writing, these are the `MapHandler_Mapbox` and `MapHandler_Cesium` classes.

<br/>

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

<br/>

## Deployment of Base Image

The intended workflow of creating DTVF visualisations is to use a standard base image that contains the compiled DTVF library and an Apache web server to host content. The base image can be built from this directory by running the `build.sh` script. Note that it will be tagged with the version currently listed within the `VERSION` file.

<br/>

## Support

For support on developing the DTVF, please contact the support team at [CMCL](mailto:support@cmclinnovations.com).