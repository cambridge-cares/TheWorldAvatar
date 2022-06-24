# Digital Twin Visualisation Framework (DTVF)

A central framework for Digital Twin Visualisations (the Digital Twin Visualisation Framework, or DTVF) has been created so that in most cases, the process of creating a new visualisation to display pre-generated data is as simple as possible. The goal is that a developer that is inexperienced with Javascript (or the Javascript libraries we're using) can get a reasonable visualisation of the data by simply ensuring the data meets a set format and providing some basic metadata files.

This directory houses the JS, CSS, and HTML files that make up the framework along with some configuration files to make them available for deployment/hosting. When creating visualisations, the hosted version of this framework should be used; files from this directory should not be copied into/directly linked to visualisation implementations.

For more details on the framework, see the [Digital Twin Visualisations](https://github.com/cambridge-cares/TheWorldAvatar/wiki/Digital-Twin-Visualisations) page on the GitHub wiki. For an example implementation of the framework, see the [example-visualisation](https://github.com/cambridge-cares/TheWorldAvatar/tree/main/web/digital-twin-vis-framework/example-visualisation) directory of the repository.

## Components

The framework currently consists of the following classes/files:

- `src/js/manager.js`: Coordinator and interface for all DTVF functions.
- `src/js/data_registry.js`: Handles reading and storing meta-data.
- `src/js/source_handler.js`: Reads, stores, and loads data sources. 
- `src/js/layer_handler.js`: Creates and configures MapBox layers. 
- `src/js/icon_handler.js`: Loads image icons for display on the map. 
- `src/js/control_handler.js`: Creates and handles camera/terrain/layer controls. 
- `src/js/panel_handler.js`: Creates and handles the side panel. 
- `src/js/timeseries_handler.js`: Parses time series data and loads charts. 
- `src/js/interaction_handler.js`: Sets up map interactions.
- `src/js/json_tree.js`: Handles generation of trees to show JSON data. 

- `src/css/dtvf.css`: Styling 

## Requirements

To function correctly, whichever visualisation implementation is using this framework also needs to include the following JS libraries. For more details, see the provided [example-visualisation](https://github.com/cambridge-cares/TheWorldAvatar/tree/main/web/digital-twin-vis-framework/example-visualisation).

- [MapBox GL JS](https://docs.mapbox.com/mapbox-gl-js/api/)
- [JQuery & JQuery UI](https://jquery.com/)
- [ChartJS](https://www.chartjs.org/)
- [Moment & ChartJS Moment Adapter](https://momentjs.com/)
- [Turf](https://turfjs.org/)
- [SunCalc](https://github.com/mourner/suncalc)

## Deployment

A `grunt` directory is also present to contain a number of configuration files that allow the generation of a Docker container housing a [Grunt](https://gruntjs.com/) installation that can be used to concatenate JS files, then minify them (and CSS files) for deployment to a CDN.

To make the DTVF ready for deployment:

- Ensure the `grunt/GruntFile.js` script contains the correct configuration (if adding new files to the librarie, the gruntfile may need to be updated). 
- Run `docker-compose up` from within this directory.
- If successful, a single JS and CSS files will be generated within the `output` directory.
- These can then be uploaded to a web server to make them publicly available.
  - To upload these to the KG website server, please contact the support team at CMCL Innovations.