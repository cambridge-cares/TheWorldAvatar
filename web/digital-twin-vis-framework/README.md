# Introduction

A central framework for Digital Twin Visualisations (the Digital Twin Visualisation Framework, or DTVF) has been created so that in most cases, the process of creating a new visualisation to display pre-generated data is as simple as possible. The goal is that a developer that is inexperienced with Javascript (or the Javascript libraries we're using) can get a reasonable visualisation of the data by simply ensuring the data meets a set format and providing some basic configuration files.

Please note that the DTVF can only plot data that is provided in the accepted formats (more on this below), restricts some features if the data isn't organised (again, more on this below), and only produces a basic visualisation. Developers that want to display non-standard data, or create more complex map interactions are free to use the DTVF as a base, but will likely need to handle these additions themselves.

The current, working version of the DTVF is **3.3.2**.

# Technical Overview

The DTVF is written in [TypeScript](https://www.typescriptlang.org/) and compiled into a single minified JS file (and a single minified CSS file). These files are then hosted on a CMCL server that allows remote imports. These are then included within each visualisation's `index.html` file as client-side libraries. A number of abstract base classes exist within the DTVF library, with concrete implementations for each Mapping Provider.

At the time of writing the available mapping providers are [Mapbox](https://www.mapbox.com/) and [CesiumJS](https://cesium.com/platform/cesiumjs/). The core differences between providers is as follows.

* Mapbox can only handle 2D data (with the option to extrude 2D polygons into basic 3D polyhedrons) from local files or from [WMS endpoints](https://en.wikipedia.org/wiki/Web_Map_Service). Unlike CesiumJS (see below), Mapbox can display 2D vector data (including use of SVGs for icons, under certain conditions) if the data is hosted using the [Mapbox Vector Tiles](https://docs.mapbox.com/data/tilesets/guides/vector-tiles-introduction/) format. It is however quite customisable and has relatively small performance overhead. Unless you're plotting 3D building data, it's advised to use this mapping provider.

* CesiumJS can handle 2D data as well as 3D data. 2D data must be provided via a WMS endpoint and can only be styled on the server hosting it (rather than the client-side styling Mapbox provides); at the time of writing we have not implemented functionality to support the _display_ of 2D vector data (GeoServer can host vector data, but Cesium will rasterise this as a PNG upon visualisation). This provider also has a large performance overhead, a decent GPU is required for a smooth visualisation; we recommend only using it if 3D data is required.

To use the DTVF, developers just need to provide some JSON configuration files and a `index.html` file that imports the remote DTVF library (along with other dependencies), adds some required HTML elements, then calls the startup routine. Template HTML files can be found within the visualisation examples (listed below).

On startup, the DTVF reads a number of those JSON configuration files (all expected to be local to the visualisation, i.e. next to the `index.html` file), constructs internal, hierarchal representations of data sources and layers, then calls provider specific classes to handle adding those sources and layers to the map for visualisation.

Display of meta and timeseries data is also a feature offered by the DTVF (regardless of the chosen mapping provider), however this functionality is still under development and requires data to be hosted within a [stack instance](https://github.com/cambridge-cares/TheWorldAvatar/tree/main/Deploy/stacks/dynamic/stack-manager), with a specific agent running, and the geospatial data to meet certain standards. If these conditions are not met, any metadata baked directly into the geospatial data is shown instead; it's recommended to use this approach for now.

A brief list of these requirements is shown below, but as the current implementation is considered deprecated, it is not advised that developers use it. If absolutely required, more information can be found within the README of the FeatureInfoAgent.

* A stack instance needs to be running (at some location, can be remote), containing:
  * A blazegraph instance holding metadata on the visualised features.
  * An instance of the [FeatureInfoAgent](https://github.com/cambridge-cares/TheWorldAvatar/tree/dev-feature-info-agent/Agents/FeatureInfoAgent) with a mapping of the possible feature classes to pre-written SPARQL queries. These queries must return data in a specific tabular format (i.e. columns of data titled "label", "value", and optionally, "unit").
  * If applicable, a PostgreSQL instance containing timeseries data for the visualisation features.
* Geospatial data needs to contain `name`, `iri`, and `endpoint` fields for each feature (regardless of how the data is served, i.e. locally or via WMS).
  * The `name` field needs to contain the human readable name of the feature.
  * The `iri` field needs to contain the full IRI of the feature as represented in the knowledge graph.
  * The `endpoint` field needs to contain the URL of the Blazegraph namespace containing data on the feature. Note that this can be absolute or relative to the FeatureInfoAgent's location.

# Developing the DTVF

Changes to the DTVF can be made by adjusting the TypeScript class files within the [library](https://github.com/cambridge-cares/TheWorldAvatar/tree/main/web/digital-twin-vis-framework/library) directory. These updated TypeScript classes can then be compiled into JavaScript and minified using the provided Docker containers. For other developers to use, the final JS and CSS file will then need to be uploaded to the CMCL server, ready for remote import.

For more information on the DTVF library, see the associated README file within the [library](https://github.com/cambridge-cares/TheWorldAvatar/tree/main/web/digital-twin-vis-framework/library) directory.

# Visualisation Examples

To act as an example of how to use the DTVF, as well as a template upon which new visualisations can be based, two example visualisations have been produced. One to show the Mapbox implementation, and one to show CesiumJS.

Before attempting to create their own visualisations, developers should try running the appropriate example visualisation and read its associated README. These examples can be found within the [DTVF](https://github.com/cambridge-cares/TheWorldAvatar/tree/main/web/digital-twin-vis-framework) directory.

# Troubleshooting/FAQs

Solutions to common issues, and answers to common questions, relating to the DTVF can be found on the [Troubleshooting](https://github.com/cambridge-cares/TheWorldAvatar/wiki/DTVF:-Troubleshooting) page.

# Support

For support on using the visualisations, or to discuss any potential changes to the DTVF, please contact the technical team at [CMCL Innovations](https://cmclinnovations.com/).
