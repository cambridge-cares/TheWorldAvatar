# Overview

A central framework for visualisations (the TWA Visualisation Framework, or TWA-VF) has been created so that in most cases, the process of creating a new visualisation to display pre-generated data is as simple as possible. The goal is that a developer that is inexperienced with Typescript (or the JavaScript libraries we're using) can get a reasonable visualisation of the data by simply ensuring the data meets a set format and providing some basic configuration files.

To avoid asking each visualisation user to write their own code interfacing with the mapping provider, the TWA-VF provides the functionality to read data source and layer definitions from a custom configuration file then automatically call the relevant library methods to display them on the map.

Based on the structure of this configuration file, a Layer tree component is generated to allow users to show/hide individual (or groups of) layers at will. In addition, controls to change the camera location, base imagery, and 3D terrain are also generated.

Once displayed, a number of standard interaction handlers are also added. These add common functionality such as displaying the name of a feature on mouse-over, showing a feature's details in the side panel when clicked, and contacting the [FeatureInfoAgent](https://github.com/cambridge-cares/TheWorldAvatar/tree/main/Agents/FeatureInfoAgent) for more detailed metadata on a feature.

<br/>

## Mapping providers

At the time of writing the available mapping providers are [Mapbox](https://www.mapbox.com/) and [Cesium](https://cesium.com/platform/Cesium/). The core differences between providers is as follows:

* Mapbox can only handle 2D data (with the option to extrude 2D polygons into basic 3D polyhedrons) from local files or from [WMS endpoints](https://en.wikipedia.org/wiki/Web_Map_Service). Unlike Cesium (see below), Mapbox can display 2D vector data (including use of SVGs for icons, under certain conditions) if the data is hosted using the [Mapbox Vector Tiles](https://docs.mapbox.com/data/tilesets/guides/vector-tiles-introduction/) format. It is however quite customisable and has relatively small performance overhead. Unless you're plotting 3D building data, it's advised to use this mapping provider.

* Cesium can handle 2D raster data as well as 3D data. 2D data must be provided via a WMS endpoint and can only be styled on the server hosting it (rather than the client-side styling Mapbox provides); at the time of writing we have not implemented functionality to support the _display_ of 2D vector data (GeoServer can host vector data, but Cesium will rasterise this as a PNG upon visualisation). This provider also has a large performance overhead, a decent GPU is required for a smooth visualisation; we recommend only using it if 3D data is required.

<br/>

## Creating a container

Before we can start specifying the data to be hosted within the visualisation, we need to create a Docker container that can host the web files the visualisation uses. This can be done by running a container based on the `twa-vf` image; an image that contains the pre-built TWA-VF libraries (available from the `/var/www/html/twa-vf` directory) and a webserver.

Users can either write their own `docker-compose.yml` file to run a standalone visualisation (i.e. outside of a TWA Stack environment), or use the TWA Stack to create a standard visualisation integrated within a stack instance (see the [TWA Stack Manager documentation](https://github.com/cambridge-cares/TheWorldAvatar/tree/main/Deploy/stacks/dynamic/stack-manager) for more details on the latter).

### Importing the library

Once you have an container ready to go, you'll need to ensure that your `index.html` correctly imports the required TWA-VF JS and CSS file, along with the required external libraries. Whilst the example visualisations show how to do this, a breakdown of how this is currently done, compared to previous methods, is detailed below.

**Before version 3.8.0:**<br/>

Before this version, the compiled JS and CSS files were not stored within the Docker image itself, but hosted on a remote server provided by CMCL. Whilst this gave the ability to easily change the version of the library used, it meant that if the server at CMCL went offline, visualisations were broken. An example JS import statement for v3.7.0 of the DTVF (previous name for the TWA-VF) is shown below.

```html
<script src="https://kg.cmclinnovations.com/cdn/twa-vf/4.4.0/dtvf.min.js"></script>
```

**Version 3.8.0 and after:**<br/>

To remove the reliance on remote servers, the TWA-VF docker image is now versioned (rather than just using `:latest`) and contains a copy of the compiled JS and CSS files within it. This means that users can import a local file (as shown below); not the addition of a character encoding, this has been shown to resolve some character rending issues we've seen.

```html
<script src="./lib/twa-vf.min.js" charset="utf-8"></script>
```

##  Writing the data configuration file

The first step for any prospective user of the TWA-VF is to understand how to structure the configuration file. At the time of writing, this file must be named `data.json` and reside within the root webspace (i.e. next to your visualisation's `index.html` file).

The TWA-VF configuration file is a [JSON](https://en.wikipedia.org/wiki/JSON) file that specifies the data sources (where and how the data is loaded) and data layers (how that data is visualised). These sources and layers are contained within hierarchal data groups, how these groups are nested is up to the writer of the configuration file.

The following subsections detail how to generate your own configuration file. Please note however that whilst the majority of the config format is the same across all mapping library providers, some parameters are only supported for specific libraries (these are detailed in other documentation).

### Defining a group

Each data group contains a number of parameters (detailed below), and can house multiple sub-groups to form a custom hierarchy.

* `name` (required): This is the user facing name of the data group.
* `stack` (optional): This is the URL for the stack containing metadata on this group's data. Note that this should be the base URL of the stack (i.e. without "/geoserver"). If missing, dynamic metadata from a remote FeatureInfoAgent cannot be utilised.
* `sources` (optional): This is an array of objects defining data sources (see below for info on sources).
* `layers` (optional): This is an array of objects defining data layers (see below for info on layers).
* `groups` (optional): This is an array of further data groups, used to build the data hierarchy.
  
Definitions of data sources and layers is optional within a data group so that groups can be added just to hierarchal purposes rather than _having_ to house data in every group.

```json
{
    "name": "My Example Group",
    "stack": "https://my-example-website.com/stack",
    "sources": [
        ...
    ],
    "layers": [
        ...
    ],
    "groups": [
        ...
    ]
}
```

### Defining a source

Each group can contain a number of sources, representing individual data files or endpoints that will be loaded into memory by the chosen mapping library. Sources can also be defined in top-level groups, then utilised by layers further down the hierarchy.

Definitions of sources vary depending on which mapping provider is chosen (as they all support different types of data). Specific parameters used for each mapping library are detailed in later parts of the TWA-VF documentation, the common parameters are detailed below.

* `id` (required): This is the internal ID of the source. It needs to be unique within the current group, but is not required to be globally unique.
* `type` (required): This is the type of the source. Acceptable values differ depending on the mapping library (see the library specific documentation for details).

```json
{
    "id": "example-mapbox-source",
    "type": "geojson",
    "data": "./my-example-data.geojson"
}
```

### Defining a layer

Each group can also contain a number of layers, defining how the data is visualised on screen. Layers can utilise sources defined in groups higher in the hierarchy; additionally multiple layers can visualise data from the same source.

As with sources, definitions of layers vary depending on which mapping provider is chosen (as they all support different styling options). Specific parameters used for each mapping library are detailed in later parts of the TWA-VF documentation, the common parameters are detailed below.

* `id` (required): This is the internal ID of the layer. It needs to be unique within the current group, but is not required to be globally unique.
* `name` (required): This is the user facing name of the layer (that will appear in the tree). Multiple layers can use the same name, they'll be combined in a single entry in the tree.
* `source` (required): This is the ID of the source used to populate the layer.

```json
{
    "id": "example-mapbox-layer",
    "name": "My Example Data",
    "source": "example-mapbox-source"
}
```

## Writing the settings configuration file

Whilst the `data.json` configuration file defines the data to be loaded into the visualisation, the `settings.json` configuration file defines global settings. These are often linked to the behaviour of the visualisation itself rather than the data present within it.

The most common use of the settings configuration file is to define the state of the map when the visualisation first loads. This is done through the `start` parameter, the exact format for each mapping provider can be see in their respective "Working with..." documentation pages.

More complex features can also be setup and configured using the settings file, these are detailed within the [Advanced features](./advanced.md) documentation.

## Additional configuration

In addition to the main `data.json` and `settings.json` configuration files, two optional files can also be added. These files are detailed below:

* `icons.json`:
  * This optional file is used to list any image files required by the mapping library. Each image is specified with a unique name and a URL to the image file (which can be local or remote). Adding "-sdf" to the icon's file name will ensure that the TWA-VF registers the image as an [SDF icon](https://docs.mapbox.com/help/troubleshooting/using-recolorable-images-in-mapbox-maps/) within Mapbox, enabling dynamic color changing (providing that the icon has been setup correctly).

* `links.json`:
  * This optional file is used to provide links to additional resources; if present these are shown in the side panel of the visualisation.

## Setting up the webspace

The TWA-VF provides a base image that can be used to host visualisations (`ghcr.io/cambridge-cares/TWA-VF-base-image:latest`). Once pulled, the image simply needs to be connected to a bind mount at the `/var/www/html` location within the container.

This webspace should house all the files needed to display your visualisation. This includes the `data.json` file as well as the main `index.html` file, any local data files, and any required images. Custom CSS and JS files can be added to provide bespoke functionality as needed.

The example Mapbox and Cesium visualisation show how to create a container using the TWA-VF base image and a webspace bind mount, all within the context of a single TWA Stack instance.

## Editing the HTML file

Within the webspace, an `index.html` file should be provided to display the UI elements and import the required JS libraries (including which version of the TWA-VF is used).

The `index.html` file of the example Mapbox & Cesium visualisations has been produced to act as a template for users creating their own visualisations. Once the template is copied, certain areas of it can be adjusted to add custom content; these sections are marked within "CUSTOMISABLE" comment sections.


## Setting credentials 

In addition to the aforementioned configuration files, two additional files are required to house a Mapbox username and associated API key. Note these are required, even in Cesium visualisations, as the base map imagery is still provided by Mapbox.

To set these two files, either create and populate `mapbox_username`, and `mapbox_api_key` files within the hosted webspace, or use the stack infrastructure to provide these as Docker secrets. You can learn more about the latter by reading [the stack's documentation](https://github.com/cambridge-cares/TheWorldAvatar/tree/main/Deploy/stacks/dynamic/stack-manager).

Once present, these files are queried by the TWA-VF, loading in the required credentials. Note that previous versions of the TWA-VF required these parameters to be set within each visualisation's `index.html` file, this is no longer required (see the example visualisations to learn about the new format).

It's worth noting that these credential files should **not** be committed; to that end, a `.gitignore` file has been added to prevent this.

## Dynamic meta data

Display of meta and timeseries data is also a feature offered by the TWA-VF (regardless of the chosen mapping provider). However, the processing of getting this system setup can be quite lengthy.

To query for dynamic data, each selectable feature of your data also needs to contain `iri` and `endpoint` properties. Once selected, these are sent to a remote agent ([FeatureInfoAgent](https://github.com/cambridge-cares/TheWorldAvatar/tree/dev-feature-info-agent/Agents/FeatureInfoAgent)) running in a stack. Data is queried from the knowledge graph and/or relational database, then returned for display in the visualisation's side panel.

A breakdown of the requirements to use this system are below, for more information check out the FeatureInfoAgent's documentation.

* A stack instance needs to be running (at some location, can be remote), containing:
  * A Blazegraph instance holding metadata on the visualised features.
  * An instance of the [FeatureInfoAgent](https://github.com/cambridge-cares/TheWorldAvatar/tree/dev-feature-info-agent/Agents/FeatureInfoAgent) with a mapping of the possible feature classes to pre-written SPARQL queries. These queries must return data in a specific tabular format.
  * If applicable, a PostgreSQL instance containing time series data.
* Geospatial data needs to contain `iri`, and `endpoint` fields for each feature (regardless of how the data is served, i.e. locally or via WMS).
  * The `iri` field needs to contain the full IRI of the feature as represented in the knowledge graph.
  * The `endpoint` field needs to contain the URL of the Blazegraph namespace containing data on the feature. Note that this can be absolute or relative to the FeatureInfoAgent's location.