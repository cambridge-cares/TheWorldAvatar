# Example Visualisation (Cesium)

This example visualisation has been put together to demonstrate the intended use of the centralised Digital Twin Visualisation Framework (DTVF). This framework has been designed to make it easier for users not experienced with Typescript (or the mapping libraries) to quickly & easily put together a new Digital Twin visualisation. It is intended for developers to use this example visualisation to gain an understanding of the DTVF before attempting to create their own visualisation; to do that, this example can be copied and used as a starting point.

It is recommended that you read the [Digital Twin Visualisations](https://github.com/cambridge-cares/TheWorldAvatar/wiki/Digital-Twin-Visualisations) page of the GitHub wiki before continuing with this document. It's also worth noting that this example uses version 3.0.0 of the DTVF, hosted on a remote CMCL server and not the raw TypeScript files within the library directory.

<img src="readme-example.JPG" alt="Example of 2D data on a Mapbox visualisation" width="100%"/>

## Restrictions

It should be noted that this example uses the Cesium JS library, and makes no use of Cesium's premium offering, Cesium Ion. Use of Cesium Ion, or the features it offers, for commercial or funded educational use is prohibited without a paid-for licence.

At the time of writing, this means that anything that requires the use of a Cesium Ion API key should be avoided (this has been confirmed by Cesium's support team). To be fully sure no premium features are being used, it is suggested that no API key is even used within the code. Premium features to be avoided include the following:

- Using the satellite imagery provided by Cesium
- Using the terrain elevation provided by Cesium

Alternatives to this paid-for features are detailed below. For more details read their licencing page [here](https://cesium.com/platform/cesium-ion/pricing/).

## Mapping Capabilities

Unlike the DTVF capabilities with the 2D mapping provider (MapBox), not all Cesium JS features are supported within the DTVF. This is because Cesium JS requires explicit method calls for each type of data, rather than the more generic approach of passing in JSON configuration objects.

At the time of writing, the following 3D data formats are supported within the DTVF:

- KML files:
  - One, or more, KML files can be loaded but **cannot** be tiled. This means that the visualisation will generally handle total KML data up to ~100MB. Any more and performance suffers enough that other formats should likely be used.
- glTF files:
  - One, or more, non-tiled glTF or glB files.
- WMS endpoint:
  - 2D data can be loaded from WMS endpoints.
- 3D tiles:
  - 3D tiles can be loaded via their JSON index file. See the [Cesium 3D Tiles page](https://cesium.com/why-cesium/3d-tiles/) for more details.

Additional formats, provided they are supported by Cesium JS, can be added but will require development resource from the team at CMCL. Get in touch with them for details.

At the time of writing, client-side styling is not implemented. Style options for 3D data should be baked into the associated model files, whilst styling for 2D data (provided via WMS) should be carried out on the server.

Custom terrain elevation has also yet to be implemented at the time of writing. In theory this can be handled by loading elevation data into Geoserver, then using a [Third Party Plugin](https://github.com/kaktus40/Cesium-GeoserverTerrainProvider) to provide that data as terrain elevation to Cesium. Note that this has not yey been tried.

## Configuration

Configuration for the visualisation is provided via a number of local JSON files. Each of these is detailed below.

- `visualisation.json`:
  - This required file contains a hierarchal specification of data groups. Each group can either house sub-groups, or individual data sources and layers for display. The structure of these groups defines the layer selection tree to the left of the visualisation. The required format for this file is listed below.
- `icons.json`:
  - This optional file is used to list any image files required by the mapping library. Each image is specified with a unique name and a URL to the image file (which can be local or remote).
- `links.json`:
  - This optional file is used to provide links to additional resources; if present these are shown in the side panel of the visualisation.

In addition to these JSON files, areas of the `index.html` file can also be adjusted to change the default side panel content of the visualisation. Please note however that not all areas of this file are configurable, some HTML elements are required by the framework and had to be setup here rather than dynamically injected by the framework itself. Areas that are considered configurable are clearly commented within the HTML file.

Please note that the `index.html` file also required users to input their Mapbox API key, this is so that the terrain imagery can be pulled from Mapbox's free API rather than using imagery from Cesium Ion (which would require a licence).

### Visualisation JSON File

The `visualisation.json` file is the core configuration file for the visualisation and defines what data is loaded and shown, so it's worth explaining it's formatting a little. Each node represents a group of data. Each group can contain data sources and layers and/or sub-groups. The hierarchy of these groups is completely up to the writer of the file and is used to build the selection tree within the visualisation. The `name` parameter specifies the group's user-facing name, and the `stack` parameter is the base URL for the stack containing that group's metadata (note that if not using the metadata, this parameter can be any old URL).

Each group can then contain a number of `sources`, representing individual data files/endpoints that will be loaded into memory/queried by the mapping library. Each source node requires a unique `id` parameter, this is used within the DTVF to keep track of sources. In addition to `sources`, each group can define a number of `layers`. These are the visual representations of the aforementioned sources. Whilst Cesium JS does not have a internal division between data sources and visual representations, this approach is still used within the configuration file for consistency with other mapping providers.

Source nodes need to provide a unique `id` field, a `type` field (`kml|gltf|wms|tiles`), and a `uri` field pointing towards the data file to be loaded. Some types of sources also require additional parameters:

- For `gltf` sources, additional `position` and `orientation` fields are required.
- For `wms` sources, additional `wmsLayer`, `transparency`, and `format` fields are required.

Layer nodes also need to provide a unique `id` field, a `source` field (listing the id of the source to use), and an public facing `name` field to use within the selection tree. Note that the `name` field can be shared with other layers, these entries will be combined into a single tree selection.

For developers creating their first visualisation, it is recommended to take a copy of this example and play around with the `visualisation.json`, perhaps changing the hierarchy and/or getting comfortable with the Mapbox styling format.

## Sample Data

A small amount of sample data has been committed to demonstrate the power of the DTVF to visualisate different data types. Please do not make changes to the sample data without consulting the original developer. At the time of writing, the sample data sets include:

- **New York**:
  - Tiled 3D buildings, loaded from a remote CMCL server.
  - 2D river data from a WMS endpoint provided by Cornell University.
  - No metadata or timeseries present in this data set.

It's worth noting that with this sample data, no stack is running so no support for dynamic metadata or timeseries is available. This is something that we plan to work on in future, no true generic solution exists for this so far.

## Building the Image

The `docker` folder contains the required files to build a Docker Image for the example visualisation; the `Dockerfile` file contains the instructions to build an Image. Please note the caveats below before attempting to build the service using Docker:

- The example visualisation installed within the Docker image will be based on the current commit of this repository, please ensure it is the correct one.
- A valid Mapbox API token must be provided in your `index.html` file.
- A connection to the internet is required to contact remote resources and use the mapping libraries.

Once the requirements have been addressed, the image can be built using the below methods. If changing the visualisation, you'll need to rebuild and rerun the Docker image after and adjustments, or setup a Docker bind mount so that local changes are reflected within the container.

- To build the Image:
  - `docker-compose -p psdt-vis-cesium -f ./docker/docker-compose.yml build --force-rm`
- To generate a Container (i.e. run the Image):
  - `docker-compose -p psdt-vis-cesium -f ./docker/docker-compose.yml up -d --force-recreate`
