# PSDT Visualisation (Cesium)

This visualisation uses the Digital Twin Visualisation Framework (DTVF). This framework has been designed to make it easier for users not experienced with Typescript (or the mapping libraries) to quickly & easily put together a new Digital Twin visualisation.

It is recommended that you read the [Digital Twin Visualisations](https://github.com/cambridge-cares/TheWorldAvatar/wiki/Digital-Twin-Visualisations) page of the GitHub wiki before continuing with this document. It's also worth noting that this visualisation uses the DTVF hosted on a remote CMCL server.

## Restrictions

It should be noted that this visualisation uses the Cesium JS library, and makes no use of Cesium's premium offering, Cesium Ion. Use of Cesium Ion, or the features it offers, for commercial or funded educational use is prohibited without a paid-for licence.

At the time of writing, this means that anything that requires the use of a Cesium Ion API key should be avoided (this has been confirmed by Cesium's support team). To be fully sure no premium features are being used, it is suggested that no API key is even used within the code. Premium features to be avoided include the following:

- Using the satellite imagery provided by Cesium
- Using the terrain elevation provided by Cesium

Alternatives to this paid-for features are detailed below. For more details read their licencing page [here](https://cesium.com/platform/cesium-ion/pricing/).

## Mapping Capabilities

Unlike the DTVF capabilities with the 2D mapping provider (Mapbox), not all Cesium JS features are supported within the DTVF. This is because Cesium JS requires explicit method calls for each type of data, rather than the more generic approach of passing in JSON configuration objects.

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

At the time of writing, client-side styling is not implemented (i.e. you cannot specify styling in the JSON files as you can do for Mapbox visualisations). Style options for 3D data should be baked into the associated model files, whilst styling for 2D data (provided via WMS) should be carried out on the server (more information on server-side styling can be found [here](https://docs.geoserver.org/stable/en/user/styling/index.html)).

## Configuration

Configuration for the visualisation is provided via a number of local JSON files. Each of these is detailed below.

- `data.json`:
  - This required file contains a hierarchal specification of data groups. Each group can either house sub-groups, or individual data sources and layers for display. The structure of these groups defines the layer selection tree to the left of the visualisation. The required format for this file (when specifying 3D data) is listed below.

- `settings.json`:
  - This required file contains global settings (i.e. not specific to data sets) for the visualisation as a whole. Items like the map's starting location, available imagery layers, and fields available for feature searching are set here. For more details please see the [GitHub wiki page](https://github.com/cambridge-cares/TheWorldAvatar/wiki/Digital-Twin-Visualisations). 

- `icons.json`:
  - This optional file is used to list any image files required by the mapping library. Each image is specified with a unique name and a URL to the image file (which can be local or remote).

- `links.json`:
  - This optional file is used to provide links to additional resources; if present these are shown in the side panel of the visualisation.

In addition to these JSON files, areas of the `index.html` file can also be adjusted to change the default side panel content of the visualisation. Please note however that not all areas of this file are configurable, some HTML elements are required by the framework and had to be setup here rather than dynamically injected by the framework itself. Areas that are considered configurable are clearly commented within the HTML file.

Please note that the `index.html` file also required users to input their Mapbox API key, this is so that the terrain imagery can be pulled from Mapbox's free API rather than using imagery from Cesium Ion (which would require a licence).

### Data Specification File

The `data.json` file is a core configuration file for the visualisation and defines what data is loaded and shown, so it's worth explaining its formatting a little. Each node represents a group of data. Each group can contain data sources and layers and/or sub-groups. The hierarchy of these groups is completely up to the writer of the file and is used to build the selection tree within the visualisation. The `name` parameter specifies the group's user-facing name, and the `stack` parameter is the base URL for the stack containing that group's metadata (note that if not using the metadata, this parameter can be any old URL).

Each group can then contain a number of `sources`, representing individual data files/endpoints that will be loaded into memory/queried by the mapping library. Each source node requires a unique `id` parameter, this is used within the DTVF to keep track of sources. In addition to `sources`, each group can define a number of `layers`. These are the visual representations of the aforementioned sources. Whilst Cesium JS does not have a internal division between data sources and visual representations, this approach is still used within the configuration file for consistency with other mapping providers.

Each group can also (optionally) contain an `expanded` boolean field. If set to false, then this group (and all of its children) will be collapsed by default within the selectable layers tree; any other value, or no field at all, will default to expanded. Note that this does not affect the default selection state of individual layers.

#### Sources

Source nodes need to provide a unique `id` field, a `type` field (`kml|gltf|wms|tiles`), and a `uri` field pointing towards the data file to be loaded. Some types of sources also require additional parameters:

- For `gltf` sources, an additional `position` field is required.
  - The `position` field is a three value array of the form `[longitude, latitude, height]`.
- For `wms` sources, additional `wmsLayer`, `transparency`, and `format` fields are required.
- For `tiles` sources, optional `position` and `rotation` fields can also be set.
  - The `position` field is a three value array of the form `[longitude, latitude, height]`.
  - The `rotation` field requires the `position` field to be present, and is a three value array of the form `[roll, pitch, heading]`. Cesium defines Roll as the rotation about the positive X axis, Pitch as the rotation about the negative Y axis, and Heading as the rotation about the negative Z axis.

#### Layers

Layer nodes also need to provide a unique `id` field, a `source` field (listing the id of the source to use), and an public facing `name` field to use within the selection tree. Note that the `name` field can be shared with other layers, these entries will be combined into a single tree selection. A `visibility` field with values of `visible|none` can also be added to change the default selection state of that layer.

Layers can also optionally include an integer `order` field (which defaults to 0 if not specified). Before visualising, all layers (across all groups) are sorted by their order from lowest to highest; this allows users to specify the Z order of their data, regardless of grouping.

Note that, at the time of writing, all `source` and `layer` nodes must be within a `group` (i.e. data cannot be loaded unless within a group), and a single top-level group must exist (i.e. the `data.json` file must be a JSON object, rather than a JSON array).

For developers creating their first visualisation, it is recommended to take a copy of this example and play around with the `data.json`, perhaps changing the hierarchy and/or getting comfortable with the Mapbox styling format. 

### Global Settings & Advanced Features

Configuration settings for features not specifically tied to an individual mapping library can be read on the [GitHub wiki](https://github.com/cambridge-cares/TheWorldAvatar/wiki/DTVF:-Settings), features specific to CesiumJS are detailed in the sections below.

These features currently include:

- [Changing the available (and default) map imagery](https://github.com/cambridge-cares/TheWorldAvatar/wiki/DTVF:-Settings#map-imagery)
- [Overriding expected feature property names](https://github.com/cambridge-cares/TheWorldAvatar/wiki/DTVF:-Settings#feature-fields)
- [Defining custom attribution text](https://github.com/cambridge-cares/TheWorldAvatar/wiki/DTVF:-Settings#attribution)

#### Map Position

The default position of the map can be specified via the start field of the settings file. The specific fields within this node differ depending on the map provider; an example the CesiumJS version can be seen below. Note that these settings represent the position of the camera itself, not what it is looking at. In this Cesium JS case, the opacity of the globe itself can also be set here.

```json
"start": {
    "center": [7.621435, 49.180285, 50],
    "heading": 90,
    "pitch": -45,
    "roll": 0.0,
    "opacity": 0.5
}
```

#### Terrain Elevation

Terrain elevation data can also be provided via use of a `terrain` variable in the `settings.json` file. The value this variable is passed directly to a new [CesiumTerrainProvider](https://cesium.com/learn/cesiumjs/ref-doc/CesiumTerrainProvider.html) instance, as such users need to ensure that their settings conform with the CesiumJS API.

An example specification of terrain elevation is shown below. Note that in this case, the data is pulled from quantized mesh tiles provided by [MapTiler](https://cloud.maptiler.com/), a service that is **not permitted for commercial use** without a paid-for licence.

```json
"terrain": {
    "url": "https://api.maptiler.com/tiles/terrain-quantized-mesh-v2/?key=API_KEY",
    "requestVertexNormals": true
}
```

## Building the Image

The `docker` folder contains the required files to build a Docker Image for the example visualisation. This uses the `dtvf-base-image` image as a base then adds the contents of the `webspace` directory to a volume mounted at `/var/www/html` within the container.

- Files to be hosted must be contained within the `webspace` directory.
- A valid Mapbox API token must be provided in your `index.html` file.
- A connection to the internet is required to contact remote resources and use the mapping libraries.

Once the requirements have been addressed, the image can be built. If changing the webspace contents only, you will not need to rebuild or rerun the Docker image. The webspace is mounted as a volume, so that local changes are reflected within the container.

- To (re)build the image and (re)deploy the container, run `./redeploy.sh`.

## Troubleshooting

For details on common issues/questions that may appear when using Cesium JS, please see the dedicated [Troubleshooting](https://github.com/cambridge-cares/TheWorldAvatar/wiki/DTVF:-Troubleshooting) page on the GitHub wiki.

