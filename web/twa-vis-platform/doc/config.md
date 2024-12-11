# Customisation of Platform Content

Given that the platform is designed to be generalisable, this directory exists to allow users to customise their web contents according to their needs. It acts as the target for a Docker volume or bind mount, and should be mounted to the `/twa/public` directory within the deployed container. Files within it can then be accessed using the `$HOST/...` URL route.

The uploaded content provided by the deploying developer should match the directory structure below. Please read the respective sections for the specific instructions. Do note that the example configuration are intended to disseminate information and may not be functional (i.e. to set up the platform). If you require sample files of a working example, please have a look over at the [example](../example/) directory.

- [`config/`](#1-configuration): Contains config/settings files.
- [`images/`](#2-assets): Custom image files.
- [`optional-pages/`](#3-optional-pages): Markdown files for optional static content (with metadata from [gray-matter](https://www.npmjs.com/package/gray-matter)).

## Table of Contents

- [Customisation of Platform Content](#customisation-of-platform-content)
  - [Table of Contents](#table-of-contents)
  - [1. Configuration](#1-configuration)
    - [1.1 UI Settings](#11-ui-settings)
    - [1.2 Map Settings](#12-map-settings)
      - [1.2.1 General Settings](#121-general-settings)
      - [1.2.2 Map Data Settings](#122-map-data-settings)
        - [Dataset: Defining a group](#dataset-defining-a-group)
        - [Dataset: Defining a source](#dataset-defining-a-source)
        - [Dataset: Defining a layer](#dataset-defining-a-layer)
  - [2. Assets](#2-assets)
  - [3. Optional Pages](#3-optional-pages)
    - [3.1 Fields](#31-fields)
    - [3.2 Sample](#32-sample)

## 1. Configuration

The platform requires the following [JSON](https://en.wikipedia.org/wiki/JSON) configuration files

- [`ui-settings.json`](#11-ui-settings): UI configuration settings; **[MANDATORY]**.
- [`data-settings.json`](#121-general-settings): Specifies the urls of datasets for mapping the data sources and layers; **[OPTIONAL]**
- [`map-settings.json`](#122-map-data-settings): Non-data specific configuration for maps; **[OPTIONAL]**

### 1.1 UI Settings

The `config/ui-settings.json` file provides general settings for the platform. This includes settings for displaying modules, branding requirements, and additional resources. A brief explanation is as follows:

- `branding`: key value pairs for various branding icons such as navigation bar and logo. It requires a string `ARRAY`.
  - `navbar`: An array of logos to be placed on the left side of the navbar
  - `landing`: One logo element (within an array) for the landing page on light mode
  - `landingDark`: One logo element (within an array) for the landing page on dark mode
- `modules`: key value pairs indicating if certain modules should be available
  - `landing`: REQUIRED. Displays landing page if enabled
  - `map`: REQUIRED. Displays map visualisation if enabled
  - `dashboard`: REQUIRED. Displays the dashboard page if enabled
  - `help`: REQUIRED. Displays help page if enabled
  - `registry`: REQUIRED. Displays the registry page if enabled
- `links`: optional configuration for adding or updating redirect links on the landing page. This configuration can overwrite the defaults for the map, dashboard, and help modules. It requires an `ARRAY` of the following JSON format:
  - `url`: REQUIRED. The url is either targeted at either an external or internal link. For internal link usage, please input `map`, `dashboard`, `help`, and `registry` accordingly.
  - `title`: REQUIRED. Thumbnail title on landing page. Optional for only internal links, which defaults to the default if not set.
  - `caption`: REQUIRED. Thumbnail caption on landing page. Optional for only internal links, which defaults to the default if not set.
  - `icon`: REQUIRED. Thumbnail icon on landing page. Optional for only internal links, which defaults to the default if not set.
- `resources`: optional configuration for additional resources. They follow the following format
  - `resourceName`: indicates the type of resource required - dashboard, scenario
    - `url`: REQUIRED. url of the resource
    - `data`: optional dataset indicator that is only used with scenario resources to target required dataset for scenarios

Note that resources are optional and their configuration options can differ from each other. Please note the list of available resources and their possible options as follows:

- Dashboard: Activate the `analytics` page with an dashboard embedded as an IFrame based on only the `url` parameter.
- Scenario: Enables scenario selection in the `map` page
  - `url`: This is a required field that specifies the URL from which the scenarios and their settings can be retrieved. In this example, the URL points to a stack deployed on theworldavatar.io platform.
  - `data`: This required field indicates the target dataset that should be accessible to the user from the central stack. In the given example, the data field is set to "water", indicating that the scenario contains information only on water assets and not power nor telecoms etc.
- Registry: Activate the `registry` page based on the backend resource indicated in the `url` parameter. The registry page provides a table for viewing all records within a lifecycle, as well as pages to add, delete, edit, and view these records individually using a form UI.
  - `url`: The registry agent endpoint (close it with /), which should be able to generate a form template, csv template, and retrieve data from the knowledge graph. The form template for generating the form UI must follow the template listed in [this document](form.md).
  - `data`: The entity of interest that acts as the first landing page for the registry. This should be `contract` at the moment.

Below is an example of the contents for a valid `ui-settings.json` file with additional comments explaining each entry. The format of the file should be consistent whether implementing mapbox or cesium maps.

> [!NOTE]
> When specifying image paths, be sure to use absolute paths beggining with a `/`
<!--  -->
> [!NOTE]  
> The comments seen below are for explanation purposes only, they are not valid JSON. If wishing to use this content in production, remove the comments first.

```json
{
  "branding": {
    "navbar": ["/images/defaults/navbar-logo.svg"], // Optional custom logo for the navbar (should be 5:1 aspect ratio)
    "landing": "/images/path/to/svg/light.svg", // Landing page brand/company image for light mode
    "landingDark": "/images/path/to/svg/dark.svg" // Optional landing page brand/company image for dark mode
  },
  "modules": {
    "landing": true, // Should the landing page be enabled
    "help": true, // Should the help page be enabled
    "dashboard": false, // Should the analytics page be enabled
    "map": true, // Should the map page be enabled
    "registry": false, // Should the registry page be enabled
  },
  "links": [
    {
      "url": "map",
      "title": "Explore",
      "caption": "Describe your map here",
      "icon": "/images/path/to/svg.svg"
    }
  ],
  "resources": {
    "dashboard": {
      "url": "" // Edit dashboard url here
    },
    "registry": {
      "url": "http://sample.org/agent/", // Edit registry agent's API here
      "data": "type" // Specify only the type to reach the registry page of interest
    },
    "scenario": {
      "url": "https://theworldavatar.io/demos/credo-ofwat/central/CentralStackAgent", // Edit scenario url here
      "data": "water" // Edit scenario target dataset here
    }
  }
}
```

### 1.2 Map Settings

If the map module is enabled, developers will need to supply `data-settings.json` and `map-settings.json` files.

#### 1.2.1 General Settings

The `config/map-settings.json` file provides general map settings that are not specific to the data sets being loaded. The specific settings are:

- `type` : The map type. Either "mapbox" or "cesium" is currently accepted
- `camera` : Camera starting position and available positions
- `imagery` : Imagery options
- `legend` : Optional custom legend settings
- `icons` : A key-value map of the icon name and its corresponding url

Please note that Cesium has not been incorporated at the moment. It is intended that this file (along with other configuration files) are provided by the deploying developer via Docker volumes created with the standard TWA Stack. As such, there may be no off-the-shelf example file committed to this repository.

> Legend setting format

Icons on the map are shown by default in the layer tree. Additional legend items can be customised in this file. The current setup uses nested groups, where group names act as headers on the legend. At the moment, the available visualisation options for legends are only `icons` and `fills`. For other options, please send in a feature request. Sample settings are as follows:

> [!NOTE]  
> The comments seen below are for explanation purposes only. They are not valid JSON and should be removed.

```json
{
  "legend": {
    // Group one for icons - Only PNG, JPG, SVG, and Google Materials icon are available
    "Status Indicators": {
      // Group one, item one
      "Active": {
        "type": "symbol",
        "icon": "/images/active.svg"
      },
      // Group one, item two
      "Inactive": {
        "type": "symbol",
        "icon": "/images/inactive.jpg"
      },
      // Group one, item three
      "Unknown": {
        "type": "symbol",
        "icon": "question_mark"
      }
    },
    // Group two for fills
    "Area of interest": {
      // Group two, item one
      "Primary": {
        "type": "fill",
        "fill": "#000000"
      },
      // Group two, item two
      "Secondary": {
        "type": "fill",
        "fill": "#709ac7"
      }
    }
  }
}
```

Below is an example of the contents for a valid `map-settings.json` file for Mapbox with additional comments explaining each entry. The format of the file is mostly consistent for either mapbox or cesium maps. However, there are some differences, that will be explained when Cesium has been incorporated.

> [!NOTE]  
> The comments seen below are for explanation purposes only, they are not valid JSON. If wishing to use this content in production, remove the comments first.

```json
{
  "type": "mapbox", // Required Type of map, "mapbox" or "cesium"
  "camera": {
    "default": "Paris", // Name of starting camera position
    "positions": [
      // Selectable positions for the camera
      {
        "name": "London",
        "center": [-0.12794, 51.50774],
        "zoom": 18,
        "bearing": 0,
        "pitch": 45
      },
      {
        "name": "Paris",
        "center": [2.29448, 48.85826],
        "zoom": 16,
        "bearing": 0,
        "pitch": 45
      },
      {
        "name": "New York",
        "center": [-73.98568, 40.74845],
        "zoom": 16,
        "bearing": 0,
        "pitch": 45
      }
    ]
  },
  "imagery": {
    "default": "Auto", // Name of default map imagery, "auto" will inherit from browser theme
    "options": [
      // Mapbox base layer options, using their Style URL system
      {
        "name": "Light",
        "url": "mapbox://styles/mapbox/light-v11?optimize=true"
      },
      {
        "name": "Dark",
        "url": "mapbox://styles/mapbox/dark-v11?optimize=true"
      },
      {
        "name": "Outdoors",
        "url": "mapbox://styles/mapbox/outdoors-v12?optimize=true"
      },
      {
        "name": "Satellite",
        "url": "mapbox://styles/mapbox/satellite-streets-v12?optimize=true"
      },
      {
        "name": "3D (Day)",
        "url": "mapbox://styles/mapbox/standard",
        "time": "dawn"
      },
      {
        "name": "3D (Night)",
        "url": "mapbox://styles/mapbox/standard",
        "time": "dusk"
      }
    ]
  },
  "icons": {
    // Mappings for the icon name that will be called in code and its corresponding url
    "info": "/images/defaults/icons/info.svg"
  }
}
```

#### 1.2.2 Map Data Settings

The `config/data-settings.json` file specifies the datasets for visualisation according to user requirements. This file can ingest both local and remote datasets. Local datasets must start with a `/` to indicate a relative path from the `<root>` directory. Moreover, the display order of datasets will follow same sequence as specified in this file. A sample with explanation is provided below:

```json
{
  "dataSets": [
    "/config/data.json", // Full url of local dataset is `<root>/uploads/config/data.json`
    "https://example.org/data.json" // Remote dataset
  ]
}
```

Datasets must adhere to a specific format defined in the `config/data.json` file. Specifically, it is expected to specify the data sources (where and how the data is loaded) and data layers (how that data is visualised) within a hierarchal data group structure. Note that these datasets do not necessarily need to be named as `data.json` and can be modified as long as they are properly set in the `config/data-settings.json` file. It is also recommended to put them into the `config` directory to minimise any confusion for setting the relative path.

##### Dataset: Defining a group

The `data.json` requires at least one defined data group. Each data group contains a number of parameters (detailed below), and can house multiple sub-groups to form a custom hierarchy. For initial visibility settings, we recommend implementing the `expanded` parameter instead of using any native layer visibility options, such as Mapbox's `"layout": { "visibility": "none" }`, to prevent unexpected behaviors. We recommend to implement this parameter for the nested group with layers, rather than at the root to improve initial user experience.

- `name` (required): This is the user facing name of the data group.
- `expanded` (optional): A boolean indicating if the starting state of the data group should be expanded. False to collapse the group.
- `tree-icon` (optional): An image that will be displayed on the layer tree.
- `stack` (optional): This is the URL for the stack containing metadata on this group's data. Note that this should be the base URL of the stack (i.e. without "/geoserver"). If missing, dynamic metadata from a remote FeatureInfoAgent cannot be utilised. This parameter can also be set with different values for different subgroups.
- `search` (optional): This is the target resource identifier that will activate the search feature capability to find the requested feature(s). The search feature will depend on the [VisBackendAgent](https://github.com/cambridge-cares/TheWorldAvatar/tree/main/Agents/VisBackendAgent) running on the same stack, which must be deployed. Please define filters at the layers if only a subset of features should be visible by default.
- `sources` (optional): This is an array of objects defining data sources (see below for info on sources).
- `layers` (optional): This is an array of objects defining data layers (see below for info on layers).
- `groups` (optional): This is an array of its data subgroups, which follows the same structure and is used to build the data hierarchy.

Definitions of data sources and layers is optional within a data group so that groups can be added just for hierarchal purposes rather than _having_ to house data in every group.

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
        "name": "My Example Sub Group",
        "expanded": false,
        "stack": "https://my-example-website.com/subgroup/stack",
        "sources": [
            ...
        ],
        "layers": [
            ...
        ],
        "groups": [
            ...
        ]
    ]
}
```

##### Dataset: Defining a source

Each group can contain a number of sources, representing individual data files or endpoints that will be loaded into memory by the chosen mapping library. Sources can also be defined in top-level groups, then utilised by layers further down the hierarchy. For loading local data files, it is recommended to placed them in the `uploads/config/data` directory for a more organised approach. Please create the subdirectory if required.

Definitions of sources vary depending on the chosen mapping provider. Specific parameters used for each mapping library are detailed in later parts of the documentation. Nonetheless, the common parameters are detailed below:

- `id` (required): This is the internal ID of the source. It needs to be unique within the current group, but is not required to be globally unique.
- `type` (required): This is the type of the source. Acceptable values differ depending on the mapping library (see the library specific documentation for details).
- `tiles` (required for `vector` types): A sample format is available below; Do note to replace the contents encapsulated within [] with their respective values; `DOMAIN` is the domain of the website hosting the geoserver layer; `WORKSPACE` is the geoserver workspace name; and `LAYER_NAME` is the name of the geoserver layer for visualisation

```js
http://[DOMAIN]/geoserver/ows?service=WMS&version=1.1.0&request=GetMap&layers=[WORKSPACE]:[LAYER_NAME]&bbox={bbox-epsg-3857}&width=256&height=256&srs=EPSG:3857&format=application/vnd.mapbox-vector-tile
```

Sample Mapbox definitions for the common geojson and vector types are as follows:

```json
[
  {
    "id": "example-mapbox-source",
    "type": "geojson",
    "data": "./my-example-data.geojson"
  },
  {
    "id": "example-mapbox-source",
    "type": "vector",
    "tiles": [
      "http://[DOMAIN]/geoserver/ows?service=WMS&version=1.1.0&request=GetMap&layers=[WORKSPACE]:[LAYER_NAME]&bbox={bbox-epsg-3857}&width=256&height=256&srs=EPSG:3857&format=application/vnd.mapbox-vector-tile"
    ]
  }
]
```

##### Dataset: Defining a layer

Each group can also contain a number of layers, defining how the data is visualised on screen. Layers can utilise sources defined in groups higher in the hierarchy; additionally multiple layers can visualise data from the same source.

As with sources, definitions of layers vary depending on the chosen mapping provider (as they all support different styling options). Specific parameters used for each mapping library are detailed in later parts of the documentation. Nonetheless, the common parameters are detailed below:

- `id` (required): This is the internal ID of the layer. It needs to be unique within the current group, but is not required to be globally unique.
- `name` (required): This is the user facing name of the layer (that will appear in the tree). Multiple layers can use the same name, they'll be combined in a single entry in the tree.
- `source` (required): This is the ID of the source used to populate the layer.
- `order` (optional): A field that defines the layer hierarchy based on their order. Defaults to 0. Both positive and negative numbers are valid.
- `layerTreeIconOverride` (optional): Manually specify an icon to display a layer in the floating panel layer tree, useful when there are multiple layers with the same name.
- `grouping` (optional): A grouping field for displaying only a subset of layers within this group.
- `clickable` (optional): Enables the layer to be clickable. Set to true by default.
- `hovering` (optional): Creates a highlight effect when hovering over the layer's features. This parameter is an array of two numbers indicating the opacity for the highlighted and non-highlighted states respectively.
- `isLive` (optional): If set to true, layer will regularly update and repaint. Useful for live data

```json
{
  "id": "example-mapbox-layer",
  "name": "My Example Data",
  "source": "example-mapbox-source"
}
```

> Displaying subset of layers

Users can choose to display a subset of the layers within the same group. For example, given a building dataset for the entire city, the user may wish to create the following three views. This can be set by creating at least one layer for each view and assign a `grouping` field to each. Then, a dropdown will be generated next to the building data group in the visualisation.

- All buildings - `grouping`: "Default"
- All buildings color coded by their use - `grouping`: "Building Use"
- Buildings containing carparks or not - `grouping`: "Carpark"

Instructions:

1. All related layers must belong to the same data group and be assigned a `grouping` field
2. The dropdown sequence is determined by the order the layers appear in the `data.json`; use the `Default` grouping to ensure that it is the first option

> Mapbox properties

1. Filters: Developers may add a filter for the entire layer by adding [Mapbox Expressions](https://docs.mapbox.com/style-spec/reference/expressions/) to the `filter` key, which is found at the root level of the layer specification. This is especially relevant for layers with searchable parameters and they should display only a subset of feature by default

## 2. Assets

This directory is expected to host any assets (images, svg, icons, gifs) for the client. The root endpoint will be `$HOST/images`. For example, the `loading.gif` is available at `$HOST/images/defaults/loading.gif`.

Do note that users should **NOT** delete any contents within the `defaults` directory, as it hosts required images and icons. The remaining contents on the repository are optional and intended to be part of a tutorial.

## 3. Optional Pages

Developers can insert landing pages alongside other supplementary pages such as about us, glossary, acknowledgements, and attributions into the platform. These optional pages must be included as `Markdown` files in the `uploads/optional-pages` directory.

Do note that the supplementary pages will be inserted as thumbnails and accessed via the landing page. It is crucial to add numbers in the file name of supplementary pages to order the thumbnail display according to your preferences. Otherwise, file names are insignificant if the display order is not of utmost significance. For instance, `01.about.md` and `02.glossary.md` will be always be displayed in this sequence as 01 is smaller than 02.

When linking the images in markdown, do note that any relative path should start from the `/images` path (e.g. `/images/defaults/icons/acknowledgement.svg`).

### 3.1 Fields

The following fields are supported, and must be added to the top of the file before any markdown content:

- `title`: Displays the title on the browser tab. Required
- `slug`: Identifier for the page route. Required
- `description`: Describes the page in the landing page. Required only for non-landing pages
- `thumbnail`: Displays the associated thumbnail image in the landing page. Required only for non-landing pages

### 3.2 Sample

A sample markdown file for the landing page:

```markdown
---
title: CReDo // Customisable
slug: landing // This must always be set to landing for the landing page
---

Insert your content here
```
