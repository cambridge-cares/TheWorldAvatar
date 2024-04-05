# Customisation of Platform Content

Given that the platform is designed to be generalisable, this directory exists to allow users to customise their web contents according to their needs. It acts as the target for a Docker volume or bind mount, and should be mounted to the `/twa/public` directory within the deployed container. Files within it can then be accessed using the `$HOST/...` URL route.

The uploaded content provided by the deploying developer should match the directory structure below. Please read the respective sections for the specific instructions.

- [`config/`](#1-configuration): Contains config/settings files.
- [`images/`](#2-assets): Custom image files.
- [`optional-pages/`](#3-optional-pages): Markdown files for optional static content (with metadata from [gray-matter](https://www.npmjs.com/package/gray-matter)).
- [`style-overrides.css`](#4-styling-overrides): Optional CSS overrides.

## 1. Configuration

The platform requires the following [JSON](https://en.wikipedia.org/wiki/JSON) configuration files:

- [`ui-settings.json`](#11-ui-settings): UI configuration settings; **[MANDATORY]**.
- [`data.json`](#121-general-settings): Specifies data sources and layers to be mapped; **[OPTIONAL]**
- [`map-settings.json`](#122-map-data-settings): Non-data specific configuration for maps; **[OPTIONAL]**

### 1.1 UI Settings

The `config/ui-settings.json` file provides general settings for the platform. This includes settings for displaying modules and branding requirements. Below is an example of the contents for a valid `ui-settings.json` file with additional comments explaining each entry. The format of the file should be consistent whether implementing mapbox or cesium maps.

> [!NOTE]  
> The comments seen below are for explanation purposes only, they are not valid JSON. If wishing to use this content in production, remove the comments first.

```json
{
  "branding": {
    "logo": "/images/whatever.svg", // Custom branding logo
    "toolbarLogo": "/images/defaults/toolbar-logo.svg" // Custom logo for the toolbar (should be 5:1 aspect ratio)
  },
  "modules": {
    "landing": true, // Should the landing page be enabled
    "help": true, // Should the help page be enabled
    "map": true, // Should the map page be enabled
    "dashboard": true // Should the dashboard page be enabled
  }
}
```

### 1.2 Map Settings

If the map module is enabled, developers will need to supply `data.json` and `map-settings.json` files.

#### 1.2.1 General Settings

The `config/map-settings.json` file provides general map settings that are not specific to the data sets being loaded. The specific settings are:

- `type` : The map type. Either "mapbox" or "cesium" is currently accepted
- `camera` : Camera starting position and available positions
- `imagery` : Imagery options
- `icons` : A key-value map of the icon name and its corresponding url

Please note that Cesium has not been incorporated at the moment. It is intended that this file (along with other configuration files) are provided by the deploying developer via Docker volumes created with the standard TWA Stack. As such, there may be no off-the-shelf example file committed to this repository.

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
  "icons": { // Mappings for the icon name that will be called in code and its corresponding url
    "info": "/images/defaults/icons/info.svg"
  }
}
```

#### 1.2.2 Map Data Settings

The `config/data.json` file visualises data according to user requirements. Specifically, it is expected to specify the data sources (where and how the data is loaded) and data layers (how that data is visualised) within a hierarchal data group structure.

##### Defining a group

The `data.json` requires at least one defined data group. Each data group contains a number of parameters (detailed below), and can house multiple sub-groups to form a custom hierarchy.

- `name` (required): This is the user facing name of the data group.
- `stack` (optional): This is the URL for the stack containing metadata on this group's data. Note that this should be the base URL of the stack (i.e. without "/geoserver"). If missing, dynamic metadata from a remote FeatureInfoAgent cannot be utilised. This parameter can also be set with different values for different subgroups.
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

##### Defining a source

Each group can contain a number of sources, representing individual data files or endpoints that will be loaded into memory by the chosen mapping library. Sources can also be defined in top-level groups, then utilised by layers further down the hierarchy. For loading local data files, it is recommended to placed them in the `uploads/config/data` directory for a more organised approach. Please create the subdirectory if required.

Definitions of sources vary depending on the chosen mapping provider. Specific parameters used for each mapping library are detailed in later parts of the documentation. Nonetheless, the common parameters are detailed below:

- `id` (required): This is the internal ID of the source. It needs to be unique within the current group, but is not required to be globally unique.
- `type` (required): This is the type of the source. Acceptable values differ depending on the mapping library (see the library specific documentation for details).

A sample definition for Mapbox is as follows:
```json
{
  "id": "example-mapbox-source",
  "type": "geojson",
  "data": "./my-example-data.geojson"
}
```

##### Defining a layer

Each group can also contain a number of layers, defining how the data is visualised on screen. Layers can utilise sources defined in groups higher in the hierarchy; additionally multiple layers can visualise data from the same source.

As with sources, definitions of layers vary depending on the chosen mapping provider (as they all support different styling options). Specific parameters used for each mapping library are detailed in later parts of the documentation. Nonetheless, the common parameters are detailed below:

- `id` (required): This is the internal ID of the layer. It needs to be unique within the current group, but is not required to be globally unique.
- `name` (required): This is the user facing name of the layer (that will appear in the tree). Multiple layers can use the same name, they'll be combined in a single entry in the tree.
- `source` (required): This is the ID of the source used to populate the layer.

```json
{
  "id": "example-mapbox-layer",
  "name": "My Example Data",
  "source": "example-mapbox-source"
}
```

## 2. Assets

This directory is expected to host any assets (images, svg, icons, gifs) for the client. The root endpoint will be `$HOST/images`. For example, the `loading.gif` is available at `$HOST/images/defaults/loading.gif`.

Do note that users should **NOT** delete any contents within the `defaults` directory, as it hosts required images and icons. The remaining contents on the repository are optional and intended to be part of a tutorial.

## 3. Optional Pages

Developers can insert landing pages alongside other supplementary pages such as about us, glossary, acknowledgements, and attributions into the platform. These optional pages must be included as `Markdown` files in the `uploads/optional-pages` directory. 

Do note that the supplementary pages will be inserted as thumbnails and accessed via the landing page. It is crucial to add numbers in the file name of supplementary pages to order the thumbnail display according to your preferences. Otherwise, file names are insignificant if the display order is not of utmost significance. For instance, `01.about.md` and `02.glossary.md` will be always be displayed in this sequence as 01 is smaller than 02.

### 3.1 Fields
The following fields are supported, and must be added to the top of the file before any markdown content:

* `title`: Displays the title on the browser tab. Required
* `slug`: Identifier for the page route. Required
* `description`: Describes the page in the landing page. Required only for non-landing pages
* `thumbnail`: Displays the associated thumbnail image in the landing page. Required only for non-landing pages

### 3.2 Sample
A sample markdown file for the landing page:
```
---
title: CReDo // Customisable
slug: landing // This must always be set to landing for the landing page
---

Insert your content here
```

## 4. Styling Overrides

Users can override existing styling to suit their requirements by adding a `style-overrides.css` to the `uploads` directory.
