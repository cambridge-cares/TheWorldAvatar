# Stack Data Uploader contents

This directory contains the different data contents for the [`stack-data-uploader`](https://github.com/cambridge-cares/TheWorldAvatar/tree/main/Deploy/stacks/dynamic/stack-data-uploader).

## Table of Contents

- [Stack Data Uploader contents](#stack-data-uploader-contents)
  - [1. Source Files](#1-source-files)
  - [2. Configuration File](#2-configuration-file)
    - [2.1 Backend services](#21-backend-services)
      - [2.1.1 Stack Data Uploader](#211-stack-data-uploader)
      - [2.1.2 Feature Info Agent](#212-feature-info-agent)
    - [2.2 Android deployment](#22-android-deployment)
      - [2.2.1 Mapbox token](#221-mapbox-token)
      - [2.2.2 Phone and stack communication](#222-phone-and-stack-communication)
    - [2.3 Web visualization](#23-web-visualization)

## 1. Source Files

Source datasets should be placed in the `amenities` directory, which should correspond to the full path at `stack-data-uploader/inputs/data/amenities`. The base functionality requires the following datasets:

1. `Toilet` dataset - Source Directory: `amenities/toilets`
2. `Wasgau` dataset - Source Directory: `amenities/wasgau`
3. `OntoCityToilets` ontology - Source Directory: `amenities/tbox`
4. `OBDA mappings` for the above - Source File: `amenities/toilet.obda`

Additional datasets required for routing purposes:

1. `OSM` point and polygon datasets - Source Directory: `amenities/point` and `amenities/polygon`
2. `Building` dataset - Uploaded separately
3. `Routing` functions and virtual tables - See routing agent for more information

## 2. Configuration File

As per the [documentation](https://github.com/cambridge-cares/TheWorldAvatar/tree/main/Deploy/stacks/dynamic/stack-data-uploader), the relevant configuration file is as follows. Users are also able to upload icons using the `staticGeoServerData` key.

```json
{
  "name": "pirmasens",
  "database": "postgres",
  "datasetDirectory": "amenities",
  "workspace": "pirmasens",
  "dataSubsets": [
    {
      "type": "vector",
      "skip": false,
      "schema": "public",
      "subdirectory": "wasgau",
      "table": "wasgau",
      "ogr2ogrOptions": {
        "sridIn": "EPSG:4326",
        "inputDatasetOpenOptions": {
          "X_POSSIBLE_NAMES": "lon",
          "Y_POSSIBLE_NAMES": "lat"
        }
      }
    },
    {
      "type": "vector",
      "skip": false,
      "schema": "osm",
      "subdirectory": "toilets",
      "table": "toilets",
      "ogr2ogrOptions": {
        "sridIn": "EPSG:4326",
        "inputDatasetOpenOptions": {
          "X_POSSIBLE_NAMES": "lon",
          "Y_POSSIBLE_NAMES": "lat"
        }
      }
    },
    {
      "type": "tboxcsv",
      "skip": false,
      "subdirectory": "tbox"
    },
    {
      "type": "vector",
      "skip": false,
      "schema": "osm",
      "subdirectory": "polygon",
      "table": "polygons"
    },
    {
      "type": "vector",
      "skip": false,
      "schema": "osm",
      "subdirectory": "point",
      "table": "points",
      "geoServerSettings": {
        "virtualTable": {
          "name": "poi",
          "sql": "@/inputs/data/amenities/sql/toilet.sql",
          "escapeSql": true,
          "geometry": {
            "name": "geometryProperty",
            "type": "Point"
          }
        },
        "defaultStyle": "generic"
      }
    },
    {
      "type": "vector",
      "skip": false,
      "schema": "osm",
      "subdirectory": "point",
      "table": "points_toilet_id",
      "geoServerSettings": {
        "virtualTable": {
          "name": "points",
          "sql": "@/inputs/data/amenities/sql/get_toilet_id.sql",
          "escapeSql": false,
          "geometry": {
            "name": "the_geom",
            "type": "Point",
            "srid": 4326
          },
          "parameters": [
            {
              "name": "lon",
              "defaultValue": "1",
              "regexpValidator": "^[\\d\\.\\+-eE]+$"
            },
            {
              "name": "lat",
              "defaultValue": "10",
              "regexpValidator": "^[\\d\\.\\+-eE]+$"
            }
          ]
        },
        "defaultStyle": "generic"
      }
    },
    {
      "type": "vector",
      "skip": true,
      "table": "polygons_nearby_building_info",
      "geoServerSettings": {
        "virtualTable": {
          "name": "polygons",
          "sql": "@/inputs/data/amenities/sql/get_nearby_building_info.sql",
          "escapeSql": false,
          "geometry": {
            "name": "geometryProperty",
            "type": "Polygon",
            "srid": 4326
          },
          "parameters": [
            {
              "name": "lon",
              "defaultValue": "1",
              "regexpValidator": "^[\\d\\.\\+-eE]+$"
            },
            {
              "name": "lat",
              "defaultValue": "10",
              "regexpValidator": "^[\\d\\.\\+-eE]+$"
            }
          ]
        },
        "defaultStyle": "generic"
      }
    },
    {
      "type": "OSMRouting",
      "skip": false,
      "name": "routing",
      "schema": "public",
      "subdirectory": "routing",
      "sql": "@/inputs/data/amenities/sql/nearest_node.sql",
      "verticesGeoServerSettings": {
        "virtualTable": {
          "name": "routing",
          "sql": "@/inputs/data/amenities/sql/nearest_vertex.sql",
          "escapeSql": false,
          "geometry": {
            "name": "the_geom",
            "type": "Point",
            "srid": 4326
          },
          "parameters": [
            {
              "name": "lon",
              "defaultValue": "1",
              "regexpValidator": "^[\\d\\.\\+-eE]+$"
            },
            {
              "name": "lat",
              "defaultValue": "10",
              "regexpValidator": "^[\\d\\.\\+-eE]+$"
            }
          ]
        }
      },
      "waysGeoServerSettings": {
        "virtualTable": {
          "name": "routing",
          "sql": "@/inputs/data/amenities/sql/find_poi.sql",
          "escapeSql": false,
          "geometry": {
            "name": "the_geom",
            "type": "Point",
            "srid": 4326
          },
          "parameters": [
            {
              "name": "lon",
              "defaultValue": "1",
              "regexpValidator": "^[\\d\\.\\+-eE]+$"
            },
            {
              "name": "lat",
              "defaultValue": "10",
              "regexpValidator": "^[\\d\\.\\+-eE]+$"
            }
          ]
        }
      },
      "poiGeoServerSettings": {
        "virtualTable": {
          "name": "routing",
          "sql": "@/inputs/data/amenities/sql/shortest_paths.sql",
          "escapeSql": false,
          "geometry": {
            "name": "the_geom",
            "type": "MultiLineString",
            "srid": 4326
          },
          "parameters": [
            {
              "name": "source",
              "defaultValue": "1",
              "regexpValidator": "^[\\d]+$"
            },
            {
              "name": "target",
              "defaultValue": "10",
              "regexpValidator": "^[\\d]+$"
            }
          ]
        }
      }
    }
  ],
  "mappings": ["toilet.obda"]
}
```
