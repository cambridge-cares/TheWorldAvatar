# The Stack Data Uploader

<img align="right" width="250" height="500" src="./img/twa-uploader-logo-padded.svg">


> :memo: **Note:** In the commands and file snippets below placeholders are indicated using angled brackets, for example `<STACK NAME>`.
You will need to substitute in appropriate values before running any commands.

> :memo: **Note:** Unless otherwise stated all paths listed in this readme are relative to the [`Deploy/stacks/dynamic/stack-data-uploader`](.) directory in the TheWorldAvatar repository.

## Table of Contents
1. [Table of Contents](#table-of-contents)
2. [Introduction](#introduction)
3. [Running the Stack Data Uploader](#running-the-stack-data-uploader)
4. [Datasets and Subsets](#datasets-and-subsets)
5. [The Dataset Configuration File](#the-dataset-configuration-file)
6. [Data Types](#data-types)
7. [OBDA Mapping file](#obda-mapping-file)
8. [Using Specific Data Sets](#using-specific-data-sets)
9. [Value by File Name](#value-by-file-name)
10. [Processing Data Without Upload](#processing-without-upload)
11. [Debugging the Stack Data Uploader in VSCode](#debugging-the-stack-data-uploader-in-vscode)
12. [Developing the Stack Data Uploader in VSCode](#developing-the-stack-data-uploader-in-vscode)
13. [Troubleshooting](#troubleshooting)

## Introduction

The Stack Data Uploader is designed to make it easier to ingest static data files into a stack.

## Running the Stack Data Uploader

#### 0. Prerequisites
These are the same as listed in [The Stack Manager](../stack-manager/README.md#prerequisites).

#### 1. Initialise the stack
You should initialise the stack by following the instructions in [Spinning up a Stack](../stack-manager/README.md#spinning-up-a-stack).

#### 2. Check the examples
Example dataset files can be found in the [`examples/datasets`](../examples/datasets/) directory.
Descriptions of each example can be found in the [Example datasets](#example-datasets) section.

#### 3. Copy in data files

The source files need to be copied into the [`inputs/data/`](./inputs/data/) directory.
The structure of this directory is described in the [Datasets and subsets](#datasets-and-subsets) section. All data must be in a subdirectory two levels below `data` folder.

#### 4. Create a configuration file
Create a JSON file in the [`inputs/config/`](./inputs/config/) directory to define how the data is to be uploaded.
The structure of this file is described in the [The Dataset configuration file](#the-dataset-configuration-file) section.

#### 5. Running the data uploader

From a terminal in the [`stack-data-uploader`](.) directory, start the `stack-data-uploader` container by running the following:
```console
./stack.sh start <STACK NAME>
```

## Datasets and Subsets

Data files are grouped into *datasets*, each of which has its own configuration file and data directory.

By default all dataset configuration files in the [`inputs/configs/`](./inputs/config/) are read by the data uploader.
When a dataset's name matches with that of the stack then only that configuration file and its *external datasets* will be loaded.

Below is an example where there are two datasets.
One of which (*dataset1*) contains one data subset and another (*dataset2*) that contains two data subsets, each with their own subdirectory. Note that every data file exists in a subdirectory, even if there are no sibling data on the same level. Data will not be uploaded unless it is two levels below `data` in a subdirectory.
```sh
inputs/
  config/               # Directory in which the dataset configuration files should be stored
    dataset1.json       # Configuration file for dataset1
    dataset2.json       # Configuration file for dataset2
  data/                 # Directory in which the data files should be stored
    dataset1/           # Data directory for dataset1
      datasubset1/     # Data subdirectory for data subset1
        data.csv        # Data file for dataset1
    dataset2/           # Data directory for dataset2
      datasubset2/      # Data subdirectory for data subset2
        polygon.geojson # Data file for dataset2
      datasubset2/      # Data directory for data subset2
        table.csv
```

### Example Datasets

There are several example configuration files in the [`examples/datasets`](../examples/datasets/) directory.
You can follow the instructions in the [README.md](../examples/datasets/README.md) to load in one of the example datasets.
The following table provides a description of each example:

| Example                                                                          | Description                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                             |
| -------------------------------------------------------------------------------- | ------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------- |
| [building-bavaria](../examples/datasets/inputs/config/building-bavaria.json)     | Uploads a [set of CityGML files](../examples/datasets/inputs/data/buildings/bavaria/README.md) into the Postgres database of the stack. [b3dm files] and [a geoserver layer] are generated automatically with default settings for visualisation. There is also a OBDA mapping file ([citydbOntop.obda](../examples/datasets/inputs/data/buildings/citydbOntop.obda)), which provides an example of how to make the uploaded data queryable through the Ontop SPARQL endpoint.                                                                                                                                                                                                                                                                          |
| [building-cambridge](../examples/datasets/inputs/config/building-cambridge.json) | Uploads a [set of GDB folders](../examples/datasets/inputs/data/building-cambridge/vector/README.md) and a [CSV file](../examples/datasets/inputs/data/building-cambridge/tabular/README.md) into the Postgres database of the stack. [b3dm files] and [a geoserver layer] are generated automatically with default settings for visualisation. There is also a OBDA mapping file ([ontop_v2.obda](../examples/datasets/inputs/data/building-cambridge/ontop_v2.obda)), which provides an example of how to make the uploaded data queryable through the Ontop SPARQL endpoint.                                                                                                                                                                         |
| [building-hongkong](../examples/datasets/inputs/config/building-hongkong.json)   | Uploads a [GeoJSON file](../examples/datasets/inputs/data/buildings/hongkong/README.md) into the Postgres database of the stack. [b3dm files] and [a geoserver layer] are generated automatically with default settings for visualisation.                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                              |
| [cropmap-reduced](../examples/datasets/inputs/config/cropmap-reduced.json)       | Uploads a [set of Shapefiles](../examples/datasets/inputs/data/cropmap/vector/README.md) into the stack as single vector layer, which is served using the default style by GeoServer. This is a reduced styling ([cropmap-reduced.sld](../examples/datasets/inputs/config/cropmap-reduced.sld)) of the cropmap to make it clearer to view with pylon data.                                                                                                                                                                                                                                                                                                                                                                                              |
| [cropmap-simple](../examples/datasets/inputs/config/cropmap-simple.json)         | Uploads a [set of Shapefiles](../examples/datasets/inputs/data/cropmap/vector/README.md) into the stack as single vector layer, which is served using the default style by GeoServer.                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                   |
| [cropmap](../examples/datasets/inputs/config/cropmap.json)                       | Uploads a [set of Shapefiles](../examples/datasets/inputs/data/cropmap/vector/README.md) into the stack as single vector layer along with several [.csv files](../examples/datasets/inputs/data/cropmap/tabular/) that contain auxiliary data. Some of the auxiliary data is then used by a custom style ([cropmap.sld](../examples/datasets/inputs/config/cropmap.sld)) to dynamically colour the polygons when served through GeoServer. There is also a OBDA mapping file ([ontop_with_comments.obda](../examples/datasets/inputs/data/cropmap/ontop_with_comments.obda)), which provides an example of how to make the uploaded data queryable through the Ontop SPARQL endpoint. Uses [reference to file name](#value-by-file-name) for SQL query. |
| [elevation](../examples/datasets/inputs/config/elevation.json)                   | Uploads a set of [GeoTiff files](../examples/datasets/inputs/data/elevation/README.md) into the stack as a single raster layer, which is served using the custom [`elevation`](../examples/datasets/inputs/config/elevation.sld) style via GeoServer.                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                   |
| [forestry](../examples/datasets/inputs/config/forestry.json)                     | Uploads [a ShapeFile](../examples/datasets/inputs/data/forestry/vector/README.md) into the stack as a vector layer, along with a [.csv file](../examples/datasets/inputs/data/forestry/tabular/forestry_colours.csv) that defines a colour for each category. The layer is served using the colour mapping and a custom style ([forestry.sld](../examples/datasets/inputs/config/forestry.sld)) through GeoServer.                                                                                                                                                                                                                                                                                                                                      |
| [forestry-reduced](../examples/datasets/inputs/config/forestry-reduced.json)     | Uploads a [set of Shapefiles](../examples/datasets/inputs/data/forestry/vector/README.md) into the stack as single vector layer, which is served using the default style by GeoServer. This is a reduced styling ([forestry-reduced.sld](../examples/datasets/inputs/config/forestry-reduced.sld)) of the forestry data to make it clearer to view with pylon data.                                                                                                                                                                                                                                                                                                                                                                                     |
| [ng-pylons](../examples/datasets/inputs/config/ng-pylons.json)                   | Uploads a [set of Shapefiles](../examples/datasets/inputs/data/ng_pylons/vector/) into the stack as multiple vector layers along a [.csv file](../examples/datasets/inputs/data/ng_pylons/tabular/ng_styling.csv) that contain auxiliary data. Some of the auxiliary data is then used by custom styles ([overhead-lines.sld](../examples/datasets/inputs/config/overhead-lines.sld) and [underground-cables.sld](../examples/datasets/inputs/config/underground-cables.sld)) to dynamically style the lines, towers, and underground cables when served through GeoServer.                                                                                                                                                                             |
| [population](../examples/datasets/inputs/config/population.json)                 | Uploads [a GeoTiff file](../examples/datasets/inputs/data/population/population/README.md) into the stack as a raster layer, which is served using the default style via GeoServer.                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                     |
| [pylons](../examples/datasets/inputs/config/pylons.json)                         | An example of how to use the `"externalDatasets"` node to load multiple datasets by name.                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                               |
| [pylons-and-veg](../examples/datasets/inputs/config/pylons-and-veg.json)         | An example of how to use the `"externalDatasets"` node to load multiple datasets by name of another config referencing other `"externalDatasets".                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                       |
| [treesAndHills](../examples/datasets/inputs/config/treesAndHills.json)           | An example of how to use the `"externalDatasets"` node to load multiple datasets by name.                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                               |
| [ukpn-pylons](../examples/datasets/inputs/config/ukpn-pylons.json)               | Uploads a [set of Shapefiles](../examples/datasets/inputs/data/ukpn_pylons/vector/) into the stack as multiple vector layers along with a [.csv file](../examples/datasets/inputs/data/ukpn_pylons/tabular/ukpn_styling.csv) that contain auxiliary data. Some of the auxiliary data is then used by a custom style ([overhead-lines.sld](../examples/datasets/inputs/config/overhead-lines.sld) to dynamically style the lines and towers when served through GeoServer. There is also a OBDA mapping file ([ukpn_ontop.obda](../examples/datasets/inputs/data/ukpn_pylons/ukpn_ontop.obda)), which provides an example of how to make the uploaded data queryable through the Ontop SPARQL endpoint.                                                  |
| [rdf](../examples/datasets/inputs/config/rdf.json)                               | A wrapper around a collection of examples of loading in [RDF](#rdf-data) data. This includes triples [with](../examples/datasets/inputs/config/triples_with_inference.json) and [without](../examples/datasets/inputs/config/triples.json) inference; [quads](../examples/datasets/inputs/config/quads.json); and [using a properties file](../examples/datasets/inputs/config/triples_using_properties_file.json).                                                                                                                                                                                                                                                                                                                                     |
| [tboxcsv](../examples/datasets/inputs/config/tboxcsv.json)                       | An example of loading a TBox from a CSV file into a custom Blazegraph namespace.                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                        |

## The Dataset Configuration File

Each dataset should have its own JSON configuration file located in the [`inputs/config/`](./inputs/config) directory.
The following table shows the top level nodes allowed in a configuration file.

| Key                                             | Required?                                             | Default value                            | Description                                                                                                                                        |
| ----------------------------------------------- | ----------------------------------------------------- | ---------------------------------------- | -------------------------------------------------------------------------------------------------------------------------------------------------- |
| [`"name"`](#name)                               | No                                                    | The filename without the .json extension | The name of the dataset                                                                                                                            |
| [`"datasetDirectory"`](#datasetDirectory)       | No                                                    | The dataset's name                       | The directory within `inputs/data/` that contains the data files associated with this dataset                                                      |
| [`"skip"`](#skip)                               | No                                                    | `false`                                  | If set to `true` this dataset will be ignored by the data uploader                                                                                 |
| [`"database"`](#database)                       | No                                                    | `"postgres"`                             | The name of the database within Postgres to which appropriate data will be uploaded                                                                |
| [`"workspace"`](#workspace)                     | No                                                    | The dataset's name                       | The GeoServer workspace into which any 2D geospatial data layers, vector and raster, will be added                                                 |
| [`"namespace"`](#namespace)                     | No                                                    | The dataset's name                       | The Blazegraph namespace into which RDF data will be added. The long syntax can be used to specify properties if the namespace needs to be created |
| [`"externalDatasets"`](#externaldatasets)       | No[*](#*-at-least-one-of-these-needs-to-be-populated) | `[]`                                     | A list of other datasets' names. Each listed dataset will also be loaded if this dataset is loaded by name                                         |
| [`"dataSubsets"`](#dataSubsets)                 | No[*](#*-at-least-one-of-these-needs-to-be-populated) | `[]`                                     | A list of *data subset* objects                                                                                                                    |
| [`"styles"`](#styles)                           | No[*](#*-at-least-one-of-these-needs-to-be-populated) | `[]`                                     | A list of GeoServer style file definition objects                                                                                                  |
| [`"mappings"`](#mappings)                       | No[*](#*-at-least-one-of-these-needs-to-be-populated) | `[]`                                     | A list of Ontop mapping file definition objects                                                                                                    |
| [`"staticGeoServerData"`](#staticGeoServerData) | No                                                    | `null`                                   | An object describing static data to be served by GeoServer                                                                                         |

##### * At least one of these needs to be populated.

#### `"name"`
This is the name of the dataset.
If left unspecified it is set to the name of the configuration file, without the .json extension.
This is the value that should be specified in the `"externalDatasets"` node of another dataset.
> :memo: **Note:** If the dataset's name matches the name of the stack it is being loaded into then only that dataset and its associated external datasets will be loaded. More information on that in [Using Specific Data Sets](#using-specific-data-sets).

#### `"datasetDirectory"`
The directory within `inputs/data/` that contains the data files associated with this dataset.

#### `"skip"`
Setting the `"skip"` value of a dataset to `true` will cause the data uploader to not load any of the data or files listed in that dataset.

#### `"database"`
The name of the database within Postgres to which appropriate data will be uploaded.
> :memo: **Note:** The database will be created if it does not already exist.

> :warning: **Warning:** Ontop can only access the default 'postgres' database so it is usually best not to change this value.

#### `"workspace"`
The GeoServer workspace into which any 2D geospatial data layers, vector and raster, will be added.
> :memo: **Note:** The workspace will be created if it does not already exist.

#### `"namespace"`
The Blazegraph namespace into which RDF data will be added.

> :memo: **Note:** The namespace will be created if it does not already exist.
##### short syntax
Just the name of the namespace.
This is the recommend form when referring to an exiting namespace.
For example:
```json
"namespace": "kb"
```
##### long syntax
This syntax should be used when creating a new namespace.
It allows the specification of properties that will be used when creating the Blazegraph namespace.
The properties can be specified in a .properties file and/or directly in the .json config file, the values in the config file take precedence.
For example, to load the properties from a file called `myquads.properties` that is stored in the `/inputs/config` directory:
```json
"namespace": {
  "name": "quads",
  "propertiesFile": "myquads.properties"
}
```
To specify properties directly under a ```"properties"``` key, for example:
```json
"namespace": {
  "name": "quads",
  "properties": {
    "com.bigdata.journal.AbstractJournal.bufferMode": "DiskRW",
    "com.bigdata.rdf.store.AbstractTripleStore.quadsMode": true
  }
}
```

Information about the different properties is distributed throughout the Blazegraph code repository.
Some basic configurations are mentioned in the Blazegraph wiki in the[Configuring_Blazegraph](https://github.com/blazegraph/database/wiki/Configuring_Blazegraph) and [InferenceAndTruthMaintenance](https://github.com/blazegraph/database/wiki/InferenceAndTruthMaintenance#configuring-inference) sections.

The most comprehensive documentation of the available properties is located in the Javadocs/comments for the various `Options` interfaces.
Most (if not all) of the `Options` classes can be found by looking at the list of interfaces that the [BigdataSail.Options](https://blazegraph.com/database/apidocs/com/bigdata/rdf/sail/BigdataSail.Options.html) interface extends (Superinterfaces).
The source code for the most commonly used options can be found in in the following files: [AbstractTripleStore Options](https://github.com/blazegraph/database/blob/6b0c935523f5064b80279b30a5175a858cddd2a1/bigdata-core/bigdata-rdf/src/java/com/bigdata/rdf/store/AbstractTripleStore.java#L524), [BigdataSail Options](https://github.com/blazegraph/database/blob/3127706f0b6504838daae226b9158840d2df1744/bigdata-core/bigdata-sails/src/java/com/bigdata/rdf/sail/BigdataSail.java#L270) and [Journal Options](https://github.com/blazegraph/database/blob/3127706f0b6504838daae226b9158840d2df1744/bigdata-core/bigdata/src/java/com/bigdata/journal/Options.java#L85).

It is also possible to create namespaces, with a small selection of options, through the Blazegraph web interface and then inspect the properties.
Be aware though that some of the property keys contain the namespace's name so can not just be copied for a different namespace.

> :warning: **Warning:** The properties will be ignored if the namespace already exists.

#### `"externalDatasets"`
Any datasets that are named under this node will be included if this dataset is loaded by name, either because the stack has the same name or because it appears in the `"externalDatasets"` list of another dataset that is loaded by name.

#### `"dataSubsets"`
This node should contain a list of data subset objects.
Each dataset should generally contain at least one data subset.
If the files in a dataset are of multiple different types, or represent different geospatial layers, they should be divided into multiple data subsets, with one for each type/layer.
Each data subset should then have its own subdirectory.
These specify how to load the data from a particular set of files.
Each data subset must have the following values specified:

| Key                               | Required? | Default value                             | Description                                                                              |
| --------------------------------- | --------- | ----------------------------------------- | ---------------------------------------------------------------------------------------- |
| [`"name"`](#name)                 | No        | Name of layer in source                   | The name of the data subset                                                              |
| [`"type"`](#type)                 | Yes       | N/A                                       | The type of the data                                                                     |
| [`"subdirectory"`](#subdirectory) | No        | [`"datasetDirectory"`](#datasetDirectory) | The subdirectory within the dataset directory that contains the data in this data subset |
| `"skip"`                          | No        | `false`                                   | If set to `true` this data subset will be ignored by the data uploader                   |

##### `"type"`
This controls which functions are used to load the data.
More information about the different data types can be found [here](#data-types).

##### `"subdirectory"`
If there are multiple data subsets then each one must have a separate subdirectory set.
If there is only one data subset, this still have to be set, otherwise no upload will be performed.

#### `"styles"`
A list of GeoServer style file definition objects.
The styles defined here will be loaded into the GeoServer workspace associated with the dataset.
Each entry requires the following values to be specified:

| Key      | Description                                                                                                |
| -------- | ---------------------------------------------------------------------------------------------------------- |
| `"name"` | The name of the style, it will need to be prefixed by the dataset's workspace when referenced in GeoServer |
| `"file"` | The style file's path, relative to the dataset directory                                                   |

Currently only `.sld` style files are supported.
Worked examples of different SLD styles can be found in the [GeoServer manual][sld-cookbook].
If required in the future support for other style formats might be add as GeoServer does support several other formats natively and a few more if the required plugins are loaded.

An example of a style specification in the configuration file is:
```json
"styles": [
    {
        "name": "elevation",
        "file": "elevation.sld"
    }
]
```

#### `"mappings"`
A list of Ontop mapping file definition objects provided as paths relative to the [`"datasetDirectory"`](#datasetDirectory).
Currently only the Ontop native format (`.obda`) is supported as it is much easier for both humans and Ontop to work with.
Ontop also supports the R2RML (`.ttl`) OBDA file standard but the data uploader would need changes to include matching support.

The OBDA file for the cropmap example ([ontop_with_comments.obda](../examples/datasets/inputs/data/cropmap/ontop_with_comments.obda)) shows the Ontop OBDA format.
The Ontop OBDA file format is also described in detail in the [OBDA mapping file](#obda-mapping-file) section.

#### `"staticGeoServerData"`
A description of static data to be loaded into and served by GeoServer.
These are served with the base directory `GEOSERVER_URL/www/icons`.
The icons can be found at `GEOSERVER_URL/www/icons` and the "other files" (being any regular files or folders) can be found at `GEOSERVER_URL/www/static_data`.

| Key            | Description                                                                                                                                                                                                                   |
| -------------- | ----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------- |
| `"iconsDir"`   | Directory relative to the [`"datasetDirectory"`](#datasetDirectory) where icons files can be found                                                                                                                            |
| `"otherFiles"` | A list of "other files" with the `source` relative to the [`"datasetDirectory"`](#datasetDirectory) and the `target` the location relative to `GEOSERVER_URL/www/icons` from which would would like this data is to be served |

```json
"staticGeoServerData": {
  "iconsDir": "icons",
  "otherFiles": [
    {
      "source": "my_additional_data/index.html",
      "target": "additional_data"
    }
  ]
}
```

Note: If you want to reference icons uploaded in this way from a GeoServer `.sld` file, you will need to use stack-internal URLs, e.g.:
```
<OnlineResource xlink:type="simple" xlink:href="http://localhost:8080/geoserver/www/icons/myicon.png" />
```
Relative file-system paths containing `..` are not supported.

> :warning: **Warning:** If you upload any static data in this way, you _must_ make sure that your `dataSubsets` are stored within their own `subdirectory`, even if you have only a single dataset. Otherwise, the data uploader will consider your static files as part of the dataset and not only upload them to GeoServer, but also attempt to upload them to PostGIS etc. and either fail or create unnecessary duplicate uploads.

## Data Types

The following data types are supported: [`vector`](#vector-data), [`citygml`](#citydb-data), [`xbuilding`](#x-building-data), [`raster`](#raster-data), [`tabular`](#tabular-data), [`rdf`](#rdf-data), [`tboxcsv`](#tbox-csv-data), and [`osmrouting`](#osm-data).
A description of how each is processed and a summary of the available configuration options are provided below.

### Vector Data

The `"vector"` data type should be used to load 2D point, line or polygon geospatial data.
The data loader does two things when uploading vector data:
1. It uses the GDAL [`ogr2ogr`][ogr2ogr] tool to read in data from a wide variety of file formats and output it to the PostgreSQL database in the stack.
The full list of file formats that `ogr2ogr` supports is given [here][vector-drivers] although some of these might not be available depending on the exact GDAL Docker image being used, see [here][gdal-docker] for details.
2. It uses the GeoServer REST API to create a new layer in GeoServer that can be used to visualise the newly uploaded geometries.

The options for these two processes are set using the following json objects within the respective data subset object in the dataset configuration file:

#### GDAL Options

In most situations the default `ogr2ogr` settings will be sufficient to upload the data but sometimes some extra options need to be supplied. [:open_file_folder:](#value-by-file-name)
These can be specified within an `"ogr2ogrOptions"` object under the following keys:

##### `"sridIn"`
If the input dataset does not have an SRID/CRS/SRS specified then it can be specified as the value for the `"sridIn"` key.
When specifying an EPSG code for the SRS it needs to include the authority as well as the ID, for example `"EPSG:4296"` rather than just `4296` or `"4296"`.
This sets the value of the [`-a_srs`][ogr2ogr-a_srs] argument passed to `ogr2ogr`.

##### `"sridOut"`
If you want to reproject the coordinates the target SRID/CRS/SRS can be set as the value for the `"sridOut"` key.
When specifying an EPSG code for the SRS it needs to include the authority as well as the ID, for example `"EPSG:4296"` rather than just `4296` or `"4296"`.
This sets the value of the [`-t_srs`][ogr2ogr-t_srs] argument passed to `ogr2ogr`.
It also means any value specified for `"sridIn"` is passed as the value of the [`-s_srs`][ogr2ogr-s_srs] argument, rather than `-a_srs`.

##### `"inputDatasetOpenOptions"`
Some data source formats require additional options to be set for the geometries and their metadata to be loaded correctly.
These can be set as key-value pairs within a `"inputDatasetOpenOptions"` object.
These options are format specific and are generally described in a section with the heading "Open options" or "Dataset open options" on the relevant driver documentation page.
All of the vector drivers are listed [here][vector-drivers] with links to their documentation.
The values are passed to the `ogr2ogr` tool as `NAME=VALUE` pair arguments of the [`-oo`][ogr2ogr-oo] option.

##### `"layerCreationOptions"`
All vector datasets are loaded into the PostGIS database within the stack with each data subset being loaded as a separate layer/table.
In general these options should not need to be set explicitly as the `ogr2ogr` tool can usually work them out from the source dataset, or use default values.
However, setting one or more of them may be required to fix specific problems with the input dataset.
The layer creation options provided by the PostGIS driver are described [here][vector-postgis-lco].
The values are passed to the `ogr2ogr` tool as `NAME=VALUE` pair arguments of the [`-lco`][ogr2ogr-lco] option.

##### `"outputDatasetOpenOptions"`
The PostGIS driver has a few options to control how the `ogr2ogr` tool connects to the database.
However, all of the essential information (database name, port number, username, etc.) is set automatically so these settings should only be changed if absolutely necessary.
The dataset open options provided by the PostGIS driver are described [here][vector-postgis-doo].
The values are passed to the `ogr2ogr` tool as `NAME=VALUE` pair arguments of the [`-doo`][ogr2ogr-doo] option.

##### `"otherOptions"`
Several non-driver specific options are also available.
These can be set as key-array-valued pairs within an `"otherOptions"` object.
This allows for multiple values per option (`["value1", "value2"]`) but requires that single values are still placed within an array  (`["value"]`) and valueless flags are paired with an empty array (`[]`).
A list of possible options can be found on the [vector common options][vector-common] and [ogr2ogr options][ogr2ogr] pages.

##### `"envVars"`
A few aspects of some of the drivers can also be set via environment variables.
These can be set as key-value pairs within an `"envVars"` object.

##### Common Drivers
- [Comma Separated Value (.csv)][vector-csv]
- [ESRI Shapefile / DBF][vector-shapefile]
- [PostgreSQL / PostGIS][vector-postgis] (mainly as the output)

#### GeoServer Options
For vector data you can add a `geoServerSettings` node within the relevant data subset in the configuration json.
These settings are generally only required to add dynamic (value-based) styling to the layers for visualisation.
Within that the following nodes can be added.
- `"virtualTable"` creates a [SQL View][geoserver-sql] which is specified as follows:
  - `"name"` a name is required.
  - `"sql"` an SQL query that defines the virtual table is required. [:open_file_folder:](#value-by-file-name)
  - `"keyColumn"` specify column for [parameter][geoserver-sql-params] key.
  - `"escapeSql"` is Boolean `true` or `false`.
    This concerns the handling of special characters in column names such as setting single-quotes to doubled single-quotes.
  - `"geometry"` specifies the geometry with the following `key:value` pairs.
    - `"name"` name of column with the geometry.
    - `"type"` one of `Point`, `LineString`, `LinearRing`, `Polygon`, `MultiPoint`, `MultiLineString`, `MultiPolygon`, `GeometryCollection`.
    - `"srid"` EPSG code as an integer, for example `4296` rather than `"EPSG:4296"` or `"4296"`.
      Note that this is different from the GDAL Options.
  - `"parameters"` specify individual [parameters][geoserver-sql-params] as a list of nodes with the following `key:value` pairs.
    - `"name"` parameter name.
    - `"defaultValue"` default value of parameter.
    - `"regexpValidator"` validation regular expression.
- `"defaultStyle"` name of style within GeoServer that will be the style if of this layer if no other style is specified.

These are the most commonly used options, for more see the examples [here][geoserver-rest] and [here][geoserver-rest-layers].

### CityDB Data

The `"CityDB"` data type should be used to load CityGML and CityJSON data.
The data loader does the following things by default when uploading data:
1. It uses the 3DCityDB Importer [`impexp import`][3dcitydb-importer] tool to read in data from CityGML and CityJSON files and output it to the PostgreSQL database in the stack using the 3DCityDB schema.
The full list of file formats that `impexp import` supports is given [here][3dcitydb-importer-formats].
2. Building footprints and heights are added to the uploaded data if they do not exist.
3. It writes the processed data that has been uploaded to PostgreSQL out to a compressed CityGML file using the 3DCityDB Importer [`impexp import`][3dcitydb-importer] tool.
4. It uses the GeoServer REST API to create a new layer in GeoServer that can be used to visualise the newly uploaded geometries (in Mapbox).
5. It uses the [`py3dtiler`][py3dtiler] tool to create 3DTile sets that can be used to visualise the newly uploaded geometries (in Cesium).

These tilesets are written to folders in a Docker volume and served on the `/3dtiles` path of the stack.
The full URL for a `tileset.json` file, which should be specified in the data.json visualisation file, is `<server base address>/3dtiles/<database name>/<database schema>/<spec>/tileset.json`.
The components of this are as follows:

| Placeholder             | Description                                                                                                                                                                                        |
| ----------------------- | -------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------- |
| `<server base address>` | e.g. `http://localhost:3838`                                                                                                                                                                       |
| `<database name>`       | The name of the database, as specified at the top-level of the dataset config file                                                                                                                 |
| `<database schema>`     | The database schema, this is fixed as `citydb` for now.                                                                                                                                            |
| `<spec>`                | There are currently three hardcoded specs: `lod2-features`, `lod2-buildings` and `lod1_lod2-buildings`. Each one is generated using different options passed to the [`py3dtiler`][py3dtiler] tool. |

The options for these processes are set using the following json objects within the respective data subset object in the dataset configuration file:

#### Import Options

The only option that is required is `"sridIn"`.
Other than `"sridIn"`, in most situations the default `impexp import` settings will be sufficient to upload the data but sometimes some extra options need to be supplied.
Options can be specified within an `"importOptions"` object under the following keys:

##### `"sridIn"` (Required)

The SRID/CRS/SRS of the input dataset does not get picked up automatically so needs to be specified as the value for the `"sridIn"` key.
When specifying an EPSG code for the SRS just the ID is required as a string, for example `"4296"` rather than `4296` or `"EPSG:4296"`.
Because the SRID is set for each database schema Each dataset should write to its own PostgreSQL database and all of the city data in a dataset must use the same SRID.
The 3DCityDB importer doesn't support reprojection but this could be added in the future.

##### `"options"`

An `"options"` node can be added with a map of options to be passed to the `impexp import` command-line interface.
The format is the same as the one used for [ogr2ogr otheroptions](#otheroptions).
The list of avaliable options can be found [here][3dcitydb-importer-cli].
The "Database connection options" are set automatically by the `stack-data-uploader` so can be ignored.

#### `"augmentData"`

This boolean flag controls whether footprint and height will be calculated and added to the existing data, if not already present. It is assumed to be `true` if omitted.

#### `"discoverThematicSurface"`

If this boolean flag is set to `true`, the uploader will attempt to identify untagged surfaces as roof, wall or ground surfaces. It is assumed to be `false`. Note that this is dependent on ["augmentData"](#augmentData).

#### `"createTile"`

This boolean flag controls whether 3D tiles will be generated for uploaded building data. It is assumed to be `true` if omitted.

#### `"parallelTiling"`

Three sets of 3D tiles with different settings will be generated by the tiler. This boolean flag controls whether the tiling process will be run in parallel or not. Running in parallel will be faster but more suspectible to running out of memory. It is assumed to be `true` if omitted. Note that this is dependent on ["createTile"](#createtile).

### X building data

The `"XtoCityDB"` data type should be used to load LoD1 (footprint with height) building data that are not CityGML or CityJSON.
The data loader does the following when uploading data:
1. It uses the GDAL [`ogr2ogr`][ogr2ogr] tool to read in data from a wide variety of file formats and output it to the PostgreSQL database in the stack.
The full list of file formats that `ogr2ogr` supports is given [here][vector-drivers] although some of these might not be available depending on the exact GDAL Docker image being used, see [here][gdal-docker] for details.
2. It uses the 3DCityDB Importer [`impexp import`][3dcitydb-importer] tool to initialise the 3DCityDB schema in the PostgreSQL database in the stack.
3. It uses two SQL scripts to convert the building data uploaded by GDAL and populate the 3DCityDB tables sequentially. The building data will be instantiated as LoD2 buildings with thematic surfaces.
4. Building footprints and heights are added to the uploaded data.
5. It writes the processed data that has been uploaded to PostgreSQL out to a compressed CityGML file using the 3DCityDB Importer [`impexp import`][3dcitydb-importer] tool.
6. It uses the GeoServer REST API to create a new layer in GeoServer that can be used to visualise the newly uploaded geometries (in Mapbox).
7. It uses the [`py3dtiler`][py3dtiler] tool to create 3DTile sets that can be used to visualise the newly uploaded geometries (in Cesium).

Check [`here`](#gdal-options) for details about configuring the GDAL, [`here`](#citydb-data) for details about the 3DTile set and [`here`](#geoserver-options) for details about the Geoserver layer. `"sridIn"` and `"augmentData"` for the 3DCityDB Importer are read.

`"table"` will be the name of the resulting table of the GDAL [`ogr2ogr`][ogr2ogr] tool, whereas `"name"` will be the name of the GeoServer layer.

#### Building data processing with SQL

Two SQL scripts are executed to convert the building data to conform with the 3DCityDB schema. The following information must be contained by (or can be derived from) the table uploaded by GDAL:

1. `"IDname"`, `"IDval"`: an ID for each building
2. `"footprint"`: the footprint of each building
3. `"elevation"`: the ground height of each building
4. `"height"`: the height of each building

A `"columnMap"` can be specified in the input configuration to inform the stack which columns to look for these information. `"IDval"` is the name of the column that contains the building ID, whereas `"IDname"` is the name of the ID that will be stored in the 3DCityDB tables.

The first SQL script creates two tables from the original table: `"raw_building_XtoCityDB"` and `"raw_surface_XtoCityDB"`. It assumes that the original data is uploaded to the public schema. The `"raw_building_XtoCityDB"` table has the following columns:

1. `"IDval"`: the ID of each building from the original data
2. `"gmlid"`: a randomly generated GML ID of each building
3. `"geom"`: the 3D solid geometry of each building, generated by extruding the `"footprint"` with `"height"` and shifting it with `"elevation"`
5. `"mh"`: the height of each building

The `"raw_surface_XtoCityDB"` table has the following columns:

1. `"building_gmlid"`: the GML ID of the parent building of a surface
2. `"gmlid"`: a randomly generated GML ID of each surface of each building
3. `"class"`: an integer indicating whether a surface is a roof, wall or ground
4. `"geom"`: the 3D polygon geometry of each surface

In some cases, the original data require more sophisticated processing. Users can supply a custom SQL script with the `"preprocessSql"` keyword using [File by Value Name](#value-by-file-name). The query must create the `"raw_building_XtoCityDB"` and `"raw_surface_XtoCityDB"` tables in the public schema from the uploaded original data with the column names stated above. In this case, only `"IDname"` and `"IDval"` will take effect and needed to be specified in `"columnMap"`.

The second SQL script populates the 3DcityDB schema with preprocessed building data.

### Raster Data

The `"raster"` data type should be used to load raster/coverage geospatial data.
The data loader does three things when uploading raster data:
1. It uses the GDAL [`gdal_translate`][gdal-translate] tool to read in data from a wide variety of file formats and output it to [Cloud Optimized GeoTIFF (COG)][raster-cog] files stored in the stack.
  This is an extension of the GeoTIFF format and both are very efficient to read.
  The full list of file formats that `gdal_translate` supports is given [here][raster-drivers] although some of these might not be available depending on the exact GDAL Docker image being used, see [here][gdal-docker] for details.
2. It uses the PostGIS [`raster2pgsql`][postgis-raster-loader] tool to register the GeoTIFF files in the PostGIS database.
   The `raster2pgsql` tool also automatically divides the data into tiles in the database to make geospatial searching more efficient.
3. It uses the GeoServer REST API to create a new coverage layer in GeoServer that can be used to visualise the newly uploaded data.

#### GDAL Options

In most situations the default `gdal_translate` settings will be sufficient to upload the data but sometimes some extra options need to be supplied. [:open_file_folder:](#value-by-file-name)
These can be specified within an `"gdalTranslateOptions"` object (previously just called `"options"`) under the following keys:

##### `"sridIn"`
If the input dataset does not have an SRID/CRS/SRS specified then it can be specified as the value for the `"sridIn"` key.
When specifying an EPSG code for the SRS it needs to include the authority as well as the ID, for example `"EPSG:4296"` rather than just `4296` or `"4296"`.
A full explanation of the acceptable SRS formats is given [here][raster-common-t_srs].
This sets the value of the [`-a_srs`][raster-common-a_srs] argument passed to `gdal_translate`.

##### `"sridOut"`
If you want to reproject the coordinates the target SRID/CRS/SRS can be set as the value for the `"sridOut"` key.
When specifying an EPSG code for the SRS it needs to include the authority as well as the ID, for example `"EPSG:4296"` rather than just `4296` or `"4296"`.
A full explanation of the acceptable SRS formats is given [here][raster-common-t_srs].
This sets the value of the [`TARGET_SRS`][gdal-cog-t_srs] creation option passed to `gdal_translate`.
This is an option specific to the [COG][gdal-cog] raster driver when using `gdal_translate`, although we could use `gdalwarp` to handle this more efficiently in the future.

 If gdal does not recognise the EPSG SRID (i.e. `gdalsrsinfo` returns an EPSG:-1), then the projection is assumed to be a custom one and will be appended to the spatial ref system in postGIS and GeoServer. This must still include an authority (although not EPSG) and a new number. The uploader will throw an error if the number exists already in the table.
 
 e.g. if the `sridOut` is set to `TWA:101000` and the projection is not recognised, this will be used as the SRID and authority in the newly specified custom projection.

##### `"inputDatasetOpenOptions"`
Some data source formats require additional options to be set for the geometries and their metadata to be loaded correctly.
These can be set as key-value pairs within a `"inputDatasetOpenOptions"` object.
These options are format specific and are generally described in a section with the heading "Open options" or "Dataset open options" on the relevant driver documentation page.
All of the raster drivers are listed [here][raster-drivers] with links to their documentation.
The values are passed to the `gdal_translate` tool as `NAME=VALUE` pair arguments of the [`-oo`][gdal-translate-oo] option.

##### `"creationOptions"`
All raster datasets are loaded into the PostGIS database within the stack with each data subset being loaded as a separate layer/table.
In general these options should not need to be set explicitly as the `gdal_translate` tool can usually work them out from the source dataset, or use default values.
However, setting one or more of them may be required to fix specific problems with the input dataset.
The creation options provided by the COG driver are described [here][raster-cog-co].
The values are passed to the `gdal_translate` tool as `NAME=VALUE` pair arguments of the [`-co`][gdal-translate-co] option.

##### `"otherOptions"`
Several non-driver specific options are also available.
These can be set as key-array-valued pairs within an `"otherOptions"` object.
This allows for multiple values per option (`["value1", "value2"]`) but requires that single values are still placed within an array  (`["value"]`) and valueless flags are paired with an empty array (`[]`).
A list of possible options can be found on the [raster common options][raster-common] and [gdal_translate options][gdal-translate] pages.

##### `"mdimSettings"`
This is for specific info about multidimensional geospatial files. Consider running `gdalinfo` and `gdalmdiminfo` on your file to get these parameters if they are not known.

- `"layerArrayName"` is the array of the multidimensional file with the required data to iterate over and generate rasters
- `"timeOptions"` contains information for geoserver to parse the time dimension. `"arrayName"` should have the name of the array with times.
  - The `"format"` needs to be set to the [standard format][time-formatting] understood by java and geoserver e.g. `"yyyyMMddHH`".
  Be careful that `m` is minute and `M` is month etc.
  See above link to correctly format your datetime string.
  This can include a timezone offset but will be overridden by `timeZone` parameter.
  - The regex should be used in conjuction with the format to parse the filename. e.g. `"regex": ".*([0-9]{10}).*" will parse 10 digits beside each other anywhere in the filename.`
  - The `timeZone` is passed as a `ZoneId` in java and can be something like `"GMT"` or `"Europe/Paris"`, see more [here][zone-id].

##### Common Drivers
- [GeoTIFF][raster-geotiff]
- [COG – Cloud Optimized GeoTIFF][raster-cog] (mainly as the output)

##### netCDF Files
netCDF files are commonly used in climate science projection data. The uploader can recognise these and will copy them into postgis and subsequently iterate over internal bands to create individual geotiffs for each value in the time series band. It is important to set [mdimSettings](#mdimSettings)

#### GeoServer Options

For raster data you can add a `geoServerSettings` node within the relevant data subset in the configuration json.
Within that the following nodes can be added.
- `"layerSettings"`
  - `"defaultStyle"`: name of style within GeoServer that will be the style if of this layer if no other style is specified.

### Tabular Data

The `"tabular"` data type should be used to load non-geospatial data.
The data loader just does one thing when uploading tabular data:
1. It uses the GDAL [`ogr2ogr`][ogr2ogr] tool to read in data from a wide variety of file formats and output it to the PostgreSQL database in the stack.
As the data is intended to be non-geospatial, this is most useful for reading in data from [comma separated value (.csv)][vector-csv], and Microsoft Excel's [XLS][vector-xls] and [XLSX][vector-xlsx] formatted files.
The full list of file formats that `ogr2ogr` supports is given [here][vector-drivers] although some of these might not be available depending on the exact GDAL Docker image being used, see [here][gdal-docker] for details.

#### GDAL Options

These are the same as listed in the vector [GDAL Options](#gdal-options) although obviously the options specific to geospatial data will not be relevant.

##### Common Drivers
- [Comma Separated Value (.csv)][vector-csv]
- [XLS - MS Excel format][vector-xls]
- [XLSX - MS Office Open XML spreadsheet][vector-xlsx]
- [PostGIS][vector-postgis] (mainly as the output)

### RDF Data

The `"rdf"` data type should be used to load RDF data (triples or quads) from common file formats.
The full list of file formats that are supported is given [here][RSC-uploader].
The data loader does the following when uploading RDF data:
1. It uses the [`RemoteStoreClient::uploadFile`][RSC-uploader] method to read in RDF triple and quad data to the Blazegraph database in the stack.

There are no configurable options for this process, the namespace the data is added to is always the one defined in the parent dataset.

### TBox CSV Data

The `"tboxcsv"` data type should be used to load TBox triples from CSV files that adhere to the TBox Generator format, example files can be found [here][tbox-examples].
The data loader does the following when uploading RDF data:

1. It uses the [`TBoxGeneration::generateTBox`][tbox-generation] method to generate an OWL file from the contents of the CSV file.
2. It uses the [`RemoteStoreClient::uploadFile`][RSC-uploader] method to uploads the contents of the OWL file to the Blazegraph database in the stack.

There are no configurable options for this process, the namespace the data is added to is always the one defined in the parent dataset.

### OSM Data

The `"osmrouting"` data type should be used to load Open Street Map (OSM) files in a form that is compatible with [pgRouting][pgrouting].
These can be `.osm` or `.pbf` files.
By default, three tables and three GeoServer layers are created; `DATA_SUBSET_NAME_ways`, `DATA_SUBSET_NAME_ways_vertices_pgr`, and `DATA_SUBSET_NAME_pointsofinterest`.

#### osm2pgrouting Options

For OSM data you can add a `osm2PGRoutingOptions` node within the relevant data subset in the configuration json.
This can be used to configure the osm2pgrouting tool as specified [here][osm2pgrouting-how-to-use].
- `"flags"`: a list of flags without arguments e.g. `[--attributes, --addnodes]`
- `"options"`: a node containing the key value pairs of options with arguments e.g. `{"--chunk": "40000"}`

#### OSM GeoServer Options

For OSM data you can add the nodes `waysGeoServerSettings`, `verticesGeoServerSettings`, and `poiGeoServerSettings` nodes within the relevant data subset in the configuration json.
The nodes that can be added within each are the same as the GeoServer options for [vector data](#vector-data).

## OBDA Mapping File

The general layout of the file is as follows:

### Comments

Comments are not natively supported in the Ontop OBDA format but the data uploader will strip them out before passing the mappings to Ontop.
Comments are started by a `#` character and can appear at the start of a line, that contains no "code", or at the end of one that does.
When a comment follows "code" the `#` character must be preceded by at least one white-space character.
For example:
``` bash
# Comment at the start of a line, whole line.
SELECT var1 var2 # Comment following some "code"
```

### Prefix Declarations
This is where the RDF prefixes should be defined, these can then be used when specifying triple patterns in the mappings.
It starts with a `[PrefixDeclaration]` tag, followed by the prefix-IRI base pairs, without angled-brackets `<>`.
```obda
[PrefixDeclaration]
rdf:    http://www.w3.org/1999/02/22-rdf-syntax-ns#
ex:     http://example.org/
```

### Mapping Declarations

The mapping declarations section starts with this line:
```[MappingDeclaration] @collection [[```
and is closed by the following line:
```]]```

Each mapping has three parts:
| Label     | Description                                                                                                                                                                                         |
| --------- | --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------- |
| mappingId | The unique name of the mapping                                                                                                                                                                      |
| target    | Template of the [Turtle format][turtle] triple patterns that will exist in Ontop's virtual knowledge graph. Placeholders for values extracted from the PostGIS database are specified as `{value}`. |
| source    | SQL query used to determine the values to replace the placeholders in the *target* block.                                                                                                           |

Ontop effectively takes the result of the SQL `SELECT` query written in the *source* block and for each row creates a set of virtual triples by substituting the SQL variables into the Turtle formatted template in the *target* block. In practice Ontop performs direct mappings, query rewriting, and other optimisations to improve the efficiency of running SPARQL queries over these virtual triples.

A simple example of a mapping is:
```
mappingId   exampleMapping
target      ex:building{id} ex:hasName "{name}"^^xsd:string;
                ex:hasId {id}^^xsd:integer .
source      SELECT id, name
            FROM buildings
```
Here the the PostgreSQL table `buildings` is assumed to contain the columns `id` (containing integers) and `name` (containing strings).
For each row in that table two virtual triples will be created `ex:building{id} ex:hasName "{name}"^^xsd:string` and `ex:building{id} ex:hasId {id}^^xsd:integer`.

For example if the `buildings` table was as follows then the subsequent virtual triples would be created.

| id   | name         | .... |
| ---- | ------------ | ---- |
| 1234 | "building 1" | ...  |
| 1235 | "building 2" | ...  |

`ex:building1234 ex:hasName "building 1"^^xsd:string`<br>
`ex:building1234 ex:hasId 1234^^xsd:integer`<br>
`ex:building1235 ex:hasName "building 2"^^xsd:string`<br>
`ex:building1235 ex:hasId 1235^^xsd:integer`

#### SPARQL Queries on Ontop

Ontop supports a wide range of [SPARQL 1.1](https://ontop-vkg.org/guide/compliance.html#sparql-1-1) and [GeoSPARQL 1.0](https://ontop-vkg.org/guide/compliance.html#geosparql-1-0) features.
The [cropmap](../examples/datasets/inputs/data/cropmap/ontop_with_comments.obda) example OBDA file shows how to use the PostGIS function [`ST_ASTEXT`](https://postgis.net/docs/ST_AsText.html) and the [`http://www.opengis.net/ont/geosparql#wktLiteral`](http://www.opengis.net/ont/geosparql#wktLiteral) to make it possible to run GeoSPARQL queries.

## Using Specific Data Sets

If you do not want the use every config file you can either use `"skip"=true` or name your stack so that the relevant config file is named `<STACK NAME>.json`.
If you want to use a few config files you can create one master config file named `<STACK NAME>.json` with the following.
    ```json
    {
        "name": "<STACK NAME>",
        "externalDatasets": [
            "name of one config file (no .json)",
            "name of another config file",
            <...>
        ]
    }
    ```

## Value by File Name

The stack uploader supports file referencing in the config file on certain values denoted here by :open_file_folder:.
This an be done by giving a value of '@' followed by the name of the file containing the text to be used for that value.
For example one can avoid long SQL queries in their configs by putting them in a file in the [inputs/config](./inputs/config) directory and referencing that file in the following way.
  ```json
    {
      <...>
      "sql": "@/inputs/config/my-sql-query.sql"
      <...>
    }
  ```
Note that this file path is the path inside the container.

## Processing Without Upload

If a subdirectory is not specified in a `dataSubset` node, no data will be uploaded, however for some types of data this is useful to run only the post processing. For example it is possible to run arbitrary sql by specifying sql queries or pointing to `.sql` files and specifying a table. It is also useful to create layers in GeoServer on already uploaded data.

## Debugging the Stack Data Uploader in VSCode

1. In the `Run and Debug` side panel of VSCode run the `Debug (stack-data-uploader)` configuration.

## Developing the Stack Data Uploader in VSCode

You will need permission to push to the CMCL package repository to be able to build the stack-data-uploader project

1. Follow the instructions in step 1. of [Debugging the Stack Data Uploader in VSCode](#debugging-the-stack-data-uploader-in-vscode)

2. Create two files called `repo_username.txt` and `repo_password.txt` in the `stack-data-uploader/docker/credentials` directory.
Populate the files with your GitHub username and access token (with scope to write packages), respectively.

3. In the `Run and Debug` side panel of VSCode run the `Build and Debug (stack-data-uploader)` configuration.

## Troubleshooting

- The best place to start is to look at the container logs.
These can be seen by right clicking on the containers on VSCode or through Docker Desktop.
- It is often helpful to run the uploader after each step after step 3 of [Running the Stack Data Uploader](#Running-the-Stack-Data-Uploader).
This way you can look at look at the user interfaces of the various services (see step 5 of [Spinning up a Stack](../stack-manager/README.md#spinning-up-a-stack)) and be able to see your data sets being populated here.
- When writing SQL queries you can try them in the Adminer first.
- It is recommended that you write and validate GeoServer styles in the GeoServer UI, saving it to a `.sld` file.

### Quick Fixes

#### General
- For certain vector geometries (e.g. `MULTILINESTRING` and `LINESTRING`) it is necessary to use [`-nlt`][ogr2ogr-nlt] to specify the geometry in the following way.
    ```json
    "ogr2ogrOptions": {
        "otherOptions": {
            "-nlt": ["<GEOMETRY TYPE>"]
        }
    }
    ```

- To upload only specific properties/fields from a source dataset their names can be specified as comma-separated values for the [`"-select"`][ogr2ogr-select] option under [`"otherOption"`](#otheroptions).
    In the example below only the fields with the names `field1`, `field3`, `field4` and `field8` would be uploaded with other fields, for example `field2` and `field5`, being ignored.
    ```json
    "ogr2ogrOptions": {
        "otherOptions": {
            "-select": [
                "field1,field3,field4,field8"
            ]
        }
    }
    ```
    The [cropmap](../examples/datasets/inputs/config/cropmap.json) example shows this being used to remove some fields (containing calculated areas and perimeters) that were not constantly named across all of the [crop-map-of-england-crome-2020][crome-2020] Shapefiles.


#### [ESRI File Geodatabase][vector-gdb]

- As described in the GDAL documentation [ESRI File Geodatabase][vector-gdb] datasets must be stored in a directory/folder with a name that ends with the `.gdb` extension.
    For improved efficiency this folder can be added to a zip file with the `.gdb.zip` extension.
    For example:
    ``` sh
    inputs/
      data/
        dataset1/
        datasubset1/
          layer.gdb.zip           # Compressed zip file
          layer.gdb/              # Special .gdb folder containing ESRI File Geodatabase files
            a0000000a.gdbtablx
            a0000000a.gdbtable
            a0000000a.gdbindexes
            a0000000a.freelist
            a0000000a.spx
            ...
    ```

[Turtle]: https://www.w3.org/TR/2014/REC-turtle-20140225/

[RSC-uploader]:   https://github.com/cambridge-cares/TheWorldAvatar/blob/main/JPS_BASE_LIB/src/main/java/uk/ac/cam/cares/jps/base/query/RemoteStoreClient.java#L875
[tbox-generation]: ../../../../JPS_BASE_LIB/src/main/java/uk/ac/cam/cares/jps/base/converter/TBoxGeneration.java#L91
[tbox-examples]:   ../../../../JPS_Ontology/KBTemplates/TBox/

[gdal-docker]:    https://github.com/OSGeo/gdal/tree/master/docker
[ogr2ogr]:        https://gdal.org/programs/ogr2ogr.html#ogr2ogr
[vector-common]:  https://gdal.org/programs/vector_common_options.html#common-options-for-vector-programs
[ogr2ogr-a_srs]:  https://gdal.org/programs/ogr2ogr.html#cmdoption-ogr2ogr-a_srs
[ogr2ogr-s_srs]:  https://gdal.org/programs/ogr2ogr.html#cmdoption-ogr2ogr-s_srs
[ogr2ogr-t_srs]:  https://gdal.org/programs/ogr2ogr.html#cmdoption-ogr2ogr-t_srs
[ogr2ogr-oo]:     https://gdal.org/programs/ogr2ogr.html#cmdoption-ogr2ogr-oo
[ogr2ogr-lco]:    https://gdal.org/programs/ogr2ogr.html#cmdoption-ogr2ogr-lco
[ogr2ogr-doo]:    https://gdal.org/programs/ogr2ogr.html#cmdoption-ogr2ogr-doo
[ogr2ogr-nlt]:    https://gdal.org/programs/ogr2ogr.html#cmdoption-ogr2ogr-nlt
[ogr2ogr-select]: https://gdal.org/programs/ogr2ogr.html#cmdoption-ogr2ogr-select

[vector-drivers]:     https://gdal.org/drivers/vector/index.html#vector-drivers
[vector-shapefile]:   https://gdal.org/drivers/vector/shapefile.html#esri-shapefile-dbf
[vector-csv]:         https://gdal.org/drivers/vector/csv.html#comma-separated-value-csv
[vector-xls]:         https://gdal.org/drivers/vector/xls.html#xls-ms-excel-format
[vector-xlsx]:        https://gdal.org/drivers/vector/xlsx.html#xlsx-ms-office-open-xml-spreadsheet
[vector-postgis]:     https://gdal.org/drivers/vector/pg.html#postgresql-postgis
[vector-postgis-lco]: https://gdal.org/drivers/vector/pg.html#layer-creation-options
[vector-postgis-doo]: https://gdal.org/drivers/vector/pg.html#dataset-open-options
[vector-gdb]:         https://gdal.org/drivers/vector/openfilegdb.html#esri-file-geodatabase-openfilegdb

[geoserver-sql]:         https://docs.geoserver.org/latest/en/user/data/database/sqlview.html
[geoserver-sql-params]:  https://docs.geoserver.org/latest/en/user/data/database/sqlview.html#defining-parameters
[geoserver-rest]:        https://docs.geoserver.org/stable/en/user/rest/
[geoserver-rest-layers]: https://docs.geoserver.org/latest/en/api/#1.0.0/layers.yaml#/definitions/Layer
[time-formatting]:       https://docs.oracle.com/javase/8/docs/api/java/time/format/DateTimeFormatter.html#:~:text=All%20letters%20%27A%27%20to%20%27Z,use%0A%20%20%20%7D%20%20%20%20%20%20%20reserved%20for%20future%20use
[sld-cookbook]:          https://docs.geoserver.org/stable/en/user/styling/sld/cookbook/index.html

[gdal-translate]:       https://gdal.org/programs/gdal_translate.html#gdal-translate
[raster-common]:        https://gdal.org/programs/raster_common_options.html#common-options-for-raster-programs
[raster-common-a_srs]:  https://gdal.org/programs/raster_common_options.html#cmdoption-a_srs
[raster-common-t_srs]:  https://gdal.org/programs/raster_common_options.html#cmdoption-t_srs
[gdal-cog-t_srs]:       https://gdal.org/drivers/raster/cog.html#reprojection-related-creation-options
[gdal-cog]:             https://gdal.org/drivers/raster/cog.html#cog-cloud-optimized-geotiff-generator
[gdal-translate-oo]:    https://gdal.org/programs/gdal_translate.html#cmdoption-gdal_translate-oo
[gdal-translate-co]:    https://gdal.org/programs/gdal_translate.html#cmdoption-gdal_translate-co

[raster-drivers]: https://gdal.org/drivers/raster/index.html#raster-drivers
[raster-cog]:     https://gdal.org/drivers/raster/cog.html#cog-cloud-optimized-geotiff-generator
[raster-cog-co]:  https://gdal.org/drivers/raster/cog.html#creation-options
[raster-geotiff]: https://gdal.org/drivers/raster/gtiff.html#gtiff-geotiff-file-format

[postgis-raster-loader]: https://postgis.net/docs/using_raster_dataman.html#RT_Raster_Loader

[pgrouting]: http://pgrouting.org/
[osm2pgrouting-how-to-use]: https://github.com/pgRouting/osm2pgrouting#how-to-use

[3dcitydb-importer]:         https://3dcitydb-docs.readthedocs.io/en/version-2022.2/impexp/cli/import.html
[3dcitydb-importer-formats]: https://3dcitydb-docs.readthedocs.io/en/version-2022.2/impexp/import.html#import-supported-file-formats
[3dcitydb-importer-cli]: https://3dcitydb-docs.readthedocs.io/en/latest/impexp/cli/import.html#import-command

[py3dtiler]: https://github.com/VCityTeam/py3dtilers

[crome-2020]: https://www.data.gov.uk/dataset/be5d88c9-acfb-4052-bf6b-ee9a416cfe60/crop-map-of-england-crome-2020

[zone-id]: https://docs.oracle.com/javase/8/docs/api/java/time/ZoneId.html