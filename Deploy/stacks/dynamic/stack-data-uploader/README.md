# The Stack Data Uploader

> :memo: **Note:** In the commands and file snippets below placeholders are indicated using angled brackets, for example `<STACK NAME>`.
You will need to substitute in appropriate values before running any commands.

> :memo: **Note:** Unless otherwise stated all paths listed in this readme are relative to the [`Deploy/stacks/dynamic/stack-data-uploader`](.) directory in the TheWorldAvatar repository.

## Introduction

The Stack Data Uploader is designed to make it easier to ingest static data files into a stack.

## Datasets and subsets

Data files are grouped into *datasets*, each of which has its own configuration file and data directory.

By default all dataset configuration files in the [`inputs/configs/`](./inputs/config/) are read by the data uploader.
When a dataset's name matches with that of the stack then only that configuration file and its *external datasets* will be loaded.

Below is an example where there are two datasets.
One of which (*dataset1*) contains one data subset and another (*dataset2*) that contains two data subsets, each with their own subdirectory.
```sh
inputs/
  config/               # Directory in which the dataset configuration files should be stored
    dataset1.json       # Configuration file for dataset1
    dataset2.json       # Configuration file for dataset2
  data/                 # Directory in which the data files should be stored
    dataset1/           # Data directory for dataset1
      data.csv          # Only one data subset so no need for a subdirectory
    dataset2/           # Data directory for dataset2
      datasubset1/      # Data directory for data subset1
        polygon.geojson # Data file 
      datasubset2/      # Data directory for data subset2
        table.csv
```
## The Dataset configuration file

Each dataset should have its own JSON configuration file located in the [`inputs/config/`](./inputs/config) directory.
There are several example configuration files in the [`example_datasets`](../example_datasets/) directory.

### Datasets

The following table shows the top level nodes allowed in a configuration file.

| Key                                       | Required?                                                        | Default value                            | Description                                                                                                |
| ----------------------------------------- | ---------------------------------------------------------------- | ---------------------------------------- | ---------------------------------------------------------------------------------------------------------- |
| [`"name"`](#name)                         | No                                                               | The filename without the .json extension | The name of the dataset                                                                                    |
| [`"datasetDirectory"`](#datasetDirectory) | No                                                               | The dataset's name                       | The directory within `inputs/data/` that contains the data files associated with this dataset              |
| [`"skip"`](#skip)                         | No                                                               | `false`                                  | If set to `true` this dataset will be ignored by the data uploader                                         |
| [`"database"`](#database)                 | No[<sup>1</sup>](#1-at-least-one-of-these-needs-to-be-populated) | `"postgres"`                             | The name of the database within Postgres to which appropriate data will be uploaded                        |
| [`"workspace"`](#workspace)               | No[<sup>1</sup>](#1-at-least-one-of-these-needs-to-be-populated) | The dataset's name                       | The GeoServer workspace into which any 2D geospatial data layers, vector and raster, will be added         |
| [`"externalDatasets"`](#externaldatasets) | No[<sup>1</sup>](#1-at-least-one-of-these-needs-to-be-populated) | `[]`                                     | A list of other datasets' names. Each listed dataset will also be loaded if this dataset is loaded by name |
| [`"dataSubsets"`](#dataSubsets)           | No[<sup>1</sup>](#1-at-least-one-of-these-needs-to-be-populated) | `[]`                                     | A list of *data subset* objects                                                                            |
| [`"styles"`](#styles)                     | No[<sup>1</sup>](#1-at-least-one-of-these-needs-to-be-populated) | `[]`                                     | A list of GeoServer style file definition objects                                                          |
| [`"mappings"`](#mappings)                 | No[<sup>1</sup>](#1-at-least-one-of-these-needs-to-be-populated) | `[]`                                     | A list of Ontop mapping file definition objects                                                            |

##### <sup>1</sup> At least one of these needs to be populated.

#### `"name"`
This is the name of the dataset.
If left unspecified it is set to the name of the configuration file, without the .json extension.
This is the value that should be specified in the `"externalDatasets"` node of another dataset.
> :memo: **Note:** If the dataset's name matches the name of the stack it is being loaded into then only that dataset and its associated external datasets will be loaded.

#### `"datasetDirectory"`
The directory within `inputs/data/` that contains the data files associated with this dataset.

#### `"skip"`
Setting the `"skip"` value of a dataset to `true` will cause the data uploader to not load any of the data or files listed in that dataset.

#### `"database"`
The name of the database within Postgres to which appropriate data will be uploaded.
> :memo: **Note:** The database will be created if it doesn't already exist.

> :warning: **Warning:** Ontop can only access the default 'postgres' database so it is usually best not to change this value.

#### `"workspace"`
The GeoServer workspace into which any 2D geospatial data layers, vector and raster, will be added.
> :memo: **Note:** The workspace will be created if it doesn't already exist.

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
If there is only one data subset then this may be left unset and the files placed directly in the dataset directory.

#### `"styles"`
A list of GeoServer style file definition objects.
The styles defined here wll be loaded into the GeoServer workspace associated with the dataset.
Each entry requires the following values to be specified:

| Key      | Description                                           |
| -------- | ----------------------------------------------------- |
| `"name"` | The name of the style as it will be used in GeoServer |
| `"file"` | The name of the file containing the style definition  |

Currently only `.sld` style files are supported.
If required in the future support for other style formats might be add as GeoServer does support several other formats natively and a few more if the required plugins are loaded.

#### `"mappings"`
A list of Ontop mapping file definition objects provided as paths relative to the [`"datasetDirectory"`](#datasetDirectory).
Currently only the Ontop native format (`.obda`) is supported as it is much easier for both humans and Ontop to work with.
Ontop also supports the R2RML (`.ttl`) OBDA file standard but the data uploader would need changes to include matching support.

## Data types

The following data types are supported: [`vector`](#vector-data), [`raster`](#raster-data) and [`tabular`](#tabular-data).
A description of how each is processed and a summary of the available configuration options are provided below.

### Vector data

The `"vector"` data type should be used to load 2D point, line or polygon geospatial data.
The data loader does two things when uploading vector data: 
1. It uses the GDAL [`ogr2ogr`](https://gdal.org/programs/ogr2ogr.html#ogr2ogr) tool to read in data from a wide variety of file formats and output it to the PostgreSQL database in the stack.
The full list of file formats that `ogr2ogr` supports is given [here](https://gdal.org/drivers/vector/index.html#vector-drivers) although some of these might not be available depending on the exact GDAL Docker image being used, see [here](https://github.com/OSGeo/gdal/tree/master/docker) for details.
2. It uses the GeoServer REST API to create a new layer in GeoServer that can be used to visualise the newly uploaded geometries.

The options for these two processes are set using the following json objects within the respective data subset object in the dataset configuration file:

#### GDAL Options

In most situations the default `ogr2ogr` settings will be sufficient to upload the data but sometimes some extra options need to be supplied.
These can be specified within an `"ogr2ogrOptions"` object under the following keys:

##### `"sridIn"`
If the input dataset does not have an SRID/CRS/SRS specified then it can be specified as the value for the `"sridIn"` key.
When specifying a EPSG code for the SRS it needs to include the authority as well as the ID, for example `"EPSG:4296"` rather than just `4296` or `"4296"`.
This sets the value of the [`-a_srs`](https://gdal.org/programs/ogr2ogr.html#cmdoption-ogr2ogr-a_srs) argument passed to `ogr2ogr`.

##### `"sridOut"`
If you want to reproject the coordinates the target SRID/CRS/SRS can be set as the value for the `"sridOut"` key.
When specifying a EPSG code for the SRS it needs to include the authority as well as the ID, for example `"EPSG:4296"` rather than just `4296` or `"4296"`.
This sets the value of the [`-t_srs`](https://gdal.org/programs/ogr2ogr.html#cmdoption-ogr2ogr-t_srs) argument passed to `ogr2ogr`.
It also means any value specified for `"sridIn"` is passed as the value of the [`-s_srs`](https://gdal.org/programs/ogr2ogr.html#cmdoption-ogr2ogr-s_srs) argument, rather than `-a_srs`.

##### `"inputDatasetOpenOptions"`
Some data source formats require additional options to be set for the geometries and their metadata to be loaded correctly.
These can be set as key-value pairs within a `"inputDatasetOpenOptions"` object.
These options are format specific and are generally described in a section with the heading "Open options" or "Dataset open options" on the relevant driver documentation page.
All of the vector drivers are listed [here](https://gdal.org/drivers/vector/index.html#vector-drivers) with links to their documentation.
The values are passed to the `ogr2ogr` tool as `NAME=VALUE` pair arguments of the [`-oo`](https://gdal.org/programs/ogr2ogr.html#cmdoption-ogr2ogr-oo) option.

##### `"layerCreationOptions"`
All vector datasets are loaded into the PostGIS database within the stack with each data subset being loaded as a separate layer/table.
In general these options should not need to be set explicitly as the `ogr2ogr` tool can usually work them out from the source dataset, or use default values.
However, setting one or more of them may be required to fix specific problems with the input dataset.
The layer creation options provided by the PostGIS driver are described [here](https://gdal.org/drivers/vector/pg.html#layer-creation-options).
The values are passed to the `ogr2ogr` tool as `NAME=VALUE` pair arguments of the [`-lco`](https://gdal.org/programs/ogr2ogr.html#cmdoption-ogr2ogr-lco) option.

##### `"outputDatasetOpenOptions"`
The PostGIS driver has a few options to control how the `ogr2ogr` tool connects to the database.
However, all of the essential information (database name, port number, username, etc.) is set automatically so these settings should only be changed if absolutely necessary.
The dataset open options provided by the PostGIS driver are described [here](https://gdal.org/drivers/vector/pg.html#dataset-open-options).
The values are passed to the `ogr2ogr` tool as `NAME=VALUE` pair arguments of the [`-doo`](https://gdal.org/programs/ogr2ogr.html#cmdoption-ogr2ogr-doo) option.

##### `"otherOptions"`
Several non-driver specific options are also available.
These can be set as key-array-valued pairs within an `"otherOptions"` object.
This allows for multiple values per option (`["value1", "value2"]`) but requires that single values are still placed within an array  (`["value"]`) and valueless flags are paired with an empty array (`[]`).
A list of possible options can be found on the [vector common options](https://gdal.org/programs/vector_common_options.html#common-options-for-vector-programs) and [ogr2ogr options](https://gdal.org/programs/ogr2ogr.html#ogr2ogr) pages.

##### `"envVars"`
A few aspects of some of the drivers can also be set via environment variables.
These can be set as key-value pairs within an `"envVars"` object.

##### Common drivers
- [Comma Separated Value (.csv)](https://gdal.org/drivers/vector/csv.html#comma-separated-value-csv)
- [ESRI Shapefile / DBF](https://gdal.org/drivers/vector/shapefile.html#esri-shapefile-dbf)
- [PostgreSQL / PostGIS](https://gdal.org/drivers/vector/pg.html#postgresql-postgis) (mainly as the output)

#### GeoServer Options
For vector data you can add a `geoServerSettings` node within the relevant data subset in the configuration json.
These settings are generally only required to add dynamic (value-based) styling to the layers for visualisation.
Within that the following nodes can be added.
- `"virtualTable"` creates a [SQL View](https://docs.geoserver.org/latest/en/user/data/database/sqlview.html) which is specified as follows:
  - `"name"` a name is required.
  - `"sql"` an SQL query that defines the virtual table is required.
  - `"keyColumn"` specify column for [parameter](https://docs.geoserver.org/latest/en/user/data/database/sqlview.html#defining-parameters) key.
  - `"escapeSql"` is Boolean `true` or `false`.
    This concerns the handling of special characters in column names such as setting single-quotes to doubled single-quotes.
  - `"geometry"` specifies the geometry with the following `key:value` pairs.
    - `"name"` name of column with the geometry.
    - `"type"` one of `Point`, `LineString`, `LinearRing`, `Polygon`, `MultiPoint`, `MultiLineString`, `MultiPolygon`, `GeometryCollection`.
    - `"srid"` EPSG code as an integer, for example `4296` rather than `"EPSG:4296"` or `"4296"`.
      Note that this is different from the GDAL Options.
  - `"parameter"` specify individual [parameters](https://docs.geoserver.org/latest/en/user/data/database/sqlview.html#defining-parameters) with the following `key:value` pairs.
    - `"name"` parameter name.
    - `"defaultValue"` default value of parameter.
    - `"regexpValidator"` validation regular expression.
- `"defaultStyle"` name of style within GeoServer that will be the style if of this layer if no other style is specified.

These are the most commonly used options, for more see the examples [here](https://docs.geoserver.org/stable/en/user/rest/) and [here](https://docs.geoserver.org/latest/en/api/#1.0.0/layers.yaml#/definitions/Layer).
      
### Raster data

The `"raster"` data type should be used to load raster/coverage geospatial data.
The data loader does three things when uploading raster data: 
1. It uses the GDAL [`gdal_translate`](https://gdal.org/programs/gdal_translate.html#gdal-translate) tool to read in data from a wide variety of file formats and output it to [Cloud Optimized GeoTIFF (COG)](https://gdal.org/drivers/raster/cog.html#raster-cog) files stored in the stack.
  This is an extension of the GeoTIFF format and both are very efficient to read.
  The full list of file formats that `gdal_translate` supports is given [here](https://gdal.org/drivers/raster/index.html#raster-drivers) although some of these might not be available depending on the exact GDAL Docker image being used, see [here](https://github.com/OSGeo/gdal/tree/master/docker) for details.
2. It uses the PostGIS [`raster2pgsql`](https://postgis.net/docs/using_raster_dataman.html#RT_Raster_Loader) tool to register the GeoTIFF files in the PostGIS database.
   The `raster2pgsql` tool also automatically divides the data into tiles in the database to make geospatial searching more efficient.
3. It uses the GeoServer REST API to create a new coverage layer in GeoServer that can be used to visualise the newly uploaded data.

#### GDAL Options

In most situations the default `gdal_translate` settings will be sufficient to upload the data but sometimes some extra options need to be supplied.
These can be specified within an `"gdalTranslateOptions"` object (previously just called `"options"`) under the following keys:

##### `"sridIn"`
If the input dataset does not have an SRID/CRS/SRS specified then it can be specified as the value for the `"sridIn"` key.
When specifying a EPSG code for the SRS it needs to include the authority as well as the ID, for example `"EPSG:4296"` rather than just `4296` or `"4296"`.
A full explanation of the acceptable SRS formats is given [here](https://gdal.org/programs/raster_common_options.html#cmdoption-t_srs).
This sets the value of the [`-a_srs`](https://gdal.org/programs/raster_common_options.html#cmdoption-a_srs) argument passed to `gdal_translate`.

##### `"sridOut"`
If you want to reproject the coordinates the target SRID/CRS/SRS can be set as the value for the `"sridOut"` key.
When specifying a EPSG code for the SRS it needs to include the authority as well as the ID, for example `"EPSG:4296"` rather than just `4296` or `"4296"`.
A full explanation of the acceptable SRS formats is given [here](https://gdal.org/programs/raster_common_options.html#cmdoption-t_srs).
This sets the value of the [`-t_srs`](https://gdal.org/programs/raster_common_options.html#cmdoption-t_srs) argument passed to `gdal_translate`.
It also means any value specified for `"sridIn"` is passed as the value of the [`-s_srs`](https://gdal.org/programs/raster_common_options.html#cmdoption-s_srs) argument, rather than `-a_srs`.

##### `"inputDatasetOpenOptions"`
Some data source formats require additional options to be set for the geometries and their metadata to be loaded correctly.
These can be set as key-value pairs within a `"inputDatasetOpenOptions"` object.
These options are format specific and are generally described in a section with the heading "Open options" or "Dataset open options" on the relevant driver documentation page.
All of the raster drivers are listed [here](https://gdal.org/drivers/raster/index.html#raster-drivers) with links to their documentation.
The values are passed to the `gdal_translate` tool as `NAME=VALUE` pair arguments of the [`-oo`](https://gdal.org/programs/gdal_translate.html#cmdoption-gdal_translate-oo) option.

##### `"creationOptions"`
All raster datasets are loaded into the PostGIS database within the stack with each data subset being loaded as a separate layer/table.
In general these options should not need to be set explicitly as the `gdal_translate` tool can usually work them out from the source dataset, or use default values.
However, setting one or more of them may be required to fix specific problems with the input dataset.
The creation options provided by the COG driver are described [here](https://gdal.org/drivers/raster/cog.html#creation-options).
The values are passed to the `gdal_translate` tool as `NAME=VALUE` pair arguments of the [`-co`](https://gdal.org/programs/gdal_translate.html#cmdoption-gdal_translate-co) option.

##### `"otherOptions"`
Several non-driver specific options are also available.
These can be set as key-array-valued pairs within an `"otherOptions"` object.
This allows for multiple values per option (`["value1", "value2"]`) but requires that single values are still placed within an array  (`["value"]`) and valueless flags are paired with an empty array (`[]`).
A list of possible options can be found on the [raster common options](https://gdal.org/programs/raster_common_options.html#common-options-for-raster-programs) and [gdal_translate options](https://gdal.org/programs/gdal_translate.html#gdal-translate) pages.

##### Common drivers
- [GeoTIFF](https://gdal.org/drivers/raster/gtiff.html#gtiff-geotiff-file-format)
- [COG – Cloud Optimized GeoTIFF](https://gdal.org/drivers/raster/cog.html#cog-cloud-optimized-geotiff-generator) (mainly as the output)

#### GeoServer Options

For raster data you can add a `geoServerSettings` node within the relevant data subset in the configuration json.
Within that the following nodes can be added.
- `"layerSettings"`
  - `"defaultStyle"`: name of style within GeoServer that will be the style if of this layer if no other style is specified.

## Tabular data

The `"tabular"` data type should be used to load non-geospatial data.
The data loader just does one thing when uploading tabular data: 
1. It uses the GDAL [`ogr2ogr`](https://gdal.org/programs/ogr2ogr.html#ogr2ogr) tool to read in data from a wide variety of file formats and output it to the PostgreSQL database in the stack.
As the data is intended to be non-geospatial, this is most useful for reading in data from [comma separated value (.csv)](https://gdal.org/drivers/vector/csv.html#comma-separated-value-csv), and Microsoft Excel's [XLS](https://gdal.org/drivers/vector/xls.html#xls-ms-excel-format) and [XLSX](https://gdal.org/drivers/vector/xlsx.html#xlsx-ms-office-open-xml-spreadsheet) formatted files.
The full list of file formats that `ogr2ogr` supports is given [here](https://gdal.org/drivers/vector/index.html#vector-drivers) although some of these might not be available depending on the exact GDAL Docker image being used, see [here](https://github.com/OSGeo/gdal/tree/master/docker) for details.

#### GDAL Options

These are the same as listed in the vector [GDAL Options](#gdal-options) although obviously the options specific to geospatial data will not be relevant.

##### Common drivers
- [Comma Separated Value (.csv)](https://gdal.org/drivers/vector/csv.html#comma-separated-value-csv)
- [XLS - MS Excel format](https://gdal.org/drivers/vector/xls.html#xls-ms-excel-format)
- [XLSX - MS Office Open XML spreadsheet](https://gdal.org/drivers/vector/xlsx.html#xlsx-ms-office-open-xml-spreadsheet)
- [PostGIS](https://gdal.org/drivers/raster/postgisraster.html#postgisraster-postgis-raster-driver) (mainly as the output)
## Prerequisites

These are the same as listed in [The Stack Manager](../stack-manager/README.md#prerequisites).

You should also initialise the stack by following the instructions in [Spinning up a Stack](../stack-manager/README.md#spinning-up-a-stack).

## Running the Stack Data Uploader

To load static data files into the stack please follow the instructions below:

1. Open the Workspace in the [`Deploy/stacks/dynamic`](..) directory in VSCode (or go to the [`stack-data-uploader`](.) subdirectory within it in a `bash` terminal).

2. To start you can follow the instructions in the [README.md](../example_datasets/README.md) file in the [`example_datasets`](../example_datasets/) directory to load in one of the example datasets.
To load another data set, put the relevant data in the [`inputs/data/`](./inputs/data) directory.
The stack uploader supports vector, raster, and non-geospatial tabular data; these can come in a variety of file formats including JSON, GeoJSON, JPEG, PNG, Shapefile, and CSV.

3. Create a JSON file in [`inputs/config/`](./inputs/config/) to configure how the data is to be uploaded.
The steps to create such a file are detailed in full here but it is recommended that you also look at the example configurations in the `example_datasets` directory.
The following is a template for a basic config file.
It is necessary that you replace any placeholders, `<...>`, with values appropriate for your data.
    ```json
    {
        "database": "postgres",
        "workspace": "the_world_avatar",
        "datasetDirectory": "<directory in ./data in which the data is stored>",
        "dataSubsets": [
            {
                "type": "<vector, raster, or tabular>",
                "skip": false,
                "schema": "public",
                "table": "<what you wish to name your table>",
                "subdirectory": "<subdirectory in datasetDirectory in which the data subset is stored>"
            },
            {
                <... another subset>
            },
            <...>
        ]
    }
    ``` 
    
4. You can create a `.sld` GeoServer style in the config directory and upload it.
In GeoServer the style will be named `the_world_avatar:<style name>` as it is created within the `the_world_avatar` workspace.
The following node can be added to the top level of the config file.
    ```json
    "styles": [
        {
            "name": "<style name>",
            "file": "<style file>.sld"
        }
    ]
    ```

5. Create a `.obda` file in the `datasetDirectory` to specify the mapping.
Once again you can look at the examples in the [`example_datasets`](../example_datasets/) directory or follow this simplified template.
    ```obda
    [PrefixDeclaration]
    ex:     http://example.org/

    # can comment like this

    [MappingDeclaration] @collection [[
    mappingId <name of first mapping> 
    target    <Turtle query to specify triples using columns from SQL in {}> 
    source    <SQL query to make table of data to be mapped>

    mappingId <name of second mapping> 
    target    <Turtle query> # can also comment like this
    source    <SQL query>
    ]]
    ```
    Add the following as a top level node in the config file.
    ```json
    "mappings": [
        "<name of mapping file>.obda"
    ]
    ```

6. From a terminal in the [`stack-data-uploader`](.) directory, start the `stack-data-uploader` container by running the following:
    ```console
    ./stack.sh start <STACK NAME>
    ```

## Using Specific Data Sets

If you don't want the use every config file you can either use `"skip"=true` or name your stack so that the relevant config file is named `<STACK NAME>.json`.
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

## Debugging the Stack Data Uploader in VSCode

1. Add the following entry into top level node the JSON file `stack-data-uploader/.vscode/settings.json`, creating the file if it doesn't exist.
    ```json
    "debug.port": "<DEBUG PORT>"
    ```
    A value around `5007` for `<DEBUG PORT>` should be appropriate, this must be different to the one specified for the `stack-manager`.

2. In the `Run and Debug` side panel of VSCode run the `Debug (stack-data-uploader)` configuration.

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
- For certain vector geometries (e.g. `MULTILINESTRING` and `LINESTRING`) it is necessary to use [`-nlt`](https://gdal.org/programs/ogr2ogr.html#cmdoption-ogr2ogr-nlt) to specify the geometry in the following way.
    ```json
    "ogr2ogrOptions": {
        "otherOptions": {
            "-nlt": ["<GEOMETRY TYPE>"]
        }
    }
    ```

- To upload only specific properties/fields from a source dataset their names can be specified as comma-separated values for the [`"-select"`](https://gdal.org/programs/ogr2ogr.html#cmdoption-ogr2ogr-select) option under [`"otherOption"`](#otheroptions).
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
    The [cropmap](../example_datasets/inputs/config/cropmap.json) example shows this being used to remove some fields (containing calculated areas and perimeters) that were not constantly named across all of the [crop-map-of-england-crome-2020](https://www.data.gov.uk/dataset/be5d88c9-acfb-4052-bf6b-ee9a416cfe60/crop-map-of-england-crome-2020) Shapefiles.


#### [ESRI File Geodatabase](https://gdal.org/drivers/vector/openfilegdb.html#esri-file-geodatabase-openfilegdb)

- As described in the GDAL documentation [ESRI File Geodatabase](https://gdal.org/drivers/vector/openfilegdb.html#esri-file-geodatabase-openfilegdb) datasets must be stored in a directory/folder with a name that ends with the `.gdb` extension.
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
