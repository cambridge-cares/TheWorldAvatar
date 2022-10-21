# The Stack Data Uploader

In the commands below placeholders are shown as `<STACK NAME>`, you will need to substitute in the required value when running the command.

## Prerequisites

These are the same as listed in [The Stack Manager](../stack-manager/README.md#prerequisites).

You should also initialise the stack by following the instructions in [Spinning up a Stack](../stack-manager/README.md#spinning-up-a-stack).

## Running the Stack Data Uploader

To load static data files into the stack please follow the instructions below:

1. Open the Workspace in the `Deploy/stacks/dynamic` directory in VSCode (or go to the `stack-data-uploader` subdirectory within it in a `bash` terminal).

2. To start you can follow the instructions in the [README.md](../example_datasets/README.md) file in the `example_datasets` directory to load in one of the example datasets. To load another data set, put the relevant data in the `.inputs/data/` directory. The stack uploader supports vector, raster, and non-geospatial tabular data; these can come in a variety of file formats including JSON, GeoJSON, JPEG, PNG, Shapefile, and CSV.

3. Create a JSON file in `.inputs/config/` to configure how the data is to be uploaded. The steps to create such a file are detailed in full here but it is recommended that you also look at the example configurations in the `example_datasets` directory. The following is a template for a basic config file. It is necessary that you replace the any text fields with `< >`.
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
    The following options are also supported.

    **Vector and Tabular Options:**

    - GDal Options

        An `"ogr2ogrOptions"` node within the relevant data subset in the configuration json can be added. Within that the following nodes can be added.
        - `"inputDatasetOpenOptions"` implements [-oo](https://gdal.org/programs/ogr2ogr.html#cmdoption-ogr2ogr-oo). These open options are driver specific and details on them can be found in the driver pages below.
        - `"layerCreationOptions"` implements [`-lco`](https://gdal.org/programs/ogr2ogr.html#cmdoption-ogr2ogr-lco).
        - `"outputDatasetOpenOptions"` implements [`-doo`](https://gdal.org/programs/ogr2ogr.html#cmdoption-ogr2ogr-doo).
        - `"envVars"` allows you to set environment variables.
        - `"otherOptions"` allows you to add any other flag you wish to explicitly. You can find the options for these in the [vector common options](https://gdal.org/programs/vector_common_options.html) and the [ogr2ogr options](https://gdal.org/programs/ogr2ogr.html).
        
        The key value pairs `"sridIn"` and `"sridOut"` can also be used inside `"ogr2ogrOptions"`. These use a combination of [`-t_srs`](https://gdal.org/programs/ogr2ogr.html#cmdoption-ogr2ogr-t_srs), [`-s_srs`](https://gdal.org/programs/ogr2ogr.html#cmdoption-ogr2ogr-t_srs), and [`-a_srs`](https://gdal.org/programs/ogr2ogr.html#cmdoption-ogr2ogr-t_srs) to set the input and output SRS.
            
        Drivers:
        - [CSV](https://gdal.org/drivers/vector/csv.html)
        - [PostGIS](https://gdal.org/drivers/vector/pg.html)
        - [Shapefile](https://gdal.org/drivers/vector/shapefile.html)

    - GeoServer Options

        For vector data you can add a `geoServerSettings` node within the relevant data subset in the configuration json. Within that the following nodes can be added.
        - `"virtualTable"` creates a [SQL View](https://docs.geoserver.org/latest/en/user/data/database/sqlview.html) which is specified as follows.
            - `"name"`: a name is required. 
            - `"sql"`: a SQL query is required.
            - `"keyColumn"`: specify column for [parameter](https://docs.geoserver.org/latest/en/user/data/database/sqlview.html#defining-parameters) keys. 
            - `"escapeSql"` is Boolean `true` or `false`. This concerns the handling of special characters in column names such as setting single-quotes to doubled single-quotes.
            - `"geometry"` specifies the geometry with the following `key:value` pairs.
                - `"name"`: name of column with the geometry.
                - `"type"`: one of `Point`, `LineString`, `LinearRing`, `Polygon`, `MultiPoint`, `MultiLineString`, `MultiPolygon`, `GeometryCollection`.
                - `"srid"`: EPSG code as an integer.
            - `"parameter"` specify individual [parameters](https://docs.geoserver.org/latest/en/user/data/database/sqlview.html#defining-parameters) with the following `key:value` pairs.
                - `"name"`: parameter name.
                - `"defaultValue"`: default value of parameter.
                - `"regexpValidator"`: validation regular expression.
        - `"defaultStyle"`: name of style within GeoServer that will be the style if of this layer if no other style is specified.
        These are the most commonly used options, for more see the examples [here](https://docs.geoserver.org/stable/en/user/rest/) and [here](https://docs.geoserver.org/latest/en/api/#1.0.0/layers.yaml#/definitions/Layer).
      
    **Raster Options:**

    - GDal Options

        An `"options"` node within the relevant data subset in the configuration json can be added. Within that the following nodes can be added.
        - `"inputDatasetOpenOptions"` implements [`-oo`](https://gdal.org/programs/gdal_translate.html#cmdoption-gdal_translate-oo). These open options are driver specific and details on them can be found in the driver pages below.
        - `"creationOptions"` implements [`-co`](https://gdal.org/programs/raster_common_options.html#cmdoption-co). These creation options are driver specific and details on them can be found in the driver pages below.
        -  `"envVars"` allows you to set environment variables.
        - `"otherOptions"` allows you to add any other flag you wish to explicitly.
        
        The `key:value` pairs `"sridIn"` and `"sridOut"` can also be used inside `"options"`. These use a combination of [`-t_srs`](https://gdal.org/programs/ogr2ogr.html#cmdoption-ogr2ogr-t_srs), [`-s_srs`](https://gdal.org/programs/raster_common_options.html#cmdoption-s_srs), and [`-a_srs`](https://gdal.org/programs/raster_common_options.html#cmdoption-a_srs) to set the input and output SRS.

        Drivers:
        - [PostGIS](https://gdal.org/drivers/raster/postgisraster.html)

    - GeoServer Options

        For vector data you can add a `geoServerSettings` node within the relevant data subset in the configuration json. Within that the following nodes can be added.
        - `"layerSettings"`
            - `"defaultStyle"`: name of style within GeoServer that will be the style if of this layer if no other style is specified.

4. You can create a `.sld` GeoServer style in the config directory and upload it. In GeoServer the style will be named `the_world_avatar:<style name>` as it is created within the `the_world_avatar` workspace. The following node can be added to the top level of the config file.
    ```json
        "styles": [
            {
                "name": "<style name>",
                "file": "<style file>.sld"
            }
        ]
    ```

5. Create a `.obda` file in the `datasetDirectory` to specify the mapping. Once again you can look at the examples in `example_datasets` directory or follow this simplified template.
    ```obda
    [PrefixDeclaration]
    ex:     http://example.org/

    # can comment like this

    [MappingDeclaration] @collection [[
    mappingId	<name of first mapping> 
    target		<Turtle query to specify triples using columns from SQL in {}> 
    source      <SQL query to make table of data to be mapped>

    mappingId	<name of second mapping> 
    target		<Turtle query> # can also comment like this
    source      <SQL query>
    ]]
    ```
    Add the following as a top level node in the config file.
    ```json
        "mappings": [
            "<name of mapping file>.obda"
        ]
    ```

6. From a terminal in the `stack-data-uploader` directory, start the `stack-data-uploader` container by running the following:
    ```console
    ./stack.sh start <STACK NAME>
    ```

## Using Specific Data Sets

If you don't want the use every config file you can either use `"skip"=true` or name your stack so that the relevant config file is named `<STACK NAME>.json`. If you want to use a few config files you can create one master config file named `<STACK NAME>.json` with the following.
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

2. Create two files called `repo_username.txt` and `repo_password.txt` in the `stack-data-uploader/docker/credentials` directory. Populate the files with your GitHub username and access token (with scope to write packages), respectively.

3. In the `Run and Debug` side panel of VSCode run the `Build and Debug (stack-data-uploader)` configuration.

## Troubleshooting

- The best place to start is to look at the container logs. These can be seen by right clicking on the containers on VSCode or through Docker Desktop.
- It is often helpful to run the uploader after each step after step 3 of [Running the Stack Data Uploader](#Running-the-Stack-Data-Uploader). This way you can look at look at the user interfaces of the various services (see step 5 of [Spinning up a Stack](../stack-manager/README.md#spinning-up-a-stack)) and be able to see your data sets being populated here.
- When writing SQL queries you can try them in the Adminer first.
- It is recommended that you write and validate GeoServer styles in the GeoServer UI, saving it to a `.sld` file.

### Quick Fixes

- For certain vector geometries (e.g. `MULTILINESTRING` and `LINESTRING`) it is necessary to use [`-nlt`](https://gdal.org/programs/ogr2ogr.html#cmdoption-ogr2ogr-nlt) to specify the geometry in the following way. 
    ```json
    "ogr2ogrOptions": {
                    "otherOptions": {
                        "-nlt": ["<GEOMETRY TYPE>"]
                    }
                }
    ```