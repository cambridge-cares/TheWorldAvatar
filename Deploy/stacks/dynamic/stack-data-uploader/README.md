# The Stack Data Uploader

In the commands below placeholders are shown as `<STACK NAME>`, you will need to substitute in the required value when running the command.

## Prerequisites

These are the same as listed in [The Stack Manager](../stack-manager/README.md#prerequisites).

You should also initialise the stack by following the instructions in [Spinning up a Stack](../stack-manager/README.md#spinning-up-a-stack).

## Running the Stack Data Uploader

To load static data files into the stack please follow the instructions below:

1. Open the Workspace in the `Deploy/stacks/dynamic` directory in VSCode (or go to the `stack-data-uploader` subdirectory within it in a `bash` terminal).

2. You can start from an example dataset by following the instructions in the [README.md](../example_datasets/README.md) file in the `example_datasets` directory. Alternatively put the relevant data in the `.inputs/data/` directory. If there are multiple data sets it is advised that you place them in seperate directories. The stack uploader supports all of vector, raster, and tabular file formats including JSON, GeoJSON, JPEG, PNG, shapefile, and CSV.

3. Create a JSON file in the `.inputs/data/` to configure how the data is to be uploaded. You can again look at the examples in `example_datasets` directory or follow this simplified template.
```
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
            "subdirectory": "<subdirectoy in datasetDirectory in which the dat subset is stored>"
        },
        {
            <... another subset>
        },
        <...>
    ]
}
```
Naturally you will need to add the necessary details where `< >` are used. This can be modified further to account for more configuration options but this will perform a basic upload of data.

4. From a terminal in the `stack-data-uploader` directory, start the `stack-data-uploader` container by running the following:
    ```console
    ./stack.sh start <STACK NAME>
    ```

5. You can verify that the data has been uploaded by looking in the PostgreSQL GUI (or you can see the data in a basic visualisation the Geoserver GUI). Details on how to access this can be found in [Spinning up a Stack](../stack-manager/README.md#spinning-up-a-stack). 

If you simply want to see this data on a map without any knowledge graph mappings you can now move on to visulaisation. If not the following steps will create and utilize an Ontop mapping to convert the data into triples. 

6. Create a `.obda` file in the `datasetDirectory` to specify the mapping. Once again you can look at the examples in `example_datasets` directory or follow this simplified template.

```
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
You can test out your SQL queries in PostgreSQL GUI by selecting a table and clicking `Edit` to quesry the table.

7. Add the following to your config file.
```
{
    "database": "postgres",
    "workspace": "the_world_avatar", 
    "datasetDirectory": ...

    ...

    "mappings": [
        "<name of mapping file>.obda"
    ]
}
```

8. Preform step 4 again. You can now query the knowledge graph you have created in the [Ontop GUI](http://localhost:3838/ontop/ui/).

## Debugging the Stack Data Uploader in VSCode

1. Add the following entry into top level node the JSON file `stack-data-uploader/.vscode/settings.json`, creating the file if it doesn't exist.
    ```json
    "debug.port": "<DEBUG PORT>"
    ```
    A value around `5007` for `<DEBUG PORT>` should be appropriate, this must be different to the one specified for the `stack-manager`.

2. In the `Run and Debug` side panel of VSCode run the `Debug (stack-data-uploader)` configuration.

## Developing the Stack Data Uploader in VSCode

You will need permission to push to the CMCL package repository to be able to build the stack-data-uploader project

1. Follow the instuctions in step 1. of [Debugging the Stack Data Uploader in VSCode](#debugging-the-stack-data-uploader-in-vscode)

2. Create two files called `repo_username.txt` and `repo_password.txt` in the `stack-data-uploader/docker/credentials` directory. Populate the files with your GitHub username and access token (with scope to write packages), respectively.

3. In the `Run and Debug` side panel of VSCode run the `Build and Debug (stack-data-uploader)` configuration.
