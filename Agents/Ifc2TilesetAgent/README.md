# IFC2Tileset Agent

## Description
This agent queries and processes the IFC data stored on a knowledge graph into the [3D Tiles Next](https://github.com/CesiumGS/3d-tiles/tree/main/next) specifications for visualisation in Cesium.
Before running this agent, the IFC model **MUST** be instantiated with the [Ifc2OntoBim agent](https://github.com/cambridge-cares/TheWorldAvatar/tree/main/Agents/Ifc2OntoBIMAgent). Please ensure that the IFC model has been preprocessed according to the [Tips for BIM processing](#4-tips-for-bim-processing) section. Tilesets and
their geometry (`gltf`/`glb`) files will be generated in the output `data` directory.

A brief description of the workflow can be found below:
1. Instantiate the semantic and geometry data in IFC models using the [Ifc2OntoBim agent](https://github.com/cambridge-cares/TheWorldAvatar/tree/main/Agents/Ifc2OntoBIMAgent).
2. Queries the metadata (IFC uid, asset name, data IRI) of relevant assets from a specified endpoint.
3. Split the geometries in the IFC model based on these metadata into their individual assets (if necessary).* These are then converted into glTF models that are stored locally.
4. 3D Tilesets are generated for the building, solar panels, and sewerage network (if they exist). The building tileset are supplemented with the queried asset metadata (if any).

**At the moment, this agent is unable to process the geometry data queried from the knowledge graph, and will require the same IFC model file as an input to create the geometry files.*

```mermaid
    %%{init: {'theme':'neutral', 'fontFamily':'verdana'}}%%
    flowchart TB
    subgraph Overview
        direction TB
        id1{{<b>START</b> - IFC Model: <br> semantic + geometry}}:::start --- |generate Abox|id2[Ifc2OntoBimAgent] 
        id2 --> |store into| id3[(<b>Knowledge Graph</b>)]:::db
        Ifc2TilesetAgent --> |query <br> iri + name|id3
        Ifc2TilesetAgent -->|input| id4[[Webapp<br>Visualisation]]:::webapp
        id4 --- id5[FeatureInfoAgent] 
        id5 --> |query selected <br> metadata + time series|id3

        subgraph Ifc2TilesetAgent
        direction LR
            id6(<b>START</b><br> query results) ---|split IFC assets| id7{{IFC file}}:::start
            id7 --> |geometry conversion|id8{{glTF/glb}}
            id6 --> |store metadata|id9{{<b>OUTPUT</b><br>3D Tileset}}
            id8 --> id9
        end
    end
    classDef start fill:#fff,stroke:#048A81,stroke-width:5px;
    classDef webapp fill:#fff,stroke:#FE4A49,stroke-width:3px;
    classDef db fill:#D0F4EA,stroke:#3066BE,stroke-width:3px, color:#0F084B;
    style Overview color:#fff;
```

# Instructions
## 1. Building the agent
The agent is designed for deployment on [Docker](#12-docker-deployment). Although it can be deployed on a local development environment, this is not the recommended setup. 

### 1.1 Required dependencies:
1. **IfcOpenShell**
    - Required to load and parse IFC files
    - Download required version from https://blenderbim.org/docs-python/ifcopenshell-python/installation.html
    - Extract and place the `ifcopenshell` from `blenderbim/libs/site/packages/` to `<venv_name>\Lib\site-packages`
    - Delete the remaining extracted content
2. **IfcConvert.exe**
    - Required to convert IFC to glb format
    - Download IfcConvert.exe from: https://blenderbim.org/docs-python/ifcconvert/installation.html
    - Extract it to `<root>`
3. **gltf-pipeline NPM package**
    - Required to convert glb to glTF format [glTF is human readable which is useful for geometry operations]
    - Read documentation from https://github.com/CesiumGS/gltf-pipeline
    - Install the library globally using `npm install -g gltf-pipeline`

### 1.2 Docker Deployment:
**TEST ENVIRONMENT**
- Deploy the agent to execute the unit and integration tests by running the following code in the CLI at the `<root>` directory:
```
docker compose -f "./docker/docker-compose.test.yml" up -d --build 
```
- Verified that all tests have passed by looking at the container logs 
-- Container must be built (regardless of failure) to ensure services are running for tests.

**DEVELOPMENT ENVIRONMENT**
- Deploy the agent for development by running the following code in the CLI at the `<root>` directory. Developers can attach the debugger running at port 5678.
```
docker compose -f "./docker/docker-compose.debug.yml" up -d --build 
```
- **Visual Studio Code**: 
    - Developers will navigate to the `Run and Debug` tab and click the button. 
    - If there is a missing launch.json, please open the `Command Palette` in any `.py` file, and run “Debug: Start Debugging”. Choose “Remote Attach”, and select `localhost` and `5678` as host and port. This should create a file at `<root>/.vscode/launch.json`
    - Once the container has been built and the `launch.json` is available, click the green arrow at the top left corner to run the debugger. Debugging capabilities should now be available.

**PRODUCTION ENVIRONMENT**
- Deploy the agent and its dependencies by running the following code in the command prompt at the `<root>` directory:
```
docker-compose up -d 
```

## 2. Running the agent
### 2.1 Precursor
Place only one IFC file in `<root>\data\ifc\`. This directory is directly linked to the relevant directory in the Docker container. The agent is only able to convert ONE IFC model at a time.

Please modify the following properties in `config/properties.yaml`:
- `root_tile`*: Bounding box for the root tile ie entire model
- `child_tile`*: Bounding box for all children tiles containing assets
- `query_endpoint`^ : SPARQL endpoint for Query operations
- `update_endpoint`^ : SPARQL endpoint for UPDATE operations

**WIP: Generating bounding boxes from their model automatically, will make this redundant*

^*Endpoints are required to query for metadata in tileset and interactions during visualisation*

### 2.2 API
Instructions for the agent and its various API routes can be found at the API root `http://localhost:5105/`. Users can visit this route in any browser to verify if the agent is running.

A brief overview is as follows:
- **POST** request to convert an IFC model to 3D Tileset, and output tileset.json files. Accepted parameters:
    1. `assetUrl`  
    - Sets the file path to directory or url holding the glTF assets in the tilesets generated.
    - Valid formats include `"."`, `"./file/path"`, `"../../file/path"`, and `"http://www.example.com"`. Please do not add an ending `/`, which will be generated in the code itself.
```
/api
```

### 2.3 POST Request
Run the agent by sending a POST request with the required JSON Object to the necessary endpoint. A sample request in `curl` syntax is as follows:
```
curl -X POST localhost:5105/api -H 'Content-Type: application/json' -d '{\"assetUrl\":\"./gltf\"}'  
```

If the agent ran successfully, a JSON Object would be returned as follows:
```
{"Result":["IFC model has successfully been converted. Please visit the 'data' directory for the outputs"]}
```

## 3. For Developers
The agent have been packaged into the following submodules:
- `agent/app.py` is the agent's entry point that accepts and processes all HTTP requests
- `agent/config` contain functions to retrieve the properties in `config/properties.yaml`
- `agent/exceptions` contain the exceptions encountered in this agent
- `agent/kgutils` contain miscellaneous functions to access and query the Knowledge graph
- `agent/utils` contain miscellaneous functions for searching, validating inputs, and system operations
- `agent/ifc2gltf` contain functions to query the metadata, and process the IFC input into their individual geometry files for each asset
- `agent/ifc2tileset` contain functions to generate the tileset and write them to json
    - If you are unable to see the assets, **Modify the bounding box coordinates** according to your use case

As Git does not allow empty directories, `.gitignore` files have been added to the subdirectories  of `<root>\data\`. This is important to set up the file structure for the code to run. 

## 4. Tips for BIM processing
>Geo-referencing
- Do not move the Project Base Point or Survey Point when creating a new Revit file
    - Do not attempt to add any georeferenced point in the physical model, except as a parameter of a property set
    - If the file has already shifted these points, you will need to manually reset them close to 0,0,0 and attempt to shift the model to that position
        - EITHER *create a new file, and copy and paste the contents as group*
            - You may need to transfer project standards under the Manage Tab
        - OR *move the content directly and altering the point coordinates*
- When converted into 3D Tiles format, Cesium does a poor job at handling these native georeference coordinates. The building model will likely not be found anywhere close to your ideal location
- It is easier to include geolocation information directly through Cesium 

>Material Display
- For exports to IFC, only materials viewable in `Visual Style: Shaded` will appear. Other material information is not retained
- Visit the `Material Browser` under the `Manage` tab -> `Materials`,
    - Check the following option if you want the visuals to look like the render: `Graphics` -> `Shading` -> `Use Render Appearance`
    - If not, do not check the option, and select your own color or transparency
- To create your own custom material, duplicate any existing material, rename the copy, unlink them to other copies, and change the options
- Have yet to test how materials using images can work, and might require additional files and complexity

>Classifying the furniture and building element appropriately
- Determine the purpose of an asset 
    - Picking individual asset to get their meta data in pop-ups
    - Merely a background element 
- Selecting individual assets' names
    - Their names must include the following supported words:
        - Sensors
        - Meters
        - Weather Station
        - Solar Panels
        - Fridge
- Ensure that the assets are classified as Furniture or Generic Models for the converter to recognise them
    - `Furniture` are exported as IfcFurnishingElement while `Generic Models` are exported as IfcBuildingElementProxy 
    - For new asset types, please include their name into `classify_file_name()` at `agent/ifc2gltf/kghelper.py`