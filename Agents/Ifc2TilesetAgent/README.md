# IFC2Tileset Agent

## Description
This agent processes an IFC file into the [3D Tiles Next](https://github.com/CesiumGS/3d-tiles/tree/main/next) specifications for visualisation in Cesium. It assumes that the IFC model has been preprocessed according to the [Tips for BIM processing](#4-tips-for-bim-processing) section. Tilesets will be generated in the output `data` directory.

A brief description of its workflow can be found below:
1. The geometries in the IFC file are split based on their individual assets. These are converted into glTF models that are stored locally.
2. During the splitting process, a mapping dictionary is generated that links the asset file names to their UID and names. 
3. THREE 3D Tilesets are generated for the building, solar panels, and sewerage network (if they exist). The mapping dictionary supplements the tileset with asset information (if any).

# Instructions
## 1. Building the agent
The agent is designed for deployment on [Docker](#12-docker-deployment). Although it can be deployed on a [local development environment](#11-python-virtual-environment-and-required-packages), this is not the recommended setup. 

### 1.1 Python virtual environment and required packages:
A brief explanation of the initial steps for local deployment has been included to highlight the three key dependencies. Local deployment of the agent on the `gunicorn` server and its usage is out of this document's scope.

1) Open `cmd` terminal and navigate into this folder (referred to as `<root>` in the following) 

2) Create virtual environment `<venv_name>` using venv (`python` command might need to be replaced with `py` or `python3` depending on whether Python is specified in system's `PATH` variables):
    ```
    // A virtual environment is recommended for easier dependency management
    python -m venv <venv_name>
    ``` 

3) Activate virtual environment by running:
    ```
    <venv_name>\Scripts\activate
    ```

4) Install requirements listed in `requirements.txt`:
    ```
    python -m pip install --upgrade pip  
    python -m pip install -r config/requirements.txt
    ```

5) Download the following resources into the required directory
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
docker compose -f "./config/docker-compose.test.yml" up -d --build 
```

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
Instructions for the agent and its various API endpoints can be found at the API root `http://localhost:5105/`. A brief overview is as follows:
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
There are four scripts available in the agent.
- `agent/app.py` is the entry point and calls developed functions to execute the conversion process
- `agent/utils.py` contain miscellaneous functions
- `agent/ifc2gltf.py` contain the functions that splits the ifc file into their individual assets and converts them into glTF models
    - Add additional IFC feature classes here if we encounter them
- `agent/ifc2tileset` sub-module contains functions to generate the tileset and write them to json
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
- Picking individual assets
    - Only the following types are supported in the converter:
        - Sensors
        - Meters
        - Weather Station
        - Solar Panels
        - Fridge
- Ensure that the assets are classified as Furniture or Generic Models for the converter to recognise them
    - `Furniture` are exported as IfcFurnishingElement while `Generic Models` are exported as IfcBuildingElementProxy 
    - For new asset types, please include their name (or part of) into line 60 of `agent/ifc2gltf.py`