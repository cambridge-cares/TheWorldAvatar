# IFC Conversion to 3D Tiles Next

## Description
This conversion tool processes an IFC file through the knowledge graph into the [3D Tiles Next](https://github.com/CesiumGS/3d-tiles/tree/main/next) specifications for visualisation in Cesium. It assumes that the IFC model has been preprocessed according to the [Tips for BIM processing](#tips-for-bim-processing) section, and that a Blazegraph server can be run locally. 

A brief description of its workflow can be found below:
1. The IFC file is converted into a TTL file and stored in a knowledge graph
2. The geometries in the IFC file are split into their individual assets and then, converted into the glTF format and stored as local data files
3. TTL triples linking the asset to their local glTF file path and equivalent instances in other ontologies are inserted into the related namespace in the knowledge graph 
4. The semantic information and local glTF file path is queried from the knowledge graph endpoint to generate a 3D tileset in JSON format

# Instructions
## Pre-Requisite
### 1. Create a Python virtual environment and install required packages (in Windows):
1) Open `cmd` terminal and navigate into the `ifcto3Dtilesnext` folder (referred to as `<root>` in the following) 

2) Create virtual environment `<venv_name>` using venv (`python` command might need to be replaced with `py` or `python3` depending on whether Python is specified in system's `PATH` variables):
    ```
    python -m venv <venv_name>
    ``` 

3) Activate virtual environment by running:
    ```
    <venv_name>\Scripts\activate
    ```

4) Install requirements listed in `requirements.txt`:
    ```
    python -m pip install --upgrade pip  
    python -m pip install -r requirements.txt
    ```

5) Download resources in cmd from `<root>` (Change the `<venv>` on line 58 to `<venv_name>`):
     ```
    python downloadresource.py
    ```
    > Tested only for Windows operating system

6) Check that the resources are placed in the right directory
    - `<venv_name>\Lib\site-packages`    
        1. **IfcOpenShell**
            - Required to load and parse IFC files
        2. **IfcPatch**
            - Required to split IFC into individual asset IFC files
        - Download instructions:
            - Download required version from https://github.com/IfcOpenShell/IfcOpenShell/releases
            - Extract and place the `ifcpatch` and `ifcopenshell` from `blenderbim/libs/site/packages/`
            - Delete the remaining extracted content
    
    - `<root>\resources`
        1. IfcConvert.exe
            - Required to convert IFC to DAE format
            - If not, download the IfcConvert.exe from: http://ifcopenshell.org/ifcconvert
        2. COLLADA2GLTF folder
            - Required to convert DAE to glTF format
            - If not, download and unzip the zip folder for your OS under the release section from: https://github.com/KhronosGroup/COLLADA2GLTF
        3. IFCtoRDF-0.4-shaded.jar
            - Required to convert IFC to TTL format
            - If not, download the latest release of the shaded executable JAR archive from https://github.com/pipauwel/IFCtoRDF/releases
        4. Blazegraph.jar
            - No configuration is required as this workflow will automate the upload and query requirements
            - If not, download the latest release **BUT NOT Release Candidate** of blazegraph.jar from the Release section on https://github.com/blazegraph/database
7) If any of the Ifc assets have additional semantic or dynamic data sources eg postgreSQL for timeseries, please add in a custom parameter/value "HasAdditionalDataSource: Yes" in BIM for that asset following the [custom parameters](#tips-for-bim-processing) section
    - The current workflow will extract the relevant data from the required sources
    - Note that this approach/code will need to be refactored once there is more data sources
    - If the timeseries query code has errors with `JPSBaseLib` not found, please follow the instructions [here](https://github.com/cambridge-cares/TheWorldAvatar/tree/main/JPS_BASE_LIB/python_wrapper) to install the `JPSBaseLib` resource in `py4jps` package

## Usage
1. Place the IFC file in `<root>\data\ifc`
2. Configurations
    - If required, you may rename the namespace and kg server at the `config.properties` file (`<root>\properties`)
    - By default, this module will delete any `<root>\blazegraph.jnl` file
        - This can be configured in `utils.py` 
        - If you wish to keep a local `blazegraph.jnl` with existing data, ensure that it is not in the `<root>` directory or subdirectories.
    - If accessing a different PostgreSQL database, please change endpoint in `tsclient.properties` 
3. Open `cmd` terminal and navigate into the `<root>`
4. Please run the following commands in the `cmd` terminal (from within the `<root>` directory and with **activated** virtual environment <venv_name>)
    - Please uncomment the `timeseriesquery()` in line 34 if your model require time series.
    ```
    python main.py
    ```

## For Developers
There are six modules available in the sub-directory.
- `main.py` imports the developed function to execute the conversion process
- `utils.py` contain miscellaneous functions and global variables
- `ifc2gltf.py` contain the functions that splits the ifc file into their individual assets and converts them into glTF format
    - Add additional IFC feature classes here if we encounter them
- `ifc2kg` directory contain the modules for interacting with the knowledge graph
    - `ttlutil.py` contains the general functions to convert ifc to ttl, query triples, and stores their result in a dataframe
    - `insertquery.py` contains the functions to generate and insert the required triples into the blazegraph
        - Edit the geometry file path once we have a clearer idea on the visualisation folder structure
    - `timeseriesquery.py` contain the function to query for timeseries data
    - `jpsSingletons.py` imports required Java packages from `The World Avatar` repository for Python 
- `ifc2tileset.py` contain the functions to generate the tileset and write them to json
    - If you are unable to see the assets, **Modify the bounding box coordinates** according to your use case
- `downloadresource.py` downloads the required external resources for Windows

As Git does not allow empty directories, `.gitignore` files have been added to the subdirectories  of `<root>\data\`. This is important to set up the file structure for the code to run. 

## Tips for BIM processing
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
- Classify the former as a `Furniture` and the latter as a `Generic Model`
    - In the rfa file: Modify -> Family category and parameters -> Furniture / Generic Model
    - If your family is already loaded into a project, click on the asset in Revit and under the **Modify** tab at top right -> **Edit Family** -> Above step -> Under **Create Tab** -> **Load into Project**
    - Add each `Furniture` asset as an individual & independent instance into Revit, that is not grouped or linked with other assets
- `Furniture` are exported as IfcFurnishingElement while `Generic Models` are exported as IfcBuildingElementProxy 

>#### Creating custom property sets and parameters
- If there is a need to use custom parameters, follow the steps below
    1. Select the object and go to **Edit Family** under `Modify` tab
    2. `Create` tab -> Properties -> Family Types
    3. In the **Family Types** window, select `New Parameter` in the bottom left corner
    4. In **Parameter Properties** window, select it as a `Family parameter`. A custom tooltip can also be assigned
        - Ensure that you check the **`Instance`** option over `Type`
            - `Instance` ensures that each individual instance of the family has their own value
        - Placed it under Others for your convenience
    5. Once done, input values into the new custom parameter in the **Family Types** window
    6. For assets with additional data sources (not in IFC), add "HasAdditionalDataSource" parameter with "Yes" value to ease conversion workflow
- For the parameters (custom or native) that you are interested to export into an IFC file, follow the steps below
    1. In the export to IFC settings under `Property Sets` tab, checked the user defined property sets
    2. It will browse to a default mapping table, which will guide you on creating one. If unavailable, here is a brief structure/format  
    PropertySet: (Pset Name) I[nstance]/T[ype] (element list separated by ',')  
    (Property Name 1) (Data Type) ([opt] Revit parameter name, if different from IFC)  
    (Property Name 2) (Data Type) ([opt] Revit parameter name, if different from IFC)  
        - Tentatively use I over T
        - Example from https://knowledge.autodesk.com/search-result/caas/simplecontent/content/export-custom-bim-standards-and-property-sets-to-ifc.html  
    3. Rename PropertySet to `Pset_Asset` available in IFC2x3 to facilitate IfcOWL conversion
        ```
        PropertySet: ` Pset_Asset` I IfcWall,IfcDoor  
        Phase Text Phase_erstellt  
        Painted Boolean(Yes/No)
        ```