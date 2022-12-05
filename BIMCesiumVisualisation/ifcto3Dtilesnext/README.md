# IFC Conversion to 3D Tiles Next

## Description
This conversion tool processes an IFC file into the [3D Tiles Next](https://github.com/CesiumGS/3d-tiles/tree/main/next) specifications for visualisation in Cesium. It assumes that the IFC model has been preprocessed according to the [Tips for BIM processing](#tips-for-bim-processing) section. 

A brief description of its workflow can be found below:
1. The geometries in the IFC file are split based on their individual assets. These are converted into glTF models that are stored locally.
2. During the splitting process, a mapping dictionary is generated that links the asset file names to their UID and names. 
3. THREE 3D Tilesets are generated for the building, solar panels, and sewerage network (if they exist). The mapping dictionary supplements the tileset with asset information (if any).

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
    python -m pip install -r config/requirements.txt
    ```

5) Download resources in cmd from `<root>` (Change the `<venv>` on line 58 to `<venv_name>`):
     ```
    python config/downloadresource.py
    ```
    > Tested only for Windows operating system

6) Check that the resources are placed in the right directory
    - `<venv_name>\Lib\site-packages`    
        1. **IfcOpenShell**
            - Required to load and parse IFC files
        - Download instructions:
            - Download required version from https://github.com/IfcOpenShell/IfcOpenShell/releases
            - Extract and place the `ifcopenshell` from `blenderbim/libs/site/packages/`
            - Delete the remaining extracted content
    
    - `<root>\agent\resources`
        1. IfcConvert.exe
            - Required to convert IFC to glb format
            - If not, download the IfcConvert.exe from: https://blenderbim.org/docs-python/ifcconvert/installation.html
    - `npm global location`
        2. gltf-pipeline NPM package
            - Required to convert glb to glTF format
            - Read documentation from https://github.com/CesiumGS/gltf-pipeline
            - If not, install the library globally using `npm install -g gltf-pipeline`

## Usage
### 1. Running Tests
1. Open `cmd` terminal and navigate into the `<root>` containing this README
2. Please run the following commands in the `cmd` terminal (from within the `<root>` directory and with **activated** virtual environment <venv_name>)
    ```
    python pytest
    ```
### 2. Running tool
1. Place the IFC file in `<root>\data\ifc`
2. Open `cmd` terminal and navigate into the `<root>`
3. Please run the following commands in the `cmd` terminal (from within the `<root>` directory and with **activated** virtual environment <venv_name>)
    ```
    python agent
    ```

## For Developers
There are five modules available in the sub-directory.
- `agent` imports the developed functions to execute the conversion process
- `agent/utils.py` contain miscellaneous functions
- `agent/ifc2gltf.py` contain the functions that splits the ifc file into their individual assets and converts them into glTF models
    - Add additional IFC feature classes here if we encounter them
- `agent/ifc2tileset` directory contains submodules with functions to generate the tileset and write them to json
    - If you are unable to see the assets, **Modify the bounding box coordinates** according to your use case
- `config/downloadresource.py` downloads the required external resources for the Windows OS

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
- Picking individual assets
    - Only the following types are supported in the converter:
        - Sensors
        - Meters
        - Weather Station
        - Solar Panels
        - Fridge
- Ensure that the assets are classified as Furniture or Generic Models for the converter to recognise them
    - `Furniture` are exported as IfcFurnishingElement while `Generic Models` are exported as IfcBuildingElementProxy 