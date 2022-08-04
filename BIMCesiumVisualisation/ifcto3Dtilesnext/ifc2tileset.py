# Standard library imports
import json
from pathlib import Path

# Third party imports

# Reader imports
import utils

def bbox_root():
    """
    Defines the bounding boxes required in generating the root tile for tilesets
    """
    # WIP: Difficulty in creating suitable bounding boxes automatically
    bbox_root= [    40, 0, 16, # x,y,z for center of model
                    100, 0, 0, # half-length for x
                    0, 100, 0, # half-length for y
                    0, 0, 20 ] # half-length for z
    return bbox_root

def bbox_child():
    """
    Defines the bounding boxes required in generating the children tiles for tilesets
    """
    # Theoretically, the x and y half lengths can remain the same to cover the entire floor, but z requires some reduction
    bbox_child= [   0, 25, 25, # x,y,z for center of model
                    50, 0, 0, # half-length for x
                    0, 25, 0, # half-length for y
                    0, 0, 5 ] # half-length for z
    return bbox_child

def root_tile():
    """
    Defines a skeleton template for all tilesets as a dictionary to write into the required json format
    """
    tileset= {'asset':{'version' : '1.1'},
            'geometricError' : 1024,
            'root' : {"boundingVolume" : {"box" : bbox_root()},
                "geometricError" : 512,
                "refine" : "ADD",
                }
            }
    return tileset

def gen_root_content():
    """
    Add the root content of building and background furniture to tileset
    If there are no assets, the tileset generated in this function is sufficient for visualisation
    """
    # Generate a minimal tileset
    tileset= root_tile()

    # Respective filepaths
    building_file_path="./data/gltf/ifcbuilding.gltf"
    bpath=Path(building_file_path)
    furniture_file_path="./data/gltf/ifcfurniture.gltf"
    fpath=Path(furniture_file_path)

    # In a special case where there is no building and furniture, no root content is added
    if bpath.is_file():
        if fpath.is_file():
            rootlist=[{"uri" : "./gltf/ifcbuilding.gltf"}, {"uri" : "./gltf/ifcfurniture.gltf"}]
            # Tileset Nomenclature for multiple geometry files = contents:[{}]
            tileset["root"]["contents"] = rootlist
        else: 
            # Tileset Nomenclature for 1 geometry file = content:{}
            tileset["root"]["content"] = {"uri" : "./gltf/ifcbuilding.gltf"}
    
    return tileset

def init_asset_tiles():
    """
    Initialise the tileset to receive asset information
    """
    # Generate a tileset with root content added
    tileset= gen_root_content()
    
    # Add new contents to the tileset
    tileset["schema"]= {'classes':{
        'AssetMetaData': {
            'name' : "Asset metadata",
            'description' : "A metadata class for all individual assets",
            # Store all asset information here even if they are not used for specific assets
            'properties' : {
                "Asset Name":{
                    "description" : "Name of the asset",
                    "type" : "STRING"
                    },
                "UID":{
                    "description" : "Unique identifier generated in IFC",
                    "type" : "STRING"
                    }
                }
            }
        }}

    tileset["root"]["children"]=[{
        "boundingVolume" : {"box" :bbox_child()},
        "geometricError" : 50,
        # Nomenclature for 1 geometry file = content:{}, multiple files = contents:[{}]
        "contents" :[{}]
        # CesiumJS only supports six content by default: https://github.com/CesiumGS/cesium/issues/10468
        # If we have more than six assets, do add more inner children contents here
        }]
    return tileset

def appenddict_rec(tileset_root, assetlist, i=6):
    """
    Recursively adds every i assets to a new child node of the required format

    Arguments:
        tileset_root - a nested dictionary starting from tileset['root']
        assetlist - a list of nested dictionary to add duplicate keys
        i - an integer denoting number of assets to add per child. Set to 6 as default as that is the current limit

    CAVEAT: Functionality for visualising tilesets with >10 child nodes in Cesium has yet to be tested
    If the code fails to work for more child nodes, DO NOT change this code
    Edit the asset2tileset function instead to stop when above the max limit of child nodes
    """
    if i<len(assetlist):
        tileset_root['children'][0]['children']=[{
            "boundingVolume" : {"box" :bbox_child()},
            "geometricError" : 50,
            "contents" :assetlist[i:i+6]
            }]
        appenddict_rec(tileset_root['children'][0], assetlist, i+6)
    else: 
        return

def gen_tileset_assets(dataframe):
    """
    Add asset properties into the tileset 
    """
    # Initialise tileset structure for assets
    tileset= init_asset_tiles()
    
    # A Python list is required to handle duplicate keys in a nested dictionary
    assetlist=[]

    # Extract unique rows of geomfile for each asset
    df_unique=dataframe.drop_duplicates(subset = ["geomfile"])

    # Add geometry and names for each asset
    for row in df_unique.index:
        assetlist.append({
            'uri': df_unique['geomfile'][row], 
            # Add the asset name to establish a metadata skeleton
            'metadata': {
                    'class': "AssetMetaData",
                    'properties': {"Asset Name": df_unique['name'][row].split(":")[0], "UID":df_unique['uid'][row]}
            }
        })
    
    # If there are properties in the IFC file
    if 'paramtitle' in dataframe.columns:
        # Remove rows that contains "HasAdditionalDataSource"
        # This parameter is used for querying and should not be visualised
        dataframe=dataframe[~dataframe.paramtitle.str.contains("HasAdditionalDataSource")]
        
        # Add the properties to their assets
        for row in dataframe.index:
            # If parameters exist for that asset
            if dataframe['paramtitle'][row]!="":
                # Find the index containing the right asset as the structure is all the same {'uri':'xxx', 'metadata':'xxx'}
                list_ind=utils.dictfind(assetlist,'uri',dataframe['geomfile'][row])
                
                # If property is a string
                if "String" in dataframe['hastype'][row]:
                    # Add property names and types
                    tileset["schema"]["classes"]['AssetMetaData']['properties'][dataframe['paramtitle'][row]]={"type":"STRING"}
                    # Add property values
                    assetlist[list_ind]['metadata']['properties'][dataframe['paramtitle'][row]]= dataframe['paramvalue'][row]
                    
                # If property is an integer, add values as a Python integer (Not NUMPY)
                if "Integer" in dataframe['hastype'][row]:
                    tileset["schema"]["classes"]['AssetMetaData']['properties'][dataframe['paramtitle'][row]]={"type":"SCALAR","componentType" : "UINT8"}
                    assetlist[list_ind]['metadata']['properties'][dataframe['paramtitle'][row]]= int(dataframe['paramvalue'][row])
                    
    # Add the first 6 assets to the first children node of the tileset
    tileset['root']['children'][0]['contents'] = assetlist[0:6]
    
    # Add the remaining assets to the next nested child node of tileset
    appenddict_rec(tileset['root'], assetlist)

    return tileset

def jsonwriter(tileset, tileset_string):
    """
    Writes the python dictionary into 3D Tiles Next in JSON format

    Arguments:
            tileset - a python dictionary formatted for 3D Tiles
            tileset_string - a string containing the output tileset's name
    """
    json_string = json.dumps(tileset)
    with open('data/'+tileset_string+'.json', 'w') as outfile:
            outfile.write(json_string)
    print(tileset_string+".json have been generated")  

def gen_ceiling_tileset():
    """
    Generates and write the tileset for ceiling into 3D Tiles Next format
    """
    tileset= root_tile()
    tileset["root"]["content"] = {"uri" : "./gltf/ifcceiling.gltf"}

    jsonwriter(tileset,"tileset_ceiling")

def gen_tilesets(dataframe):
    """
    Generates the tileset in json format
    
    Argument:
        dataframe - the query results for assets
    """
    # Create and write a separate tileset for ceiling
    gen_ceiling_tileset()
    
    # If there is no assets
    if dataframe.empty:
        jsonwriter(gen_root_content(),"tileset_bim")
    else:
        jsonwriter(gen_tileset_assets(dataframe),"tileset_bim")