"""
# Author: qhouyee #

This module generates the tilesets for the IFC file and its glTF models.
"""

# Standard library imports
import json
from pathlib import Path


def bbox_root():
    """
    Defines the bounding boxes required in generating the root tile for tilesets

    Returns:
    The bounding box coordinates for the root tile
    """
    # WIP: Difficulty in creating suitable bounding boxes automatically
    bbox = [40, 0, 16,  # x,y,z for center of model
            100, 0, 0,  # half-length for x
            0, 100, 0,  # half-length for y
            0, 0, 20]  # half-length for z
    return bbox


def bbox_child():
    """
    Defines the bounding boxes required in generating the children tiles for tilesets

    Returns:
    The bounding box coordinates for children tiles
    """
    # Theoretically, the x and y half lengths can remain the same to
    # cover the entire floor, but z requires some reduction
    bbox = [0, 25, 25,  # x,y,z for center of model
            50, 0, 0,  # half-length for x
            0, 25, 0,  # half-length for y
            0, 0, 5]  # half-length for z
    return bbox


def root_tile():
    """
    Defines a skeleton template for all tilesets as a dictionary
    to write into the required json format

    Returns:
    The root tileset generated as a python dictionary
    """
    tileset = {'asset': {'version': '1.1'},
               'geometricError': 1024,
               'root': {"boundingVolume": {"box": bbox_root()},
                        "geometricError": 512,
                        "refine": "ADD",
                        }
               }
    return tileset


def gen_root_content():
    """
    Add the root content of building and background furniture to tileset
    If there are no assets, the tileset generated in this function is sufficient for visualisation

    Returns:
    The tileset generated as a python dictionary
    """
    # Generate a minimal tileset
    tileset = root_tile()

    # Respective filepaths
    building_file_path = "./data/gltf/building.gltf"
    bpath = Path(building_file_path)
    furniture_file_path = "./data/gltf/furniture.gltf"
    fpath = Path(furniture_file_path)

    # In a special case where there is no building and furniture, no root content is added
    if bpath.is_file():
        rootlist = []
        if fpath.is_file():
            rootlist += [{"uri": "./gltf/furniture.gltf"}]
        # If there are furniture, use the multiple nomenclature
        if rootlist:
            rootlist += [{"uri": "./gltf/building.gltf"}]
            # Tileset Nomenclature for multiple geometry files = contents:[{}]
            tileset["root"]["contents"] = rootlist
        else:
            # Tileset Nomenclature for 1 geometry file = content:{}
            tileset["root"]["content"] = {"uri": "./gltf/building.gltf"}
    return tileset


def init_asset_tiles():
    """
    Initialise the tileset to receive asset information

    Returns:
    The tileset generated with initialised asset metadata as a python dictionary
    """
    # Generate a tileset with root content added
    tileset = gen_root_content()

    # Add new contents to the tileset
    tileset["schema"] = {'classes': {
        'AssetMetaData': {
            'name': "Asset metadata",
            'description': "A metadata class for all individual assets",
            # Store all asset information here even if they are not used for specific assets
            'properties': {
                "Asset Name": {
                    "description": "Name of the asset",
                    "type": "STRING"
                },
                "UID": {
                    "description": "Unique identifier generated in IFC",
                    "type": "STRING"
                }
            }
        }
    }}

    tileset["root"]["children"] = [{
        "boundingVolume": {"box": bbox_child()},
        "geometricError": 50,
        # Nomenclature for 1 geometry file = content:{}, multiple files = contents:[{}]
        "contents": [{}]
        # CesiumJS only supports six content by default:
        # https://github.com/CesiumGS/cesium/issues/10468
        # If we have more than six assets, do add more inner children contents here
    }]
    return tileset


def appenddict_rec(tileset_root, assetlist, i=6):
    """
    Recursively adds every i assets to a new child node of the required format

    Arguments:
        tileset_root - a nested dictionary starting from tileset['root']
        assetlist - a list of nested dictionary to add duplicate keys
        i - an integer denoting number of assets to add per child.
            Set to 6 as default as that is the current limit

    CAVEAT: Functionality for visualising tilesets with >10 child nodes
    in Cesium has yet to be tested. If the code fails to work for more child nodes,
    DO NOT change this code.
    Edit the asset2tileset function instead to stop when above the max limit of child nodes
    """
    if i < len(assetlist):
        tileset_root['children'][0]['children'] = [{
            "boundingVolume": {"box": bbox_child()},
            "geometricError": 50,
            "contents": assetlist[i:i+6]
        }]
        appenddict_rec(tileset_root['children'][0], assetlist, i+6)
    else:
        return


def gen_tileset_assets(hashmapping):
    """
    Add asset properties into the tileset.

    Returns:
    The tileset generated with asset metadata as a python dictionary
    """
    # Initialise tileset structure for assets
    tileset = init_asset_tiles()

    # A Python list is required to handle duplicate keys in a nested dictionary
    assetlist = []
    # Add geometry and uid for each asset
    for uid, nested_dict in hashmapping.items():
        assetlist.append({
            'uri': "./gltf/"+nested_dict.get("file")+".gltf",
            # Add the asset name to establish a metadata skeleton
            'metadata': {
                'class': "AssetMetaData",
                'properties': {"Asset Name": nested_dict.get("name").split(":")[0],
                               "UID": uid}
            }
        })

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
    with open('data/'+tileset_string+'.json', 'w', encoding="utf-8") as outfile:
        outfile.write(json_string)
    print(tileset_string+".json have been generated")


def gen_solarpanel_tileset():
    """
    Generates and write the tileset for solar panel into 3D Tiles Next format if it exists
    """
    solarpanel_file_path = "./data/gltf/solarpanel.gltf"
    solarpath = Path(solarpanel_file_path)
    if solarpath.is_file():
        tileset = root_tile()
        tileset["root"]["content"] = {"uri": "./gltf/solarpanel.gltf"}
        jsonwriter(tileset, "tileset_solarpanel")


def gen_tilesets(hashmapping):
    """
    Generates the tileset in json format

    Argument:
        hashmappings - A hashtable to match assets to their IFC ID
    """
    # Create and write a separate tileset for solar panels
    gen_solarpanel_tileset()

    # If there are assets, generate tilesets with asset information
    if hashmapping:
        jsonwriter(gen_tileset_assets(hashmapping), "tileset_bim")
    # Else, generate only the root building contents
    else:
        jsonwriter(gen_root_content(), "tileset_bim")
