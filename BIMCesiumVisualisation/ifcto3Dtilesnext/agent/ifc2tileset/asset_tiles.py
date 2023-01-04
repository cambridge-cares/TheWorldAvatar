"""
# Author: qhouyee #

This module provides methods to generate tilesets related to assets.
"""

# Self imports
from agent.ifc2tileset.root_tile import gen_root_content, bbox_child


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
                "name": {
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
    Recursively adds every i assets to a new child node of the required format.
    Function will only run if there are more than 6 assets.

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
    if len(assetlist) > i:
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
                'properties': {"name": nested_dict.get("name").split(":")[0],
                               "UID": uid}
            }
        })

    # Add the first 6 assets to the first children node of the tileset
    tileset['root']['children'][0]['contents'] = assetlist[0:6]

    # Add the remaining assets to the next nested child node of tileset
    appenddict_rec(tileset['root'], assetlist)
    return tileset
