# Standard library imports
import json
from pathlib import Path

# Third party imports

# Reader imports
import utils

def bbox():
        """
        Defines the bounding boxes required in generating the tilesets
        """
        # WIP: Difficulty in creating suitable bounding boxes automatically
        bbox_root= [    40, 0, 16, # x,y,z for center of model
                        100, 0, 0, # half-length for x
                        0, 100, 0, # half-length for y
                        0, 0, 20 ] # half-length for z
        # Theoretically, the x and y half lengths can remain the same to cover the entire floor, but z requires some reduction
        bbox_child= [   0, 25, 25, # x,y,z for center of model
                        50, 0, 0, # half-length for x
                        0, 25, 0, # half-length for y
                        0, 0, 5 ] # half-length for z
        return bbox_root, bbox_child

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

def genbuilding_ceiling_tileset():
        """
        Generate and write the tileset for the building structure and ceiling into the 3D Tiles Next format
        """
        #Generate bounding boxes
        bbox_root, bbox_child=bbox()

        # First tileset for the building structure and fixtures
        tileset_building= {'asset':{'version' : '1.1'},
                'geometricError' : 1024,
                'root' : {"boundingVolume" : { "box" : bbox_root},
                        "geometricError" : 512,
                        "refine" : "ADD",
                        "content":{
                                "uri" : "./gltf/ifcbuilding.gltf"
                                }
                        }
                }

        # Adding the background furniture as a child content if they exist
        file_path="./data/gltf/ifcfurniture.gltf"
        path=Path(file_path)
        if path.is_file():
                tileset_building['root']['children']=[{
                        "boundingVolume" : {"box" :bbox_child},
                        "geometricError" : 50,
                        # Nomenclature for 1 geometry file = content:{}, multiple files = contents:[{}]
                        "content" :{"uri" : "./gltf/ifcfurniture.gltf"}
                        }]

        # Second tileset for the ceiling and covering to toggle hiding and unhiding options
        tileset_ceiling= {'asset':{'version' : '1.1'},
                'geometricError' : 1024,
                'root' : {"boundingVolume" : { "box" : bbox_root},
                        "geometricError" : 512,
                        "refine" : "ADD",
                        "content":{
                                "uri" : "./gltf/ifcceiling.gltf"
                                }
                        }
                }
        
        jsonwriter(tileset_building,"tileset_building")
        jsonwriter(tileset_ceiling,"tileset_ceiling")


def filterdf(dataframe, findtext, bool=True):
        """
        Currently not in use but may be useful when we have more assets that cannot be placed in one tileset
        This function filters a dataframe based on a substring of an asset's names. 
        It includes an optional argument when we wish to find the inverse of a keyword, which can be set to False.
        If we use parenthesis(), add a \ to prevent regex error as parenthesis are special characters
                Eg: df1= ts.filterdf(df,'Fume Hood \(Cabinets\)')
                Eg: df2= ts.filterdf(df,'Fume Hood', False)
        """
        return dataframe[dataframe['name'].str.contains(findtext)== bool]


def appenddict_rec(tileset_root, assetlist, bbox_child, i=6):
        """
        Recursively adds every i assets to a new child node of the required format

        Arguments:
                tileset_root - a nested dictionary starting from tileset['root']
                assetlist - a list of nested dictionary to add duplicate keys
                bbox_child - the bounding box for a child node
                i - an integer denoting number of assets to add per child. Set to 6 as default as that is the current limit

        CAVEAT: Functionality for visualising tilesets with >10 child nodes in Cesium has yet to be tested
        If the code fails to work for more child nodes, DO NOT change this code
        Edit the asset2tileset function instead to stop when above the max limit of child nodes
        """
        if i<len(assetlist):
                tileset_root['children'][0]['children']=[{
                                "boundingVolume" : {"box" :bbox_child},
                                "geometricError" : 50,
                                "contents" :assetlist[i:i+6]
                                }]
                appenddict_rec(tileset_root['children'][0], assetlist, bbox_child, i+6)
        else: 
                return 

def asset2tileset(dataframe):
        """
        Add geometry and meta data originating from BIM for all assets
        
        Returns:
                A tileset dictionary
        """
        bbox_root, bbox_child=bbox()
        
        # Extract unique rows of geomfile for each asset
        df_unique=dataframe.drop_duplicates(subset = ["geomfile"])

        # Setting up a skeleton structure
        tileset= {'asset':{'version' : '1.1'},
                'schema':{'classes':{
                        'AssetMetaData': {
                                'name' : "Asset metadata",
                                'description' : "A metadata class for all individual assets",
                                # Store all asset information here even if they are not used for specific assets
                                'properties' : {
                                        "Asset Name":{
                                                "description" : "Name of the asset",
                                                "type" : "STRING"
                                                }
                                        }
                                }
                        }
                },
                'geometricError' : 1024,
                'root' : {"boundingVolume" : {"box" : bbox_root},
                        "geometricError" : 512,
                        "refine" : "ADD",
                        "children" :[{
                                "boundingVolume" : {"box" :bbox_child},
                                "geometricError" : 50,
                                # Nomenclature for 1 geometry file = content:{}, multiple files = contents:[{}]
                                "contents" :[{}]
                                # Add another set of children if there are more than six assets
                                }]
                        }
                }
        # Build a list of nested dictionary to facilitate duplicate keys
        assetlist=[]

        # Add geometry and names for each asset
        for row in df_unique.index:
                assetlist.append({
                        'uri': df_unique['geomfile'][row], 
                        # Add the asset name to establish a metadata skeleton
                        'metadata': {
                                'class': "AssetMetaData",
                                'properties': {"Asset Name": df_unique['name'][row]}
                        }
                })
        
        #If there are properties added in the IFC file
        if 'paramtitle' in dataframe.columns:
                # Add the properties to their assets
                for row in dataframe.index:
                        # If there are property values, run the following code
                        if dataframe['paramtitle'][row]!="":
                                # Find the index containing the right asset as the structure is all the same {'uri':'xxx', 'metadata':'xxx'}
                                list_ind=utils.dictfind(assetlist,'uri',dataframe['geomfile'][row])
                                # If property is a string, 
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
        appenddict_rec(tileset['root'], assetlist, bbox_child)

        return tileset

