# Standard library imports
import subprocess 

# Third party imports
import ifcopenshell.util
from ifcopenshell.util.selector import Selector
import ifcpatch

def add_feature_to_dict(filename, feature, dict, ifc):
    """
    Extract the IFC classes in the required dictionary format {filename : query_syntax} if their features exist

    Arguments:
        filename - A string containing the file name of the output 
        feature - A string of the IfcClass to add into the dictionary
        dict - A dictionary
        ifc - The required IFC model loaded in ifcopenshell
    """
    # selector is a utility tool from  ifcopenshell
    selector = Selector()

    if len(selector.parse(ifc, feature)) > 0:        
        # If dictionary is empty, add a key with no values
        if filename not in dict:
            dict[filename] = ""
        
        # If the feature is the first value added, the format would be different than the second and subsequent values
        # Sample query syntax = '.IfcWall| .IfcDoor |.IfcWindow'
        if len(dict[filename])==0: 
            dict[filename]= feature
        else:
            dict[filename]+= ' | ' + feature

def gendict4split(ifc):
    """
    Creates a dictionary {filename : query_syntax} to split the IFC model into smaller IFC files

    Arguments:
        ifc - The required IFC model loaded in ifcopenshell

    Returns:
        The required dictionary for splitting the IFC model
        A hashtable to match assets to their IFC ID
    """
    dict_elements = {}
    # Adding the available features of the building model's broader categories
    # Non-exhaustive. If required, add more classes in the format '.IfcFeatureType'
    for feature in ['.IfcWall', '.IfcWallStandardCase', '.IfcSlab[PredefinedType!="ROOF"]', '.IfcColumn', '.IfcDoor', '.IfcWindow', '.IfcCurtainWall', '.IfcPlate', '.IfcMember', '.IfcStair', '.IfcStairFlight', '.IfcRailing']:
        add_feature_to_dict("ifcbuilding", feature, dict_elements, ifc)
    for feature in ['.IfcSlab[PredefinedType="ROOF"]', '.IfcCovering']:
        add_feature_to_dict("ifcceiling", feature, dict_elements, ifc)
    for feature in ['.IfcBuildingElementProxy', '.IfcFlowTerminal']:
        add_feature_to_dict("ifcfurniture", feature, dict_elements, ifc)

    hashtable={}
    i=1
    # Adding the key : value pair for each asset into the dictionary
    for element in ifc.by_type("IfcFurnishingElement"):
        # Keep the name of asset files simple, as Cesium cannot load too complicated and long file names
        dict_elements["asset"+str(i)] = '.IfcFurnishingElement' + '[GlobalId ="' + element.GlobalId + '"]'
        # Create a hashtable dict to store the lookup values
        hashtable[element.GlobalId] = "asset"+str(i)
        i= i+1
    return dict_elements, hashtable

def conv2gltf(ifc, input_ifc):
    """
    Invokes the related external tools to split a IFC file and convert to the glTF format in the gltf subfolder
    
    Arguments:
        ifc - The required IFC model loaded in ifcopenshell
        input_ifc - Local file path of ifc model

    Returns:
        A hashtable linking each IFC id with their glTF file name
    """
    dict, hashtable = gendict4split(ifc) 
    for key, value in dict.items(): 
        splitpath = "./data/splitifc/" + key +".ifc"
        logfile = "./data/log/ifcpatch_" + key + ".log"
        daepath = "./data/dae/" + key +".dae"
        gltfpath = "./data/gltf/" + key +".gltf"
        
        #IfcSplitting
        ifcpatch.execute({
            "input": input_ifc,
            "output": splitpath,
            "recipe": "ExtractElements",
            "log": logfile,
            "arguments": [value],
        })

        # Convert from IFC to DAE to glTF
        ifc2dae = subprocess.run([".\\resources\IfcConvert", splitpath, daepath])
        dae2gltf = subprocess.run([".\\resources\COLLADA2GLTF\COLLADA2GLTF-bin", "-i", daepath, "-o", gltfpath])

    print("Conversion to gltf completed...")
    return hashtable
