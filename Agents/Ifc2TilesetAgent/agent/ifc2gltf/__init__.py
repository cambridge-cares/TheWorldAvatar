"""
# Author: qhouyee #

This is the submodule's entry function
to split and convert the IFC model to glTF.
"""
# Third party imports
from py4jps import agentlogging

# Self imports
from agent.ifc2gltf.kghelper import retrieve_metadata, get_building_iri
from agent.ifc2gltf.ifchelper import gendict4split, append_ifcconvert_command
from agent.utils import run_shellcommand, retrieve_abs_filepath

# Retrieve logger
logger = agentlogging.get_logger("dev")


def conv2gltf(input_ifc: str, query_endpoint: str, update_endpoint: str):
    """
    Invokes the related external tools to convert an IFC file
    to the glTF format in the gltf subfolder

    Arguments:
        input_ifc - Local file path of ifc model
        query_endpoint - SPARQL QUERY endpoint
        update_endpoint - SPARQL UPDATE endpoint
    Returns:
        - A dataframe containing the individual assets and their metadata
        - The data IRI of the building
    """
    logger.info("Retrieving metadata from endpoint...")
    metadata = retrieve_metadata(query_endpoint, update_endpoint)
    building_iri = get_building_iri(query_endpoint, update_endpoint)

    logger.info("Generating dictionary for splitting IFC assets.")
    dict_for_split, asset_data = gendict4split(metadata)

    for key, value_list in dict_for_split.items():
        glbpath = "./data/glb/" + key + ".glb"
        glbpath = retrieve_abs_filepath(glbpath)
        gltfpath = "./data/gltf/" + key + ".gltf"
        gltfpath = retrieve_abs_filepath(gltfpath)

        # Initialise the commands and append accordingly
        ifcconvert_command = ["./IfcConvert", "-q", input_ifc, glbpath]
        ifcconvert_command = append_ifcconvert_command(
            key, value_list, ifcconvert_command)
        glb2gltf_command = "gltf-pipeline -i " + glbpath + " -o " + gltfpath

        # Convert from IFC -> glb -> glTF
        logger.info("Converting " + key + " to glTF...")
        run_shellcommand(ifcconvert_command)
        logger.debug("IfcConvert command executed: " + " ".join(ifcconvert_command))
        run_shellcommand(glb2gltf_command, True)
        logger.debug("gltf-pipeline Command executed: " + " ".join(glb2gltf_command))

    logger.info("Conversion to gltf completed...")

    return asset_data, building_iri
