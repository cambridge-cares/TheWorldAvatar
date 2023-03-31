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
    """Converts an IFC file to the glb format in the glb subfolder.

    Args:
        input_ifc: Local file path to an IFC model.
        query_endpoint: SPARQL QUERY endpoint.
        update_endpoint: SPARQL UPDATE endpoint.

    Returns:
        A tuple (asset_df, building_iri), where asset_df is a dataframe containing the individual assets and their
        metadata with headers 'file', 'name', 'uid', 'iri', and building_iri is the data IRI of the building.
    """
    logger.info("Retrieving metadata from endpoint...")
    metadata = retrieve_metadata(query_endpoint, update_endpoint)
    building_iri = get_building_iri(query_endpoint, update_endpoint)

    logger.info("Generating dictionary for splitting IFC assets.")
    dict_for_split, asset_data = gendict4split(metadata)

    for key, value_list in dict_for_split.items():
        glbpath = "./data/glb/" + key + ".glb"
        glbpath = retrieve_abs_filepath(glbpath)

        # Initialise the commands and append accordingly
        ifcconvert_command = ["./IfcConvert", "-q", input_ifc, glbpath]
        ifcconvert_command = append_ifcconvert_command(key, value_list, ifcconvert_command)

        # Convert from IFC -> glb -> glTF
        logger.info("Converting " + key + " to glTF...")
        run_shellcommand(ifcconvert_command)

    logger.info("Conversion to gltf completed...")

    return asset_data, building_iri
