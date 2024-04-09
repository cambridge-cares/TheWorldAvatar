"""
# Author: qhouyee, picas9dan #

This is the submodule's entry function
to split and convert the IFC model to geometry outputs.
"""

# Third-party imports
from py4jps import agentlogging

# Self imports
from agent.ifc2gltf.kghelper import retrieve_metadata, get_building_iri_name
from agent.ifc2gltf.ifchelper import exec_gltf_conversion, get_filename_to_ifc_ids_mapping

# Retrieve logger
logger = agentlogging.get_logger("dev")


def conv2gltf(input_ifc: str, query_endpoint: str, update_endpoint: str):
    """Converts an IFC file to the glb format in the glb subfolder.

    Args:
        input_ifc: Local file path to an IFC model.
        query_endpoint: SPARQL QUERY endpoint.
        update_endpoint: SPARQL UPDATE endpoint.

    Returns:
        A tuple (asset_df, building_data), where asset_df is a dataframe containing the individual assets and their
        metadata with headers 'file', 'name', 'uid', 'iri', and building_data is an list
        containing building_iri and building_name in this order.
    """
    logger.info("Retrieving metadata from endpoint...")
    metadata = retrieve_metadata(query_endpoint, update_endpoint)

    logger.info("Generating dictionary for splitting IFC assets.")
    filename_to_ifc_ids_mappings = get_filename_to_ifc_ids_mapping(metadata)

    # Convert IFC -> glb for building
    # Initialise a dictionary with the IFC classes to exclude from the building output model
    # Non-exhaustive. If required, add more classes in the format 'IfcFeatureType'
    building_exclude_ifcclasses = ["IfcBuildingElementProxy", "IfcFurnishingElement", "IfcFlowTerminal", "IfcSpace",
                                   "IfcOpeningElement", "IfcFlowSegment"]
    building_ifcconvert_options = ["--exclude", "entities"] + building_exclude_ifcclasses
    exec_gltf_conversion(input_ifc, "building", building_ifcconvert_options)

    # Convert IFC -> glb for assets
    for filename, ifc_ids in filename_to_ifc_ids_mappings.items():
        ifcconvert_options = ["--include", "attribute", "GlobalId"] + ifc_ids
        exec_gltf_conversion(input_ifc, filename, ifcconvert_options)

    logger.info("Conversion to geometry outputs completed...")

    asset_data = metadata[~metadata["file"].isin(["furniture", "solarpanel", "sewagenetwork"])]
    building_iri, building_name = get_building_iri_name(query_endpoint, update_endpoint)
    building_data = [building_iri, building_name]
    return asset_data, building_data
