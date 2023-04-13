"""
# Author: qhouyee, picas9dan #

This module provides helper functions to generate geometry outputs from the IFC file.
"""

# Standard library imports
from typing import List

# Third-party imports
import pandas as pd
from py4jps import agentlogging

# Self imports
from agent.utils.system_utils import retrieve_abs_filepath, run_shellcommand
from agent.kgutils.const import ID_VAR

# Retrieve logger
logger = agentlogging.get_logger("dev")


def get_filename_to_ifc_ids_mapping(dataframe: pd.DataFrame):
    """Creates a dictionary {filename : ifc_id} to split the IFC model into smaller IFC files.

    Args:
        dataframe: A dataframe containing asset data, with headers 'file', 'name', 'uid', 'iri'.

    Returns:
        A dict that maps filenames to IFC id's.
    """
    return dataframe.groupby("file", group_keys=False)[ID_VAR].apply(list).to_dict()


def exec_gltf_conversion(input_ifc: str, filename: str, options: List[str]):
    glbpath = f"./data/glb/{filename}.glb"
    glbpath = retrieve_abs_filepath(glbpath)

    ifcconvert_command = ["./IfcConvert", "-q", input_ifc, glbpath] + options

    logger.info(f"Converting {filename} to geometry output...")
    run_shellcommand(ifcconvert_command)
