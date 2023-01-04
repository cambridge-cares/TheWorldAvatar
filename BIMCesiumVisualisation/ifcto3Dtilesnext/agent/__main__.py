"""
# Author: qhouyee #

This module is the program's entry point.
"""

# Third party imports
import ifcopenshell

# Self imports
from agent.utils import read_ifc_file
from agent.ifc2gltf import conv2gltf
from agent.ifc2tileset import gen_tilesets

# Set the IFC model
ifc_filepath = read_ifc_file(['data', 'ifc'])
ifc = ifcopenshell.open(ifc_filepath)

# Convert and split the ifc model into gltf files
hashmapping = conv2gltf(ifc, ifc_filepath)

# Generate tilesets
gen_tilesets(hashmapping)
