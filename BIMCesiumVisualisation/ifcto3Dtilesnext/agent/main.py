"""
# Author: qhouyee #

This module is the program's entry point.
"""

# Third party imports
import ifcopenshell

# Self imports
import utils
import ifc2gltf
import ifc2tileset as ts

# Set the IFC model
ifc = ifcopenshell.open(utils.read_ifc_file())

# Convert and split the ifc model into gltf files
hashmapping = ifc2gltf.conv2gltf(ifc, utils.read_ifc_file())

# Generate tilesets
ts.gen_tilesets(hashmapping)
