"""
# Author: qhouyee #

A test suite for the agent.ifc2gltf.ifchelper submodule.
"""

# Standard import
import os

# Third party import
import pytest
import ifcopenshell

# Self import
from agent.ifc2gltf.ifchelper import verify_feature_exists, gendict4split, append_ifcconvert_command
from tests.ifc2gltf.test_ifc2gltf import gen_sample_ifc_file


def test_verify_feature_exists():
    """
    Tests verify_feature_exists()
    """
    # Create sample IFC file at this path
    ifc_path = os.path.join("data", "model.ifc")
    gen_sample_ifc_file(ifc_path)
    # Open the file in Python
    ifc = ifcopenshell.open(ifc_path)
    # Execute method
    result_list = verify_feature_exists([".IfcWall", ".IfcDoor"], ifc)
    os.remove(ifc_path)  # Remove file
    # Ensure that there is only IfcWall output
    assert result_list == ["IfcWall"]


def test_gendict4split_simple():
    """
    Tests gendict4split() for simple model with no assets, furniture, or other elements
    """
    # Create sample IFC file at this path
    ifc_path = os.path.join("data", "model.ifc")
    gen_sample_ifc_file(ifc_path)
    # Open the file in Python
    ifc = ifcopenshell.open(ifc_path)
    # Execute method
    dict_output, hashmapping = gendict4split(ifc)
    os.remove(ifc_path)  # Remove file
    # Ensure that there is only building key in output
    assert dict_output["building"] == ['IfcBuildingElementProxy', 'IfcFurnishingElement',
                                       'IfcFlowTerminal', 'IfcSpace', 'IfcOpeningElement',
                                       'IfcFlowSegment']
    assert "furniture" not in dict_output
    assert "solarpanel" not in dict_output
    assert "asset1" not in dict_output
    # Check hashmapping is empty without any asset
    assert not hashmapping


def test_gendict4split_furniture():
    """
    Tests gendict4split() for model with asset and furniture
    """
    # Create sample IFC file at this path
    ifc_path = os.path.join("data", "model.ifc")
    idlist = gen_sample_ifc_file(ifc_path, False)
    # Open the file in Python
    ifc = ifcopenshell.open(ifc_path)
    # Execute method
    dict_output, hashmapping = gendict4split(ifc)
    os.remove(ifc_path)  # Remove file
    # Ensure the following key value pairs are present
    assert dict_output["asset1"] == idlist[0]
    assert dict_output["furniture"][0] == idlist[1]
    # Ensure following keys are not present
    assert "solarpanel" not in dict_output
    assert "sewagenetwork" not in dict_output
    # Check hashmapping has one dictionary and the following values
    assert len(hashmapping) == 1
    assert hashmapping[idlist[0]]["file"] == "asset1"
    assert hashmapping[idlist[0]]["name"] == "Water Sensor"


def test_append_ifcconvert_command_building():
    """
    Tests append_ifcconvert_command() for building key
    """
    sample_command = ["test"]
    building_key = "building"
    building_values = ["IfcFurnishingElement", "IfcFlowTerminal", "IfcSpace"]
    output = append_ifcconvert_command(
        building_key, building_values, sample_command)
    assert output[0] == "test"
    assert output[1] == "--exclude"
    assert output[2] == "entities"
    for i, element in enumerate(building_values):
        assert output[3+i] == element


def test_append_ifcconvert_command():
    """
    Tests append_ifcconvert_command() for non-building keys
    """
    sample_command = ["uid"]
    building_key = "IfcElement"
    building_values = ["62a15v4", "912w85", "18p27s"]
    output = append_ifcconvert_command(
        building_key, building_values, sample_command)
    assert output[0] == "uid"
    assert output[1] == "--include"
    assert output[2] == "attribute"
    assert output[3] == "GlobalId"
    for i, element in enumerate(building_values):
        assert output[4+i] == element
