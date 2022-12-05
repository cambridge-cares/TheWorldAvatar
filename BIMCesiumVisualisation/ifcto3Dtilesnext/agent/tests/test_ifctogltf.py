"""
# Author: qhouyee #

A test suite for the agent.ifc2gltf submodule.
"""

# Standard import
import os

# Third party import
import pytest
import ifcopenshell
from ifcopenshell.api import run

# Self import
from ifc2gltf import verify_feature_exists, gendict4split, append_ifcconvert_command, conv2gltf


def gen_sample_ifc_file(ifc_path, is_simplified=True):
    """
    A test function to generate a sample IFC file with wall

    Argument:
    ifc_path - File path to the IFC file
    is_simplified - A boolean whether to create a simple IFC model with
        only one Wall, or include other elements
    Returns:
    A list containing the IDs of non-wall elements generated
    """
    # Create a blank model
    model = ifcopenshell.file()
    # All projects must have one IFC Project element
    project = run("root.create_entity", model,
                  ifc_class="IfcProject", name="My Project")
    # To generate geometry, must assign units, defaults to metric units without args
    run("unit.assign_unit", model)
    # Create a geometry modelling context for storing 3D geometries
    context = run("context.add_context", model, context_type="Model")
    body = run(
        "context.add_context", model,
        context_type="Model", context_identifier="Body", target_view="MODEL_VIEW", parent=context
    )
    # Create a site, building, and storey
    site = run("root.create_entity", model,
               ifc_class="IfcSite", name="My Site")
    building = run("root.create_entity", model,
                   ifc_class="IfcBuilding", name="Building A")
    storey = run("root.create_entity", model,
                 ifc_class="IfcBuildingStorey", name="Ground Floor")
    # Assign their relations
    run("aggregate.assign_object", model, relating_object=project, product=site)
    run("aggregate.assign_object", model, relating_object=site, product=building)
    run("aggregate.assign_object", model,
        relating_object=building, product=storey)
    # Create a wall
    wall = run("root.create_entity", model, ifc_class="IfcWall")
    # Add body geometry in meters
    representation = run("geometry.add_wall_representation",
                         model, context=body, length=5, height=3, thickness=0.2)
    # Assign body geometry to the wall
    run("geometry.assign_representation", model,
        product=wall, representation=representation)
    # Place the wall on ground floor
    run("spatial.assign_container", model,
        relating_structure=storey, product=wall)

    idlist = []
    if is_simplified:
        # Write out to a file
        model.write(ifc_path)
        # Return empty list and need not be stored
    else:
        sensorid = ifcopenshell.guid.new()
        furnishingid = ifcopenshell.guid.new()

        idlist.append(sensorid)
        idlist.append(furnishingid)
        # Create a furnishing element for water sensor
        model.create_entity("IfcFurnishingElement",
                            GlobalId=sensorid, Name="Water Sensor")
        # Create a random furnishing element
        model.create_entity("IfcFurnishingElement",
                            GlobalId=furnishingid, Name="Test object")
        # Write out to a file
        model.write(ifc_path)
    return idlist


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


def test_conv2gltf():
    """
    Tests conv2gltf()
    """
    # Create sample IFC file at this path
    ifc_path = os.path.join("data","ifc","model.ifc")
    glb_path = os.path.join("data","glb","building.glb")
    gltf_path = os.path.join("data","gltf","building.gltf")
    gen_sample_ifc_file(ifc_path)
    # Open the file in Python
    ifc = ifcopenshell.open(ifc_path)
    # Execute method
    hashmapping = conv2gltf(ifc,ifc_path)
    # Check if these files exists, and store them for testing after clean up
    glb_gltf_exists = os.path.exists(glb_path) and os.path.exists(gltf_path)
    # Remove files
    os.remove(ifc_path)  
    os.remove(glb_path)
    os.remove(gltf_path)
    # Test that both glb and gltf exists
    assert glb_gltf_exists
    # Check hashmapping is empty without any asset
    assert not hashmapping
