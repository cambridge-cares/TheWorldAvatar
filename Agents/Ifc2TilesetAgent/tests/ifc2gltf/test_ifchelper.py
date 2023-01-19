"""
# Author: qhouyee #

A test suite for the agent.ifc2gltf.ifchelper submodule.
"""

# Standard import
import os

# Third party import
import pytest
import ifcopenshell
import pandas as pd

# Self import
from agent.ifc2gltf.ifchelper import append_aggregate, append_individual_asset, gendict4split, append_ifcconvert_command


def gen_sampledf(asset=True):
    """
    Generate a sample dataframe for testing

    Arguments:
        asset - a boolean indicating if dataframe generated should be for asset or not
    """
    if asset:
        assetdata = {
            "file": ["asset1", "asset2"],
            "uid": ["id3", "id4"]
            }
        return pd.DataFrame(assetdata)
    else:
        aggregatedata = {
            "file": ["furniture", "furniture"],
            "uid": ["id1", "id2"]
            }
        return pd.DataFrame(aggregatedata)


def test_append_aggregate():
    """
    Tests append_aggregate()
    """
    # Generate sample dataframe
    sampledf = gen_sampledf(False)
    result = {}
    # Execute method
    append_aggregate(sampledf, result)
    # Test assertion
    assert result == {"furniture":["id1","id2"]}

    # Test for solar panel
    new_row = {"file": "solarpanel", "uid": "id5"}
    sampledf = pd.concat([sampledf, pd.DataFrame([new_row])], axis=0, ignore_index=True)
    result = {}
    append_aggregate(sampledf, result)
    # Test assertion
    assert result == {"furniture":["id1","id2"], "solarpanel":["id5"]}


def test_append_aggregate_emptydf():
    """
    Tests append_aggregate() with an empty dataframe input
    """
    # Initialise test params
    emptydf = pd.DataFrame()
    result = {}
    # Execute method
    append_aggregate(emptydf, result)
    assert result == {}


def test_append_individual_asset():
    """
    Tests append_individual_asset()
    """
    # Generate sample dataframe
    sampledf = gen_sampledf()
    result = {}
    # Execute method
    append_individual_asset(sampledf, result)
    # Test assertion
    assert result == {"asset1":"id3", "asset2":"id4"}


def test_append_individual_asset_emptydf():
    """
    Tests append_individual_asset() with an empty dataframe input
    """
    # Initialise test params
    emptydf = pd.DataFrame()
    result = {}
    # Execute method
    append_individual_asset(emptydf, result)
    assert result == {}


def test_gendict4split_simple():
    """
    Tests gendict4split() for simple model with no assets, furniture, or other elements
    """
    emptydf = pd.DataFrame()
    # Execute method
    dict_output, assetdata = gendict4split(emptydf)
    # Ensure that there is only building key in output
    assert dict_output["building"] == ['IfcBuildingElementProxy', 'IfcFurnishingElement',
                                       'IfcFlowTerminal', 'IfcSpace', 'IfcOpeningElement',
                                       'IfcFlowSegment']
    assert "furniture" not in dict_output
    assert "solarpanel" not in dict_output
    assert "asset1" not in dict_output
    # Check asset dataframe is empty without any asset
    assert not assetdata


def test_gendict4split_furniture():
    """
    Tests gendict4split() for model with asset and furniture
    """
    # Create sample dataframe
    assetdata = gen_sampledf()
    aggdata = gen_sampledf(False)
    sample_df = pd.concat([assetdata, aggdata], ignore_index=True, sort=False)

    # Execute method
    dict_output, assetdata = gendict4split(sample_df)
    # Ensure the following key value pairs are present
    assert dict_output["asset1"] == "id3"
    assert dict_output["asset2"] == "id4"
    assert dict_output["furniture"] == ["id1", "id2"]
    # Ensure following keys are not present
    assert "solarpanel" not in dict_output
    assert "sewagenetwork" not in dict_output
    # Check assetdata has two rows and the following values
    assert len(assetdata.index) == 2
    assert assetdata['file'].iloc[0] == "asset1"
    assert assetdata['uid'].iloc[0] == "id3"
    assert assetdata['file'].iloc[1] == "asset2"
    assert assetdata['uid'].iloc[1] == "id4"


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
