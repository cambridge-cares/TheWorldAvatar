"""
# Author: qhouyee #

A test suite for the agent.config.config submodule.
"""

# Standard import
import os

# Third party import
import pytest
import yaml

# Self import
import agent.config.config as properties

ENDPOINT = "http://www.example.org/test"

def gen_test_yaml_file(output_path):
    """
    Generates a sample yaml file with required properties for retrieval

    Arguments:
    output_path - output file path for the test yaml file
    """
    data = dict(
        root_tile = dict(
            x_center = 40,
            y_center = 0,
            z_center = 15,
            length = 100,
            width = 100,
            height = 10
        ),
        child_tile = dict(
            x_center = 10,
            y_center = 0,
            z_center = 5,
            length = 50,
            width = 50,
            height = 6
        ),
        query_endpoint = ENDPOINT,
        update_endpoint = ENDPOINT
    )
    # Generate the file
    with open(output_path, 'w') as outfile:
        yaml.dump(data, outfile)


def test_set_properties():
    """
    Tests set_properties()
    """
    # Set up and generate sample yaml file
    yaml_path = 'sample.yml'
    gen_test_yaml_file(yaml_path)
    try:
        # Execute method
        res_query_endpoint, res_update_endpoint = properties.set_properties(yaml_path)
        # Generate expected box
        expected_root_bbox = [40, 0, 15, 50, 0, 0, 0, 50, 0, 0, 0, 5]
        expected_child_bbox = [10, 0, 5, 25, 0, 0, 0, 25, 0, 0, 0, 3]
        assert expected_root_bbox == properties.bbox_root
        assert expected_child_bbox == properties.bbox_child
        assert ENDPOINT == res_query_endpoint
        assert ENDPOINT == res_update_endpoint
    finally:
        os.remove(yaml_path)


def test_bbox_root():
    """
    Tests set_bbox()
    """
    expected_bbox = [40, 0, 16, 100, 0, 0, 0, 100, 0, 0, 0, 20]
    assert expected_bbox == properties.set_bbox(40,0,16,200,200,40)
