"""
# Author: qhouyee, picas9dan #

A test suite for the agent.config.config submodule.
"""

# Third-party imports
import yaml

# Self imports
import agent.config.config as properties

ENDPOINT = "http://www.example.org/test"


def gen_test_yaml_file(output_path):
    """
    Generates a sample yaml file with required properties for retrieval

    Arguments:
    output_path - output file path for the test yaml file
    """
    data = dict(
        query_endpoint=ENDPOINT,
        update_endpoint=ENDPOINT
    )
    # Generate the file
    with open(output_path, 'w') as outfile:
        yaml.dump(data, outfile)


def test_set_properties():
    """
    Tests set_properties()
    """
    # Set up and generate sample yaml file
    yaml_path = "sample.yml"
    gen_test_yaml_file(yaml_path)

    # Execute method
    res_query_endpoint, res_update_endpoint = properties.load_properties(yaml_path)
    assert ENDPOINT == res_query_endpoint
    assert ENDPOINT == res_update_endpoint
