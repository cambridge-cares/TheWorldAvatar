"""
# Author: qhouyee, picas9dan #

A test suite for the agent.config.config submodule.
"""

# Third-party imports
import yaml

# Self imports
import agent.config.config as properties

ENDPOINT = "http://www.example.org/test"


def gen_test_yaml_file(output_path, bim_iri="", bim_name="", solar_iri="", solar_name="", sewage_iri="", sewage_name=""):
    """
    Generates a sample yaml file with required properties for retrieval

    Arguments:
    output_path - output file path for the test yaml file
    """
    data = dict(
        query_endpoint=ENDPOINT,
        update_endpoint=ENDPOINT,
        bim_tileset_iri=bim_iri,
        bim_tileset_name=bim_name,
        solar_panel_tileset_iri=solar_iri,
        solar_panel_tileset_name=solar_name,
        sewage_tileset_iri=sewage_iri,
        sewage_tileset_name=sewage_name
    )
    # Generate the file
    with open(output_path, 'w') as outfile:
        yaml.dump(data, outfile)


def test_load_properties():
    """
    Tests load_properties()
    """
    # Set up and generate sample yaml file
    yaml_path = "sample.yml"
    sample_solar_iri = "http://www.example.org/Test/Panel_123"
    sample_solar_name = "Solar panels"
    sample_sewage_iri = "http://www.example.org/Test/Sewage_123"
    sample_sewage_name = "Sewage city network"
    gen_test_yaml_file(yaml_path, solar_iri=sample_solar_iri,
                       solar_name=sample_solar_name, sewage_iri=sample_sewage_iri,
                       sewage_name=sample_sewage_name)

    # Execute method
    res_query_endpoint, res_update_endpoint, solar_panel_tileset, sewage_tileset, bim_tileset = properties.load_properties(
        yaml_path)
    assert ENDPOINT == res_query_endpoint
    assert ENDPOINT == res_update_endpoint
    # Verify solar panel tileset
    assert solar_panel_tileset
    assert sample_solar_iri == solar_panel_tileset[0]
    assert sample_solar_name == solar_panel_tileset[1]
    # Verify sewage network tileset
    assert sewage_tileset
    assert sample_sewage_iri == sewage_tileset[0]
    assert sample_sewage_name == sewage_tileset[1]


def test_load_properties_for_solar_panel():
    """
    Tests load_properties() when there is an iri and name for solar panel
    """
    # Set up and generate sample yaml file
    yaml_path = "sample.yml"
    sample_solar_iri = "http://www.example.org/Test/Panel_123"
    sample_solar_name = "Solar panels"
    gen_test_yaml_file(yaml_path, solar_iri=sample_solar_iri,
                       solar_name=sample_solar_name)

    # Execute method
    res_query_endpoint, res_update_endpoint, solar_panel_tileset, sewage_tileset, bim_tileset = properties.load_properties(
        yaml_path)
    assert ENDPOINT == res_query_endpoint
    assert ENDPOINT == res_update_endpoint
    # Verify solar panel tileset
    assert solar_panel_tileset
    assert sample_solar_iri == solar_panel_tileset[0]
    assert sample_solar_name == solar_panel_tileset[1]
    # If empty, it should not have values
    assert not sewage_tileset


def test_load_properties_for_sewage():
    """
    Tests load_properties() when there is an iri and name for the sewage network
    """
    # Set up and generate sample yaml file
    yaml_path = "sample.yml"
    sample_sewage_iri = "http://www.example.org/Test/Sewage_123"
    sample_sewage_name = "Sewage city network"
    gen_test_yaml_file(yaml_path, sewage_iri=sample_sewage_iri,
                       sewage_name=sample_sewage_name)

    # Execute method
    res_query_endpoint, res_update_endpoint, solar_panel_tileset, sewage_tileset, bim_tileset = properties.load_properties(
        yaml_path)
    assert ENDPOINT == res_query_endpoint
    assert ENDPOINT == res_update_endpoint
    # Verify sewage network tileset
    assert sewage_tileset
    assert sample_sewage_iri == sewage_tileset[0]
    assert sample_sewage_name == sewage_tileset[1]
    # If empty, it should not have values
    assert not solar_panel_tileset


def test_load_properties_no_optional_values():
    """
    Tests load_properties() when there is no tileset iri or name
    """
    # Set up and generate sample yaml file
    yaml_path = "sample.yml"
    gen_test_yaml_file(yaml_path)

    # Execute method
    res_query_endpoint, res_update_endpoint, solar_panel_tileset, sewage_tileset, bim_tileset = properties.load_properties(
        yaml_path)
    assert ENDPOINT == res_query_endpoint
    assert ENDPOINT == res_update_endpoint
    # If empty, it should not have values
    assert not solar_panel_tileset
    assert not sewage_tileset
