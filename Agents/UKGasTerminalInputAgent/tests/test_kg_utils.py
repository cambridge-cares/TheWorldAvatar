import os
import pytest
from rdflib import Graph
from configobj import ConfigObj

from src import kg_utils as utils


def initialise_triple_store():

    # Instantiate ABox from components_abox.owl as in-memory triple store / local graph
    g = Graph()
    rdf_file = os.path.abspath(os.path.join(os.getcwd(), "..", "resources", "components_abox.owl"))
    g.parse(rdf_file)

    return g


def test_read_properties_file(tmp_path):

    # Create empty test properties file
    p = os.path.join(tmp_path, "test_timeseries.properties")

    # Test for 'fallback_kg'
    # 1) Exception for missing key
    with pytest.raises(KeyError) as excinfo:
        # Check correct exception type
        utils.read_properties_file(p)
    # Check correct exception message
    expected = 'Key "fallback_kg" is missing in properties file: '
    assert expected in str(excinfo.value)
    # 2) Exception for available key, but missing value
    with open(p, 'w') as f:
        f.write("fallback_kg = ")
    with pytest.raises(KeyError) as excinfo:
        # Check correct exception type
        utils.read_properties_file(p)
    # Check correct exception message
    expected = 'No "fallback_kg" value has been provided in properties file: '
    assert expected in str(excinfo.value)

    # Test for 'namespace'
    # 1) Exception for missing key
    with open(p, 'w') as f:
        f.write("fallback_kg = test_kg\n")
    with pytest.raises(KeyError) as excinfo:
        # Check correct exception type
        utils.read_properties_file(p)
    # Check correct exception message
    expected = 'Key "namespace" is missing in properties file: '
    assert expected in str(excinfo.value)
    # 2) Exception for available key, but missing value
    with open(p, 'a') as f:
        f.write("namespace = ")
    with pytest.raises(KeyError) as excinfo:
        # Check correct exception type
        utils.read_properties_file(p)
    # Check correct exception message
    expected = 'No "namespace" value has been provided in properties file: '
    assert expected in str(excinfo.value)

    # Test for 'output.directory'
    # 1) Exception for missing key
    with open(p, 'w') as f:
        f.write("fallback_kg = test_kg\n"
                "namespace = test_namespace\n")
    with pytest.raises(KeyError) as excinfo:
        # Check correct exception type
        utils.read_properties_file(p)
    # Check correct exception message
    expected = 'Key "output.directory" is missing in properties file: '
    assert expected in str(excinfo.value)
    # 2) Exception for available key, but missing value
    with open(p, 'a') as f:
        f.write("output.directory = ")
    with pytest.raises(KeyError) as excinfo:
        # Check correct exception type
        utils.read_properties_file(p)
    # Check correct exception message
    expected = 'No "output.directory" value has been provided in properties file: '
    assert expected in str(excinfo.value)

    # Test correct reading of timeseries.properties file
    with open(p, 'w') as f:
        f.write("fallback_kg = test_kg\n"
                "namespace = test_namespace\n"
                "output.directory = test_outdir")
    utils.read_properties_file(p)
    assert utils.FALLBACK_KG == 'test_kg'
    assert utils.NAMESPACE == 'test_namespace'
    assert utils.OUTPUT_DIR == 'test_outdir'


sets = ["fallback_kg = test_kg\nnamespace = test_namespace\noutput.directory = test_outdir",
        "fallback_kg = test_kg/\nnamespace = test_namespace\noutput.directory = test_outdir"]
@pytest.mark.parametrize('settings', sets)
def test_setKGEndpoints(tmp_path, settings):

    # Create test properties file
    p = os.path.join(tmp_path, "test_timeseries.properties")
    with open(p, 'w') as f:
        f.write(settings)
    # Read properties
    utils.read_properties_file(p)

    # Set KG endpoints and retrieve from properties file
    utils.setKGEndpoints(p)
    # Read properties file
    props = ConfigObj(p)
    assert props['sparql.query.endpoint'] == 'test_kg/namespace/test_namespace/sparql'
    assert props['sparql.update.endpoint'] == 'test_kg/namespace/test_namespace/sparql'
    assert props['sparql.query.endpoint'] != 'test_kg/namespace/test_namespace/sparql...'


def test_create_sparql_prefix():

    # Check for proper exception for not defined prefixes
    test_abbreviation = 'test'
    with pytest.raises(KeyError) as exc_info:
        # Check correct exception type
        utils.create_sparql_prefix(test_abbreviation)
    # Check correct exception message
    assert 'Prefix: "test" has not been specified' in str(exc_info.value)

    # Check for correct creation of defined prefixes
    test_abbreviation = 'comp'
    test_prefix = utils.create_sparql_prefix(test_abbreviation)
    assert test_prefix ==\
           'PREFIX comp: <http://www.theworldavatar.com/ontology/ontogasgrid/gas_network_components.owl#> '
