import os
import pytest
from rdflib import Graph

from src import kg_utils as utils


def initialise_triple_store():

    # Instantiate ABox from components_abox.owl as in-memory triple store / local graph
    g = Graph()
    rdf_file = os.path.abspath(os.path.join(os.getcwd(), "..", "resources", "components_abox.owl"))
    g.parse(rdf_file)
    return g


# def test_read_properties_file(tmp_path):
#
#     # Create empty test properties file
#     p = os.path.join(tmp_path, "test_timeseries.properties")
#     #p.write_text("")
#
#     with pytest.raises(KeyError) as excinfo:
#         # Check correct exception type
#         utils.read_properties_file(p)
#         # Check correct exception message
#         assert str(excinfo.value) == 'Key "fallback_kg" is missing in properties file: ' + p


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


# if __name__ == "__main__":
#     d = pytest.tmp_path / "sub"
#     d.mkdir()
#     p = d / "test_timeseries.properties"