import pytest

from src import kg_utils as utils


def test_create_sparql_prefix():

    # Check for proper exception for not defined prefixes
    test_abbreviation = 'test'
    with pytest.raises(KeyError) as excinfo:
        # Check correct exception type
        utils.create_sparql_prefix(test_abbreviation)
        # Check correct exception message
        assert str(excinfo.value) == 'Prefix: "test" has not been specified'

    # Check for correct creation of defined prefixes
    test_abbreviation = 'comp'
    test_prefix = utils.create_sparql_prefix(test_abbreviation)
    assert test_prefix ==\
           'PREFIX comp: <http://www.theworldavatar.com/ontology/ontogasgrid/gas_network_components.owl#> '
