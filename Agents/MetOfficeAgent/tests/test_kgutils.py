import pytest

# Import module under test from metoffice
import metoffice.kgutils.prefixes as prefix


def test_create_sparql_prefix():
    # Check for proper exception for not defined prefixes
    test_abbreviation = 'test'
    with pytest.raises(KeyError) as exc_info:
        # Check correct exception type
        prefix.create_sparql_prefix(test_abbreviation)
    # Check correct exception message
    assert 'Prefix: "test" has not been specified' in str(exc_info.value)

    # Check for correct creation of defined prefixes
    test_abbreviation = 'ems'
    test_prefix = prefix.create_sparql_prefix(test_abbreviation)
    assert test_prefix == \
           'PREFIX ems: <http://www.theworldavatar.com/ontology/ontoems/OntoEMS.owl#> '
