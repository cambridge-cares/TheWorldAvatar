###############################################
# Authors: Markus Hofmeister (mh807cam.ac.uk) #    
# Date: 04 Apr 2022                           #
###############################################

import pytest

# Import module(s) under test from metoffice
import metoffice.kgutils.prefixes as prefix
import metoffice.kgutils.querytemplates as templates


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


def test_add_station_data():
    # Define test data
    data1 = {'station_iri': None}
    data2 = {'station_iri': 'test_iri',
            'dataSource': 'test_source',
            'comment': 'test_comment',
            'id': 'test_id',
            'location': 'test_location',
            'elevation': 1.23,
	}
    data3 = {'station_iri': 'test_iri',
            'comment': 'test_comment',
            'dataSource': 'test_source',
            'id': 'test_id',
	}
    # Define expected results
    result1 = None
    result2 = "<test_iri> rdf:type ems:ReportingStation . " \
                + "<test_iri> ems:dataSource \"test_source\"^^xsd:string . " \
                + "<test_iri> rdfs:comment \"test_comment\"^^xsd:string . " \
                + "<test_iri> ems:hasIdentifier \"test_id\"^^xsd:string . " \
                + "<test_iri> ems:hasObservationLocation \"test_location\"^^geolit:lat-lon . " \
                + "<test_iri> ems:hasObservationElevation \"1.23\"^^xsd:float . "
    result3 = "<test_iri> rdf:type ems:ReportingStation . " \
                + "<test_iri> ems:dataSource \"test_source\"^^xsd:string . " \
                + "<test_iri> rdfs:comment \"test_comment\"^^xsd:string . " \
                + "<test_iri> ems:hasIdentifier \"test_id\"^^xsd:string . "   
    # Tests
    test1 = templates.add_station_data(**data1)
    assert test1 == result1
    test2 = templates.add_station_data(**data2)
    assert test2 == result2
    test3 = templates.add_station_data(**data3)
    assert test3 == result3
