################################################
# Authors: Markus Hofmeister (mh807@cam.ac.uk) #    
# Date: 04 Apr 2022                            #
################################################

import pytest

# Import module(s) under test from agent
from agent.kgutils.querytemplates import add_station_data


def test_add_station_data():
    # Test creation of correct SPARQL update triples to instantiate MetOffice 
    # station according to OntoEMS

    # Define test data
    data1 = {'station_iri': None}
    data2 = {'station_iri': 'test_iri',
            'dataSource': 'test_source',
            'label': 'test_label',
            'id': 'test_id',
            'elevation': 1.23,
	}
    data3 = {'station_iri': 'test_iri',
            'label': 'test_label',
            'dataSource': 'test_source',
            'id': 'test_id',
	}
    # Define expected results
    result1 = None
    result2 = "<test_iri> <http://www.w3.org/1999/02/22-rdf-syntax-ns#type> <https://www.theworldavatar.com/kg/ontoems/ReportingStation> . " \
            + "<test_iri> <https://www.theworldavatar.com/kg/ontoems/dataSource> \"test_source\"^^<http://www.w3.org/2001/XMLSchema#string> . " \
            + "<test_iri> <http://www.w3.org/2000/01/rdf-schema#label> \"test_label\"^^<http://www.w3.org/2001/XMLSchema#string> . " \
            + "<test_iri> <https://www.theworldavatar.com/kg/ontoems/hasIdentifier> \"test_id\"^^<http://www.w3.org/2001/XMLSchema#string> . " \
            + "<test_iri> <https://www.theworldavatar.com/kg/ontoems/hasObservationElevation> \"1.23\"^^<http://www.w3.org/2001/XMLSchema#float> . "
    result3 = "<test_iri> <http://www.w3.org/1999/02/22-rdf-syntax-ns#type> <https://www.theworldavatar.com/kg/ontoems/ReportingStation> . " \
            + "<test_iri> <https://www.theworldavatar.com/kg/ontoems/dataSource> \"test_source\"^^<http://www.w3.org/2001/XMLSchema#string> . " \
            + "<test_iri> <http://www.w3.org/2000/01/rdf-schema#label> \"test_label\"^^<http://www.w3.org/2001/XMLSchema#string> . " \
            + "<test_iri> <https://www.theworldavatar.com/kg/ontoems/hasIdentifier> \"test_id\"^^<http://www.w3.org/2001/XMLSchema#string> . " 
    # Tests
    test1 = add_station_data(**data1)
    assert test1 == result1
    test2 = add_station_data(**data2)
    assert test2 == result2
    test3 = add_station_data(**data3)
    assert test3 == result3
