import pytest

# Import module under test from metoffice
import metoffice.kgoperations.querytemplates as templates

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
    result2 = "<test_iri> rdf:type ems:ReportingStation ; ems:dataSource test_source ; rdfs:comment test_comment ; ems:hasIdentifier test_id ; ems:hasObservationLocation \"test_location\"^^geo:lat-lon ; ems:hasObservationElevation 1.23 ; "
    result3 = "<test_iri> rdf:type ems:ReportingStation ; ems:dataSource test_source ; rdfs:comment test_comment ; ems:hasIdentifier test_id ; "
    # Tests
    test1 = templates.add_station_data(**data1)
    assert test1 == result1
    test2 = templates.add_station_data(**data2)
    assert test2 == result2
    test3 = templates.add_station_data(**data3)
    assert test3 == result3
