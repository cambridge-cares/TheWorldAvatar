################################################
# Authors: Markus Hofmeister (mh807@cam.ac.uk) #    
# Date: 04 Apr 2022                            #
################################################

import time
import pytest
from testcontainers.core.container import DockerContainer

from airquality.datainstantiation.stations import *
from airquality.dataretrieval.readings import *
from airquality.errorhandling.exceptions import InvalidInput
from tests.utils import *

# Import module under test from gasgridagent
from airquality.dataretrieval.stations import *


@pytest.fixture()
def initialise_triple_store():
    # Define temporary Docker container based on empty Blazegraph image from CMCL registry
    blazegraph = DockerContainer('docker.cmclinnovations.com/blazegraph_for_tests:1.0.0')
    blazegraph.with_exposed_ports(9999)
    yield blazegraph


@pytest.mark.parametrize("center, radius, expected_msg", \
[
('12.34', None, "Circle center or radius is missing for geo:search."),
(None, 50, "Circle center or radius is missing for geo:search."),
('123,456', 50, "Circle center coordinates shall be provided as \"latitude#longitude\" in WGS84 coordinates."),
]
)
def test_get_all_airquality_stations_exceptions(center, radius, expected_msg):

    with pytest.raises(InvalidInput) as excinfo:
    # Check correct exception type
        get_all_airquality_stations(circle_center=center, circle_radius=radius)
        pytest.fail
    # Check correct exception message
    assert expected_msg in str(excinfo.value)


def test_get_all_airquality_stations(initialise_triple_store):

    # Read test station data
    station_data = read_station_data()

    # Spin up temporary docker container
    with initialise_triple_store as container:
        # Wait some arbitrary time until container is reachable
        time.sleep(3)
        # Retrieve SPARQL endpoint & create new namespace with geospatial capabilities
        endpoint = get_sparql_endpoint(container)
        create_blazegraph_namespace(endpoint)

        # Verify that knowledge base is empty
        res = get_all_airquality_stations(query_endpoint=endpoint)
        assert len(res) == 0

        # Instantiate stations
        instantiate_stations(station_data, query_endpoint=endpoint, update_endpoint=endpoint)    

        # Verify that all stations are retrieved
        res = get_all_airquality_stations(query_endpoint=endpoint)
        assert len(res) == 3

        # Verify that only one station is retrieved
        res = get_all_airquality_stations(query_endpoint=endpoint,
                                          circle_center='57.0#-2.0',
                                          circle_radius='50')
        assert len(res) == 1
        assert list(res.keys())[0] == list(station_data[0].keys())[0]


@pytest.mark.parametrize("station_iris, expected_query", \
[
(None, '\n        SELECT ?station ?stationID ?quantityType ?dataIRI ?comment ?tsIRI ?unit\n        WHERE {\n            ?station <https://www.theworldavatar.com/kg/ontoems/hasIdentifier> ?stationID ;\n                     <https://www.theworldavatar.com/kg/ontoems/reports> ?quantity .\n            \n            ?station <http://www.w3.org/1999/02/22-rdf-syntax-ns#type> <https://www.theworldavatar.com/kg/ontoems/ReportingStation> ;\n                     <https://www.theworldavatar.com/kg/ontoems/dataSource> "UK-AIR Sensor Observation Service" .  \n            ?quantity <http://www.ontology-of-units-of-measure.org/resource/om-2/hasValue> ?dataIRI ;\n                      <http://www.w3.org/1999/02/22-rdf-syntax-ns#type> ?quantityType ;\n                      <http://www.w3.org/2000/01/rdf-schema#comment> ?comment .\n            ?dataIRI <https://github.com/cambridge-cares/TheWorldAvatar/blob/main/JPS_Ontology/ontology/ontotimeseries/OntoTimeSeries.owl#hasTimeSeries> ?tsIRI ;   \n                     <http://www.ontology-of-units-of-measure.org/resource/om-2/hasUnit>/<http://www.ontology-of-units-of-measure.org/resource/om-2/symbol> ?unit .\n            ?tsIRI <http://www.w3.org/1999/02/22-rdf-syntax-ns#type> <https://github.com/cambridge-cares/TheWorldAvatar/blob/main/JPS_Ontology/ontology/ontotimeseries/OntoTimeSeries.owl#TimeSeries> .\n        }\n        ORDER BY ?tsIRI\n    '),
(['Station_IRI1', 'Station_IRI2'], '\n        SELECT ?station ?stationID ?quantityType ?dataIRI ?comment ?tsIRI ?unit\n        WHERE {\n            ?station <https://www.theworldavatar.com/kg/ontoems/hasIdentifier> ?stationID ;\n                     <https://www.theworldavatar.com/kg/ontoems/reports> ?quantity .\n            FILTER (?station IN (<Station_IRI1>, <Station_IRI2>) )  \n            ?quantity <http://www.ontology-of-units-of-measure.org/resource/om-2/hasValue> ?dataIRI ;\n                      <http://www.w3.org/1999/02/22-rdf-syntax-ns#type> ?quantityType ;\n                      <http://www.w3.org/2000/01/rdf-schema#comment> ?comment .\n            ?dataIRI <https://github.com/cambridge-cares/TheWorldAvatar/blob/main/JPS_Ontology/ontology/ontotimeseries/OntoTimeSeries.owl#hasTimeSeries> ?tsIRI ;   \n                     <http://www.ontology-of-units-of-measure.org/resource/om-2/hasUnit>/<http://www.ontology-of-units-of-measure.org/resource/om-2/symbol> ?unit .\n            ?tsIRI <http://www.w3.org/1999/02/22-rdf-syntax-ns#type> <https://github.com/cambridge-cares/TheWorldAvatar/blob/main/JPS_Ontology/ontology/ontotimeseries/OntoTimeSeries.owl#TimeSeries> .\n        }\n        ORDER BY ?tsIRI\n    ')
]
)
def test_instantiated_observation_timeseries(station_iris, expected_query):

    # Create query
    query = instantiated_observation_timeseries(station_iris)
    assert query == expected_query


@pytest.mark.parametrize("t1, t2, expectedMsg", \
[
('2022-03', None, f'Provided format of tmin could not be derived. Expected format: {TIME_FORMAT}'),
(None, 'test', f'Provided format of tmax could not be derived. Expected format: {TIME_FORMAT}'),
]
)
def test_get_time_series_data_exceptions(t1, t2, expectedMsg):
    with pytest.raises(InvalidInput) as excinfo:
    # Check correct exception type
        get_time_series_data(tmin=t1, tmax=t2)
    # Check correct exception message
    assert expectedMsg in str(excinfo.value)
