################################################
# Authors: Markus Hofmeister (mh807@cam.ac.uk) #    
# Date: 04 Apr 2022                            #
################################################

import pytest

from agent.datainstantiation.stations import *
from agent.dataretrieval.readings import *
from agent.errorhandling.exceptions import InvalidInput

from tests.conftest import *

# Import module(s) under test from agent
from agent.dataretrieval.stations import *


@pytest.mark.parametrize("center, radius, expected_msg", \
[
('12.34', None, "Circle center or radius is missing for geospatial search."),
(None, 50, "Circle center or radius is missing for geospatial search."),
('123,456', 50, "Circle center coordinates shall be provided as \"latitude#longitude\" in EPSG:4326 coordinates."),
]
)
def test_get_all_metoffice_stations_exceptions(center, radius, expected_msg):

    with pytest.raises(InvalidInput) as excinfo:
    # Check correct exception type
        get_all_metoffice_stations(circle_center=center, circle_radius=radius)
        pytest.fail
    # Check correct exception message
    assert expected_msg in str(excinfo.value)


@pytest.mark.parametrize("station_iris, expected_query", \
[
(None, '\n        SELECT ?station ?stationID ?quantityType ?dataIRI ?unit ?tsIRI \n        WHERE {\n            ?station <https://www.theworldavatar.com/kg/ontoems/hasIdentifier> ?stationID ;\n                     <https://www.theworldavatar.com/kg/ontoems/reports> ?quantity .\n            \n            ?station <http://www.w3.org/1999/02/22-rdf-syntax-ns#type> <https://www.theworldavatar.com/kg/ontoems/ReportingStation> ;\n                     <https://www.theworldavatar.com/kg/ontoems/dataSource> "Met Office DataPoint" .  \n            ?quantity <http://www.ontology-of-units-of-measure.org/resource/om-2/hasValue> ?dataIRI ;\n                      <http://www.w3.org/1999/02/22-rdf-syntax-ns#type> ?quantityType .\n            ?dataIRI <https://www.theworldavatar.com/kg/ontotimeseries/hasTimeSeries> ?tsIRI ;   \n                     <http://www.ontology-of-units-of-measure.org/resource/om-2/hasUnit>/<http://www.ontology-of-units-of-measure.org/resource/om-2/symbol> ?unit .\n            ?tsIRI <http://www.w3.org/1999/02/22-rdf-syntax-ns#type> <https://www.theworldavatar.com/kg/ontotimeseries/TimeSeries> .\n        }\n        ORDER BY ?tsIRI\n    '),
(['Station_IRI1', 'Station_IRI2'], '\n        SELECT ?station ?stationID ?quantityType ?dataIRI ?unit ?tsIRI \n        WHERE {\n            ?station <https://www.theworldavatar.com/kg/ontoems/hasIdentifier> ?stationID ;\n                     <https://www.theworldavatar.com/kg/ontoems/reports> ?quantity .\n            FILTER (?station IN (<Station_IRI1>, <Station_IRI2>) )  \n            ?quantity <http://www.ontology-of-units-of-measure.org/resource/om-2/hasValue> ?dataIRI ;\n                      <http://www.w3.org/1999/02/22-rdf-syntax-ns#type> ?quantityType .\n            ?dataIRI <https://www.theworldavatar.com/kg/ontotimeseries/hasTimeSeries> ?tsIRI ;   \n                     <http://www.ontology-of-units-of-measure.org/resource/om-2/hasUnit>/<http://www.ontology-of-units-of-measure.org/resource/om-2/symbol> ?unit .\n            ?tsIRI <http://www.w3.org/1999/02/22-rdf-syntax-ns#type> <https://www.theworldavatar.com/kg/ontotimeseries/TimeSeries> .\n        }\n        ORDER BY ?tsIRI\n    ')
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


@pytest.mark.skip(reason="Only works as integration test with Blazegraph running at endpoint specified in `stack_configs_mock.py` file.\
                          Default settings in `stack_configs_mock.py` match provided `docker-compose.test.yml`")
def test_get_all_metoffice_stations(clear_triple_store, mocker):

    # Read test station data
    station_data = read_station_data()
    to_instantiate = [station_data[i] for i in station_data]

    # Mock Stack client initialisations
    mocker.patch('agent.kgutils.stackclients.PostGISClient.__init__', return_value=None)
    mocker.patch('agent.kgutils.stackclients.GdalClient.__init__', return_value=None)
    mocker.patch('agent.kgutils.stackclients.GeoserverClient.__init__', return_value=None)
    # Mock PostGIS client methods
    mocker.patch('agent.kgutils.stackclients.PostGISClient.check_table_exists', return_value=True)
    mocker.patch('agent.kgutils.stackclients.PostGISClient.check_point_feature_exists', return_value=True)
    mocker.patch('agent.kgutils.stackclients.PostGISClient.get_feature_iris_in_circle', return_value=['https://www.theworldavatar.com/kg/ontoems/ReportingStation_1'])
    # Mock call to uuid function
    mocker.patch('uuid.uuid4', side_effect=[str(1), str(2), str(3)])

    # Retrieve SPARQL endpoint from `stack_configs_mock.py` file and create namespace (if not exists)
    endpoint = QUERY_ENDPOINT
    create_blazegraph_namespace(endpoint)

    # Verify that knowledge base is empty
    res = get_all_metoffice_stations(query_endpoint=endpoint)
    assert len(res) == 0

    # Instantiate stations
    instantiate_stations(to_instantiate, query_endpoint=endpoint, update_endpoint=endpoint)    

    # Verify that all stations are retrieved
    res = get_all_metoffice_stations(query_endpoint=endpoint)
    assert len(res) == 3

    # Verify that only one stations is retrieved
    res = get_all_metoffice_stations(query_endpoint=endpoint,
                                     circle_center='57.5#-3.5',
                                     circle_radius='100')
    assert len(res) == 1
    assert list(res.keys())[0] == station_data['station1']['id']
