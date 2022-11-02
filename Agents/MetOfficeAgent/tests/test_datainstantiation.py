###############################################
# Authors: Markus Hofmeister (mh807cam.ac.uk) #    
# Date: 04 Apr 2022                           #
###############################################

import time
import copy
import datetime as dt
import pytest
from testcontainers.core.container import DockerContainer

from metoffice.dataretrieval.stations import *
from metoffice.dataretrieval.readings import *
from metoffice.errorhandling.exceptions import APIException
from metoffice.utils.env_configs import QUERY_ENDPOINT
from metoffice.flaskapp import create_app
from tests.utils import *

# Import modules under test from gasgridagent
from metoffice.datainstantiation.stations import *
from metoffice.datainstantiation.readings import *


@pytest.fixture()
def initialise_triple_store():
    # Define temporary Docker container based on empty Blazegraph image from CMCL registry
    blazegraph = DockerContainer('docker.cmclinnovations.com/blazegraph_for_tests:1.0.0')
    blazegraph.with_exposed_ports(9999)
    yield blazegraph


@pytest.fixture
def client():
    app = create_app({'TESTING': True})
    with app.test_client() as client:
        yield client


def test_instantiate_stations(initialise_triple_store):

    # Read test station data
    station_data = read_station_data()
    data1 = [station_data['station1']]
    data2 = [station_data['station1'], station_data['station2']]
    data3 = [station_data['station3']]

    # Spin up temporary docker container
    with initialise_triple_store as container:
        # Wait some arbitrary time until container is reachable
        time.sleep(3)
        # Retrieve SPARQL endpoint
        endpoint = get_sparql_endpoint(container)
        create_blazegraph_namespace(endpoint)

        # Verify that knowledge base is empty
        res = get_all_metoffice_stations(query_endpoint=endpoint)
        assert len(res) == 0

        # Instantiate first station  
        instantiate_stations(data1, query_endpoint=endpoint, update_endpoint=endpoint)      
        res = get_all_metoffice_station_ids(query_endpoint=endpoint)
        assert res[0] == station_data['station1']['id']
        triples = get_number_of_triples(endpoint)
        assert triples == 6

        # Instantiate second station   
        instantiate_stations(data2, query_endpoint=endpoint, update_endpoint=endpoint)           
        res = get_all_metoffice_station_ids(query_endpoint=endpoint)
        assert len(res) == 3
        triples = get_number_of_triples(endpoint)
        assert triples == 17

        # Instantiate third station   
        instantiate_stations(data3, query_endpoint=endpoint, update_endpoint=endpoint)           
        res = get_all_metoffice_station_ids(query_endpoint=endpoint)
        assert len(res) == 4
        triples = get_number_of_triples(endpoint)
        assert triples == 21


def test_retrieve_station_data_from_api_exceptions():

    with pytest.raises(APIException) as excinfo:
    # Check correct exception type
        retrieve_station_data_from_api(None)
    # Check correct exception message
    expected = 'No Met Office DataPoint API key provided.'
    assert expected in str(excinfo.value)


def test_instantiate_all_stations(initialise_triple_store, mocker):

    # Read test station data
    station_data = read_station_data()
    station_data = [station_data[i] for i in station_data]
    # Mock call to Met Office DataPoint API
    m = mocker.patch('metoffice.datainstantiation.stations.retrieve_station_data_from_api',
                     return_value=station_data)

    # Spin up temporary docker container
    with initialise_triple_store as container:
        # Wait some arbitrary time until container is reachable
        time.sleep(3)
        # Retrieve SPARQL endpoint
        endpoint = get_sparql_endpoint(container)
        create_blazegraph_namespace(endpoint)

        # Verify that knowledge base is empty
        res = get_all_metoffice_station_ids(query_endpoint=endpoint)
        assert len(res) == 0

        # Instantiate all stations
        instantiate_all_stations('test_api_key', query_endpoint=endpoint,
                                 update_endpoint=endpoint)
        # Verify that data gets added
        res = get_all_metoffice_station_ids(query_endpoint=endpoint)
        assert len(res) == 3
        triples = get_number_of_triples(endpoint)
        assert triples == 15

        # Instantiate all stations
        instantiate_all_stations('test_api_key', query_endpoint=endpoint,
                                 update_endpoint=endpoint)
        # Verify that same data does not get added twice
        res = get_all_metoffice_station_ids(query_endpoint=endpoint)
        assert len(res) == 3
        triples = get_number_of_triples(endpoint)
        assert triples == 15


@pytest.mark.skip(reason="only works as integration test with blank namespace in local blazegraph \
                          as TimeSeriesClient reads required inputs directly from properties file")
def test_instantiate_all_stations_webapp(client, mocker):
    # Integration test for expected behavior of instantiation of all stations
    # via webapp (requires (local) blazegraph running at endpoints specified
    # in 'metoffice.properties'; namespace MUST be empty)

    # Read test station data
    station_data = read_station_data()
    station_data = [station_data[i] for i in station_data]
    # Mock call to Met Office DataPoint API
    m = mocker.patch('metoffice.datainstantiation.stations.retrieve_station_data_from_api',
                     return_value=station_data)

    # Verify that knowledge base is empty
    res = get_all_metoffice_station_ids(query_endpoint=QUERY_ENDPOINT)
    assert len(res) == 0
   
    # Instantiate all stations
    route = '/api/metofficeagent/instantiate/stations'
    response = client.get(route)
    new_stations = response.json['stations']
    assert new_stations == 3

    # Instantiate all stations (2nd time)
    route = '/api/metofficeagent/instantiate/stations'
    response = client.get(route)
    new_stations = response.json['stations']
    assert new_stations == 0


def test_condition_readings_data():

    # Test readings data as returned by metoffer
    test_readings = [
        {'Dew Point': (8.3, 'C', 'Dp'),
        'Pressure': (1002, 'hpa', 'P'),
        'Pressure Tendency': ('F', 'Pa/s', 'Pt'),
        'Screen Relative Humidity': (90.3, '%', 'H'),
        'Temperature': (9.8, 'C', 'T'),
        'Visibility': (75000, 'm', 'V'),
        'Weather Type': (7, '', 'W'),
        'Wind Direction': ('S', 'compass', 'D'),
        'Wind Gust': (30, 'mph', 'G'),
        'Wind Speed': (21, 'mph', 'S'),
        'timestamp': (dt.datetime(2022, 4, 4, 1, 0), '')},       
        {'Dew Point': (8.1, 'C', 'Dp'),
        'Pressure': (1001, 'hpa', 'P'),
        'Pressure Tendency': ('F', 'Pa/s', 'Pt'),
        'Screen Relative Humidity': (87.9, '%', 'H'),
        'Temperature': (10.0, 'C', 'T'),
        'Visibility': (75000, 'm', 'V'),
        'Weather Type': (9, '', 'W'),
        'Wind Direction': ('WSW', 'compass', 'D'),
        'Wind Gust': (30, 'mph', 'G'),
        'Wind Speed': (19, 'mph', 'S'),
        'timestamp': (dt.datetime(2022, 4, 4, 2, 0), '')},       
        {'Dew Point': (8.4, 'C', 'Dp'),
        'Pressure': (1001, 'hpa', 'P'),
        'Pressure Tendency': ('F', 'Pa/s', 'Pt'),
        'Screen Relative Humidity': (90.9, '%', 'H'),
        'Temperature': (9.8, 'C', 'T'),
        'Visibility': (29000, 'm', 'V'),
        'Weather Type': (12, '', 'W'),
        'Wind Direction': ('NE', 'compass', 'D'),
        'Wind Gust': (32, 'mph', 'G'),
        'Wind Speed': (22, 'mph', 'S'),
        'timestamp': (dt.datetime(2022, 4, 4, 3, 0), '')}
        ]
    # Expected results
    expected1 = {'AirTemperature': None, 'AtmosphericPressure': None, 'DewPoint': None, 
                 'RelativeHumidity': None, 'Visibility': None, 
                 'WindDirection': None, 'WindSpeed': None, 'WindGust': None}
    expected2 = {'AirTemperature': [9.8, 10.0, 9.8], 
                 'AtmosphericPressure': [1002.0, 1001.0, 1001.0], 
                 'DewPoint': [8.3, 8.1, 8.4], 
                 'RelativeHumidity': [90.3, 87.9, 90.9], 
                 'Visibility': [75000.0, 75000.0, 29000.0], 
                 'WindDirection': [180.0, 247.5, 45.0], 
                 'WindSpeed': [21.0, 19.0, 22.0], 
                 'WindGust': [30.0, 30.0, 32.0], 
                 'timestamp': ['2022-04-04T01:00:00Z', '2022-04-04T02:00:00Z', '2022-04-04T03:00:00Z']}
    
    # Perform test for retrieval of keys only
    res = condition_readings_data(test_readings)
    for k in res['readings']:
        assert res['readings'][k] == expected1[k]

    # Perform test for retrieval of keys and data
    res = condition_readings_data(test_readings, False)
    for k in res['readings']:
        assert res['readings'][k] == expected2[k]
    assert res['times'] == expected2['timestamp']


def test_add_readings_for_station(mocker):

    # Test readings
    test_readings = ['DewPoint', 'WindGust']
     
    # Expected result
    expected_obs1_1 = '<http://Station/1> <https://www.theworldavatar.com/kg/ontoems/reports> <https://www.theworldavatar.com/kg/ontoems/WindGust_1> . <https://www.theworldavatar.com/kg/ontoems/WindGust_1> <http://www.w3.org/1999/02/22-rdf-syntax-ns#type> <https://www.theworldavatar.com/kg/ontoems/WindGust> . <https://www.theworldavatar.com/kg/ontoems/Measure_1> <http://www.w3.org/1999/02/22-rdf-syntax-ns#type> <http://www.ontology-of-units-of-measure.org/resource/om-2/Measure> . <https://www.theworldavatar.com/kg/ontoems/Measure_1> <http://www.ontology-of-units-of-measure.org/resource/om-2/hasUnit> <http://www.ontology-of-units-of-measure.org/resource/om-2/mile-StatutePerHour> . <http://www.ontology-of-units-of-measure.org/resource/om-2/mile-StatutePerHour> <http://www.w3.org/1999/02/22-rdf-syntax-ns#type> <http://www.ontology-of-units-of-measure.org/resource/om-2/Unit> . <http://www.ontology-of-units-of-measure.org/resource/om-2/mile-StatutePerHour> <http://www.ontology-of-units-of-measure.org/resource/om-2/symbol> "mi/h"^^<http://www.w3.org/2001/XMLSchema#string> . <https://www.theworldavatar.com/kg/ontoems/WindGust_1> <http://www.ontology-of-units-of-measure.org/resource/om-2/hasValue> <https://www.theworldavatar.com/kg/ontoems/Measure_1> .'
    expected_obs1_2 = '<http://Station/1> <https://www.theworldavatar.com/kg/ontoems/reports> <https://www.theworldavatar.com/kg/ontoems/DewPoint_1> . <https://www.theworldavatar.com/kg/ontoems/DewPoint_1> <http://www.w3.org/1999/02/22-rdf-syntax-ns#type> <https://www.theworldavatar.com/kg/ontoems/DewPoint> . <https://www.theworldavatar.com/kg/ontoems/Measure_1> <http://www.w3.org/1999/02/22-rdf-syntax-ns#type> <http://www.ontology-of-units-of-measure.org/resource/om-2/Measure> . <https://www.theworldavatar.com/kg/ontoems/Measure_1> <http://www.ontology-of-units-of-measure.org/resource/om-2/hasUnit> <http://www.ontology-of-units-of-measure.org/resource/om-2/degreeCelsius> . <http://www.ontology-of-units-of-measure.org/resource/om-2/degreeCelsius> <http://www.w3.org/1999/02/22-rdf-syntax-ns#type> <http://www.ontology-of-units-of-measure.org/resource/om-2/Unit> . <http://www.ontology-of-units-of-measure.org/resource/om-2/degreeCelsius> <http://www.ontology-of-units-of-measure.org/resource/om-2/symbol> "&#x00B0;C"^^<http://www.w3.org/2001/XMLSchema#string> . <https://www.theworldavatar.com/kg/ontoems/DewPoint_1> <http://www.ontology-of-units-of-measure.org/resource/om-2/hasValue> <https://www.theworldavatar.com/kg/ontoems/Measure_1> .'
    expected_obs2_1 = '<http://Station/1> <https://www.theworldavatar.com/kg/ontoems/reports> <https://www.theworldavatar.com/kg/ontoems/WindGust_1> . <https://www.theworldavatar.com/kg/ontoems/WindGust_1> <http://www.w3.org/1999/02/22-rdf-syntax-ns#type> <https://www.theworldavatar.com/kg/ontoems/WindGust> . <https://www.theworldavatar.com/kg/ontoems/Measure_1> <http://www.w3.org/1999/02/22-rdf-syntax-ns#type> <http://www.ontology-of-units-of-measure.org/resource/om-2/Measure> . <https://www.theworldavatar.com/kg/ontoems/Measure_1> <http://www.ontology-of-units-of-measure.org/resource/om-2/hasUnit> <http://www.ontology-of-units-of-measure.org/resource/om-2/mile-StatutePerHour> . <http://www.ontology-of-units-of-measure.org/resource/om-2/mile-StatutePerHour> <http://www.w3.org/1999/02/22-rdf-syntax-ns#type> <http://www.ontology-of-units-of-measure.org/resource/om-2/Unit> . <http://www.ontology-of-units-of-measure.org/resource/om-2/mile-StatutePerHour> <http://www.ontology-of-units-of-measure.org/resource/om-2/symbol> "mi/h"^^<http://www.w3.org/2001/XMLSchema#string> . <https://www.theworldavatar.com/kg/ontoems/WindGust_1> <http://www.ontology-of-units-of-measure.org/resource/om-2/hasValue> <https://www.theworldavatar.com/kg/ontoems/Measure_1> . <https://www.theworldavatar.com/kg/ontoems/WindGust_1> <http://www.w3.org/2000/01/rdf-schema#comment> "test"^^<http://www.w3.org/2001/XMLSchema#string> .'
    expected_obs2_2 = '<http://Station/1> <https://www.theworldavatar.com/kg/ontoems/reports> <https://www.theworldavatar.com/kg/ontoems/DewPoint_1> . <https://www.theworldavatar.com/kg/ontoems/DewPoint_1> <http://www.w3.org/1999/02/22-rdf-syntax-ns#type> <https://www.theworldavatar.com/kg/ontoems/DewPoint> . <https://www.theworldavatar.com/kg/ontoems/Measure_1> <http://www.w3.org/1999/02/22-rdf-syntax-ns#type> <http://www.ontology-of-units-of-measure.org/resource/om-2/Measure> . <https://www.theworldavatar.com/kg/ontoems/Measure_1> <http://www.ontology-of-units-of-measure.org/resource/om-2/hasUnit> <http://www.ontology-of-units-of-measure.org/resource/om-2/degreeCelsius> . <http://www.ontology-of-units-of-measure.org/resource/om-2/degreeCelsius> <http://www.w3.org/1999/02/22-rdf-syntax-ns#type> <http://www.ontology-of-units-of-measure.org/resource/om-2/Unit> . <http://www.ontology-of-units-of-measure.org/resource/om-2/degreeCelsius> <http://www.ontology-of-units-of-measure.org/resource/om-2/symbol> "&#x00B0;C"^^<http://www.w3.org/2001/XMLSchema#string> . <https://www.theworldavatar.com/kg/ontoems/DewPoint_1> <http://www.ontology-of-units-of-measure.org/resource/om-2/hasValue> <https://www.theworldavatar.com/kg/ontoems/Measure_1> . <https://www.theworldavatar.com/kg/ontoems/DewPoint_1> <http://www.w3.org/2000/01/rdf-schema#comment> "test"^^<http://www.w3.org/2001/XMLSchema#string> .'
    expected_fc1 =    '<http://Station/1> <https://www.theworldavatar.com/kg/ontoems/reports> <https://www.theworldavatar.com/kg/ontoems/WindGust_1> . <https://www.theworldavatar.com/kg/ontoems/WindGust_1> <http://www.w3.org/1999/02/22-rdf-syntax-ns#type> <https://www.theworldavatar.com/kg/ontoems/WindGust> . <https://www.theworldavatar.com/kg/ontoems/Forecast_1> <http://www.w3.org/1999/02/22-rdf-syntax-ns#type> <https://www.theworldavatar.com/kg/ontoems/Forecast> . <https://www.theworldavatar.com/kg/ontoems/Forecast_1> <http://www.ontology-of-units-of-measure.org/resource/om-2/hasUnit> <http://www.ontology-of-units-of-measure.org/resource/om-2/mile-StatutePerHour> . <http://www.ontology-of-units-of-measure.org/resource/om-2/mile-StatutePerHour> <http://www.w3.org/1999/02/22-rdf-syntax-ns#type> <http://www.ontology-of-units-of-measure.org/resource/om-2/Unit> . <http://www.ontology-of-units-of-measure.org/resource/om-2/mile-StatutePerHour> <http://www.ontology-of-units-of-measure.org/resource/om-2/symbol> "mi/h"^^<http://www.w3.org/2001/XMLSchema#string> . <https://www.theworldavatar.com/kg/ontoems/WindGust_1> <https://www.theworldavatar.com/kg/ontoems/hasForecastedValue> <https://www.theworldavatar.com/kg/ontoems/Forecast_1> .'
    expected_fc2 =    '<http://Station/1> <https://www.theworldavatar.com/kg/ontoems/reports> <https://www.theworldavatar.com/kg/ontoems/DewPoint_1> . <https://www.theworldavatar.com/kg/ontoems/DewPoint_1> <http://www.w3.org/1999/02/22-rdf-syntax-ns#type> <https://www.theworldavatar.com/kg/ontoems/DewPoint> . <https://www.theworldavatar.com/kg/ontoems/Forecast_1> <http://www.w3.org/1999/02/22-rdf-syntax-ns#type> <https://www.theworldavatar.com/kg/ontoems/Forecast> . <https://www.theworldavatar.com/kg/ontoems/Forecast_1> <http://www.ontology-of-units-of-measure.org/resource/om-2/hasUnit> <http://www.ontology-of-units-of-measure.org/resource/om-2/degreeCelsius> . <http://www.ontology-of-units-of-measure.org/resource/om-2/degreeCelsius> <http://www.w3.org/1999/02/22-rdf-syntax-ns#type> <http://www.ontology-of-units-of-measure.org/resource/om-2/Unit> . <http://www.ontology-of-units-of-measure.org/resource/om-2/degreeCelsius> <http://www.ontology-of-units-of-measure.org/resource/om-2/symbol> "&#x00B0;C"^^<http://www.w3.org/2001/XMLSchema#string> . <https://www.theworldavatar.com/kg/ontoems/DewPoint_1> <https://www.theworldavatar.com/kg/ontoems/hasForecastedValue> <https://www.theworldavatar.com/kg/ontoems/Forecast_1> .'

    # Mock call to uuid function
    m = mocker.patch('uuid.uuid4', return_value=str(1))
        
    station_iri = 'http://Station/1'

    # Perform test for observation without comment
    res = add_readings_for_station(station_iri, test_readings, is_observation=True)
    query = res[0]
    query = re.sub(r'\n', '', query)
    query = re.sub(r' +', ' ', query)
    query = query.strip()
    assert (expected_obs1_1 in query) and (expected_obs1_2 in query)
    assert len(res[1]) == 2
    assert res[2] == ['https://www.theworldavatar.com/kg/ontoems/Measure_1']*2

    # Perform test for observation with comment
    comments = ['test']*2
    res = add_readings_for_station(station_iri, test_readings, is_observation=True, 
                                   quantity_comments=comments)
    query = res[0]
    query = re.sub(r'\n', '', query)
    query = re.sub(r' +', ' ', query)
    query = query.strip()
    assert (expected_obs2_1 in query) and (expected_obs2_2 in query)
    assert len(res[1]) == 2
    assert res[2] == ['https://www.theworldavatar.com/kg/ontoems/Measure_1']*2

    # Perform test for forecast without comment
    res = add_readings_for_station(station_iri, test_readings, is_observation=False)
    query = res[0]
    query = re.sub(r'\n', '', query)
    query = re.sub(r' +', ' ', query)
    query = query.strip()
    assert (expected_fc1 in query) and (expected_fc2 in query)
    assert len(res[1]) == 2
    assert res[2] == ['https://www.theworldavatar.com/kg/ontoems/Forecast_1']*2
