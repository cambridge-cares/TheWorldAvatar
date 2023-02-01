################################################
# Authors: Markus Hofmeister (mh807@cam.ac.uk) #    
# Date: 04 Apr 2022                            #
################################################

import time
import copy
import datetime as dt
import pytest
from testcontainers.core.container import DockerContainer

from airquality.dataretrieval.stations import *
from airquality.dataretrieval.readings import *
from airquality.errorhandling.exceptions import APIException
from airquality.utils.properties import QUERY_ENDPOINT
from airquality.flaskapp import create_app
from tests.utils import *

# Import modules under test from gasgridagent
from airquality.datainstantiation.stations import *
from airquality.datainstantiation.readings import *


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
    data1 = [station_data[0]]
    data2 = [station_data[0], station_data[1]]
    data3 = [station_data[2]]

    # Spin up temporary docker container
    with initialise_triple_store as container:
        # Wait some arbitrary time until container is reachable
        time.sleep(3)
        # Retrieve SPARQL endpoint
        endpoint = get_sparql_endpoint(container)
        create_blazegraph_namespace(endpoint)

        # Verify that knowledge base is empty
        res = get_all_airquality_stations(query_endpoint=endpoint)
        assert len(res) == 0

        # Instantiate first station  
        instantiate_stations(data1, query_endpoint=endpoint, update_endpoint=endpoint)      
        res = get_all_airquality_station_ids(query_endpoint=endpoint)
        assert res[0] == list(data1[0].keys())[0]
        triples = get_number_of_triples(endpoint)
        assert triples == 6

        # Instantiate second station   
        instantiate_stations(data2, query_endpoint=endpoint, update_endpoint=endpoint)           
        res = get_all_airquality_station_ids(query_endpoint=endpoint)
        assert len(res) == 3
        triples = get_number_of_triples(endpoint)
        assert triples == 17

        # Instantiate third station   
        instantiate_stations(data3, query_endpoint=endpoint, update_endpoint=endpoint)           
        res = get_all_airquality_station_ids(query_endpoint=endpoint)
        assert len(res) == 4
        triples = get_number_of_triples(endpoint)
        assert triples == 21


def test_instantiate_all_stations(initialise_triple_store, mocker):

    # Read test station data
    station_data = read_station_data()
    # Mock call to Met Office DataPoint API
    m = mocker.patch('airquality.datainstantiation.stations.retrieve_station_data_from_api',
                     return_value=station_data)

    # Spin up temporary docker container
    with initialise_triple_store as container:
        # Wait some arbitrary time until container is reachable
        time.sleep(3)
        # Retrieve SPARQL endpoint
        endpoint = get_sparql_endpoint(container)
        create_blazegraph_namespace(endpoint)

        # Verify that knowledge base is empty
        res = get_all_airquality_station_ids(query_endpoint=endpoint)
        assert len(res) == 0

        # Instantiate all stations
        instantiate_all_stations(query_endpoint=endpoint,
                                 update_endpoint=endpoint)
        # Verify that data gets added
        res = get_all_airquality_station_ids(query_endpoint=endpoint)
        assert len(res) == 3
        triples = get_number_of_triples(endpoint)
        assert triples == 15

        # Instantiate all stations
        instantiate_all_stations(query_endpoint=endpoint,
                                 update_endpoint=endpoint)
        # Verify that same data does not get added twice
        res = get_all_airquality_station_ids(query_endpoint=endpoint)
        assert len(res) == 3
        triples = get_number_of_triples(endpoint)
        assert triples == 15


@pytest.mark.skip(reason="only works as integration test with blank namespace in local blazegraph \
                          as TimeSeriesClient reads required inputs directly from properties file")
def test_instantiate_all_stations_webapp(client, mocker):
    # Integration test for expected behavior of instantiation of all stations
    # via webapp (requires (local) blazegraph running at endpoints specified
    # in 'airquality.properties'; namespace MUST be empty)

    # Read test station data
    station_data = read_station_data()
    # Mock call to Met Office DataPoint API
    m = mocker.patch('airquality.datainstantiation.stations.retrieve_station_data_from_api',
                     return_value=station_data)

    # Verify that knowledge base is empty
    res = get_all_airquality_station_ids(query_endpoint=QUERY_ENDPOINT)
    assert len(res) == 0
   
    # Instantiate all stations
    route = '/api/airqualityagent/instantiate/stations'
    response = client.get(route)
    new_stations = response.json['stations']
    assert new_stations == 3

    # Instantiate all stations (2nd time)
    route = '/api/airqualityagent/instantiate/stations'
    response = client.get(route)
    new_stations = response.json['stations']
    assert new_stations == 0


def test_add_readings_for_station(mocker):

    # Read test readings data
    test_readings = read_readings_timeseries()
     
    # Expected result
    expected_obs1 = '<http://Station/1> <https://www.theworldavatar.com/kg/ontoems/reports> <https://www.theworldavatar.com/kg/ontoems/NitrogenDioxideConcentration_1> . <https://www.theworldavatar.com/kg/ontoems/NitrogenDioxideConcentration_1> <http://www.w3.org/1999/02/22-rdf-syntax-ns#type> <https://www.theworldavatar.com/kg/ontoems/NitrogenDioxideConcentration> . <https://www.theworldavatar.com/kg/ontoems/NitrogenDioxideConcentration_1> <http://www.w3.org/2000/01/rdf-schema#comment> "nitrogen dioxide in air"^^<http://www.w3.org/2001/XMLSchema#string> . <https://www.theworldavatar.com/kg/ontoems/NitrogenDioxideConcentration_1> <http://www.ontology-of-units-of-measure.org/resource/om-2/hasValue> <https://www.theworldavatar.com/kg/ontoems/Measure_1> . <https://www.theworldavatar.com/kg/ontoems/Measure_1> <http://www.w3.org/1999/02/22-rdf-syntax-ns#type> <http://www.ontology-of-units-of-measure.org/resource/om-2/Measure> . <https://www.theworldavatar.com/kg/ontoems/Measure_1> <http://www.ontology-of-units-of-measure.org/resource/om-2/hasUnit> <https://www.theworldavatar.com/kg/ontouom/microgramPerCubicMetre> . <https://www.theworldavatar.com/kg/ontouom/microgramPerCubicMetre> <http://www.w3.org/1999/02/22-rdf-syntax-ns#type> <http://www.ontology-of-units-of-measure.org/resource/om-2/Unit> . <https://www.theworldavatar.com/kg/ontouom/microgramPerCubicMetre> <http://www.ontology-of-units-of-measure.org/resource/om-2/symbol> "μg/m3"^^<http://www.w3.org/2001/XMLSchema#string> . <https://www.theworldavatar.com/kg/ontoems/NitrogenDioxideConcentration_1> <http://www.w3.org/2002/07/owl#sameAs> <http://dd.eionet.europa.eu/vocabulary/aq/pollutant/8>'
    expected_obs2 = '<http://Station/1> <https://www.theworldavatar.com/kg/ontoems/reports> <https://www.theworldavatar.com/kg/ontoems/NitrogenOxidesConcentration_1> . <https://www.theworldavatar.com/kg/ontoems/NitrogenOxidesConcentration_1> <http://www.w3.org/1999/02/22-rdf-syntax-ns#type> <https://www.theworldavatar.com/kg/ontoems/NitrogenOxidesConcentration> . <https://www.theworldavatar.com/kg/ontoems/NitrogenOxidesConcentration_1> <http://www.w3.org/2000/01/rdf-schema#comment> "nitrogen oxides in air"^^<http://www.w3.org/2001/XMLSchema#string> . <https://www.theworldavatar.com/kg/ontoems/NitrogenOxidesConcentration_1> <http://www.ontology-of-units-of-measure.org/resource/om-2/hasValue> <https://www.theworldavatar.com/kg/ontoems/Measure_1> . <https://www.theworldavatar.com/kg/ontoems/Measure_1> <http://www.w3.org/1999/02/22-rdf-syntax-ns#type> <http://www.ontology-of-units-of-measure.org/resource/om-2/Measure> . <https://www.theworldavatar.com/kg/ontoems/Measure_1> <http://www.ontology-of-units-of-measure.org/resource/om-2/hasUnit> <https://www.theworldavatar.com/kg/ontouom/microgramPerCubicMetre> . <https://www.theworldavatar.com/kg/ontouom/microgramPerCubicMetre> <http://www.w3.org/1999/02/22-rdf-syntax-ns#type> <http://www.ontology-of-units-of-measure.org/resource/om-2/Unit> . <https://www.theworldavatar.com/kg/ontouom/microgramPerCubicMetre> <http://www.ontology-of-units-of-measure.org/resource/om-2/symbol> "μg/m3"^^<http://www.w3.org/2001/XMLSchema#string> . <https://www.theworldavatar.com/kg/ontoems/NitrogenOxidesConcentration_1> <http://www.w3.org/2002/07/owl#sameAs> <http://dd.eionet.europa.eu/vocabulary/aq/pollutant/9>'
    expected_obs3 = '<http://Station/1> <https://www.theworldavatar.com/kg/ontoems/reports> <https://www.theworldavatar.com/kg/ontoems/PM10Concentration_1> . <https://www.theworldavatar.com/kg/ontoems/PM10Concentration_1> <http://www.w3.org/1999/02/22-rdf-syntax-ns#type> <https://www.theworldavatar.com/kg/ontoems/PM10Concentration> . <https://www.theworldavatar.com/kg/ontoems/PM10Concentration_1> <http://www.w3.org/2000/01/rdf-schema#comment> "pm10 in aerosol"^^<http://www.w3.org/2001/XMLSchema#string> . <https://www.theworldavatar.com/kg/ontoems/PM10Concentration_1> <http://www.ontology-of-units-of-measure.org/resource/om-2/hasValue> <https://www.theworldavatar.com/kg/ontoems/Measure_1> . <https://www.theworldavatar.com/kg/ontoems/Measure_1> <http://www.w3.org/1999/02/22-rdf-syntax-ns#type> <http://www.ontology-of-units-of-measure.org/resource/om-2/Measure> . <https://www.theworldavatar.com/kg/ontoems/Measure_1> <http://www.ontology-of-units-of-measure.org/resource/om-2/hasUnit> <https://www.theworldavatar.com/kg/ontouom/microgramPerCubicMetre> . <https://www.theworldavatar.com/kg/ontouom/microgramPerCubicMetre> <http://www.w3.org/1999/02/22-rdf-syntax-ns#type> <http://www.ontology-of-units-of-measure.org/resource/om-2/Unit> . <https://www.theworldavatar.com/kg/ontouom/microgramPerCubicMetre> <http://www.ontology-of-units-of-measure.org/resource/om-2/symbol> "μg/m3"^^<http://www.w3.org/2001/XMLSchema#string> . <https://www.theworldavatar.com/kg/ontoems/PM10Concentration_1> <http://www.w3.org/2002/07/owl#sameAs> <http://dd.eionet.europa.eu/vocabulary/aq/pollutant/5>'

    # Mock call to uuid function
    m = mocker.patch('uuid.uuid4', return_value=str(1))
        
    station_iri = 'http://Station/1'

    # Perform test for observation without comment
    res = add_readings_for_station(station_iri, test_readings)
    query = res[0]
    query = re.sub(r'\n', '', query)
    query = re.sub(r' +', ' ', query)
    query = query.strip()
    assert (expected_obs1 in query) and (expected_obs2 in query) and (expected_obs3 in query)
    assert len(res[1]) == 3
    assert res[2] == ['https://www.theworldavatar.com/kg/ontoems/Measure_1']*3
