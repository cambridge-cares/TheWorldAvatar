###############################################
# Authors: Markus Hofmeister (mh807cam.ac.uk) #    
# Date: 08 Apr 2022                           #
###############################################

import copy
import pytest

from metoffice.dataretrieval.stations import *
from metoffice.dataretrieval.readings import *
from metoffice.utils.properties import QUERY_ENDPOINT
from metoffice.flaskapp import create_app
from tests.utils import *

# Import modules under test from gasgridagent
from metoffice.datainstantiation.stations import *
from metoffice.datainstantiation.readings import *


@pytest.fixture
def client():
    app = create_app({'TESTING': True})
    with app.test_client() as client:
        yield client

@pytest.mark.skip(reason="only works as integration test with blank namespace in local blazegraph \
                          as well as blank RDB as defined in properties file")
def test_update_all_stations_webapp(client, mocker):
    # Integration test for expected behavior of updating all stations and readings
    # via webapp (requires (local) blazegraph running at endpoints specified
    # in 'metoffice.properties'; namespace MUST be empty)

    # Read test station data
    sites_data = read_readings_locations()
    readings_data = read_readings_timeseries()
    # Mock calls to Met Office DataPoint API
    def _side_effect1(*args):
        # Mock observation API calls
        if args[0] == metoffer.SITELIST:
            return copy.deepcopy(sites_data[0])
        elif args[0] == metoffer.ALL:
            return copy.deepcopy(readings_data[0].copy())
    metoffer.MetOffer.loc_observations = mocker.Mock(side_effect=_side_effect1)
    def _side_effect2(*args):
        # Mock forecast API calls
        if args[0] == metoffer.SITELIST:
            return copy.deepcopy(sites_data[1])
        elif args[0] == metoffer.ALL and metoffer.MetOffer.loc_forecast.call_count in [2,3]:
            # 3rd call is the first one to instantiate readings time series (before:
            # 1 call while instantiating stations and 2 for static readings triples)
            # return different readings for consecutive calls to mock
            return copy.deepcopy(readings_data[1])
        else:
            return copy.deepcopy(readings_data[2])
    metoffer.MetOffer.loc_forecast = mocker.Mock(side_effect=_side_effect2)

    # Verify that knowledge base is empty
    res = get_all_metoffice_station_ids(query_endpoint=QUERY_ENDPOINT)
    #assert len(res) == 0
   
    # Update all stations and readings
    route = '/api/metofficeagent/update/all'
    response = client.get(route)
    updated = response.json
    assert updated['stations'] == 6
    assert updated['readings'] == 50
    assert updated['timeseries'] == 50

    # Update all stations and readings
    route = '/api/metofficeagent/update/all'
    response = client.get(route)
    updated = response.json
    assert updated['stations'] == 0
    assert updated['readings'] == 1
    assert updated['timeseries'] == 51

    # Update all stations and readings
    route = '/api/metofficeagent/update/all'
    response = client.get(route)
    updated = response.json
    assert updated['stations'] == 0
    assert updated['readings'] == 0
    assert updated['timeseries'] == 51

    # Verify time series format
    ts_client = TSClient.tsclient_with_default_settings()
    # Get IRIs for timeseries to verify
    df = get_all_instantiated_forecast_timeseries(query_endpoint=QUERY_ENDPOINT)
    df = df[df['stationID'] == '25']
    # 1) Undistorted "FeelsLikeTemperature" time series
    dataIRI = df[df['reading'] == 'FeelsLikeTemperature']['dataIRI'].iloc[0]
    ts = ts_client.getTimeSeries([dataIRI])
    # 2) Distorted "RelativeHumidity" time series
    # 3) Distorted "AirTemperature" time series
    #ts1 df[]

    #query
