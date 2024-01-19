################################################
# Authors: Markus Hofmeister (mh807@cam.ac.uk) #    
# Date: 08 Apr 2022                            #
################################################

import copy
import pytest
from math import nan

from agent.dataretrieval.stations import *
from agent.dataretrieval.readings import *

from tests.conftest import *

# Import module(s) under test from agent
from agent.datainstantiation.stations import *
from agent.datainstantiation.readings import *


@pytest.mark.skip(reason="Only works as integration test with Blazegraph running at endpoint specified in `stack_configs_mock.py` file.\
                          Default settings in `stack_configs_mock.py` match provided `docker-compose.test.yml`")
def test_update_all_stations_webapp(clear_triple_store, clear_database, create_testing_agent, mocker):
    # Integration test for expected behavior of updating all stations and readings via Flask App
    
    # Mock Stack client initialisations
    mocker.patch('agent.kgutils.stackclients.PostGISClient.__init__', return_value=None)
    mocker.patch('agent.kgutils.stackclients.GdalClient.__init__', return_value=None)
    mocker.patch('agent.kgutils.stackclients.GeoserverClient.__init__', return_value=None)
    # Mock PostGIS client methods
    mocker.patch('agent.kgutils.stackclients.PostGISClient.check_table_exists', return_value=True)
    mocker.patch('agent.kgutils.stackclients.PostGISClient.check_point_feature_exists', return_value=True)

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
    res = get_number_of_triples()
    assert res == 0
   
    # Update all stations and readings
    route = '/api/metofficeagent/update/all'
    response = create_testing_agent.get(route)
    updated = response.json
    assert updated['stations'] == 6
    assert updated['readings'] == 50
    assert updated['reading_timeseries'] == 50
    # Verify creation of 3 observation and 3 forecast time series (1 per station)
    obs = get_instantiated_observation_timeseries()
    assert len(obs['tsIRI'].unique()) == 3
    fcs = get_instantiated_forecast_timeseries()
    assert len(fcs['tsIRI'].unique()) == 3

    # Update all stations and readings
    route = '/api/metofficeagent/update/all'
    response = create_testing_agent.get(route)
    updated = response.json
    assert updated['stations'] == 0
    assert updated['readings'] == 1
    assert updated['reading_timeseries'] == 51
    # Verify creation of additional forecast time series (for initially missing AirTemperature)    
    obs = get_instantiated_observation_timeseries()
    assert len(obs['tsIRI'].unique()) == 3
    fcs = get_instantiated_forecast_timeseries()
    assert len(fcs['tsIRI'].unique()) == 4

    # Update all stations and readings
    route = '/api/metofficeagent/update/all'
    response = create_testing_agent.get(route)
    updated = response.json
    assert updated['stations'] == 0
    assert updated['readings'] == 0
    assert updated['reading_timeseries'] == 51

    # Get test reference data (forecast for station 25)
    data = readings_dict_gen(copy.deepcopy(readings_data[2]))
    data = {key: condition_readings_data(data[key], False) for key in data}
    data = data['25']
    times1 = data['times']
    cutoff_time = data['times'][10]
    times2 = data['times'][10:]

    # Test correct retrieval of data for 4 different time series
    # "FeelsLikeTemperature": no missing data
    # "Visibility": one data point missing
    # "WindSpeed": two data points missing
    # "RelativeHumidity": all data points except first missing

    # Get time series data lists and extract first elements (only one time series)
    station_iri = [fcs.loc[fcs['stationID'] == '25', 'station'].unique()[0]]
    ts_data1, ts_names1, ts_units1 = get_time_series_data(station_iri, 
                                     ['FeelsLikeTemperature', 'Visibility', 'WindSpeed','RelativeHumidity'])    
    ts1 = ts_data1[0]
    names1 = ts_names1[0]
    units1 = ts_units1[0]
    ts_data2, ts_names2, ts_units2 = get_time_series_data(station_iri, 
                                     ['FeelsLikeTemperature', 'Visibility', 'WindSpeed','RelativeHumidity'],
                                     tmin=cutoff_time)
    ts2 = ts_data2[0]
    names2 = ts_names2[0]
    units2 = ts_units2[0]
    assert names1 == names2
    assert units1 == units2
    # Get time readings from Java time series object
    times_instant1 = ts1.getTimes()
    times_unix1 = [t.getEpochSecond() for t in times_instant1]
    times_string1 = [dt.datetime.utcfromtimestamp(t).strftime(TIME_FORMAT) for t in times_unix1]
    assert times_string1 == times1
    times_instant2 = ts2.getTimes()
    times_unix2 = [t.getEpochSecond() for t in times_instant2]
    times_string2 = [dt.datetime.utcfromtimestamp(t).strftime(TIME_FORMAT) for t in times_unix2]
    assert times_string2 == times2

    # 1) "FeelsLikeTemperature"
    reading = 'FeelsLikeTemperature'
    val1 = data['readings'][reading]
    val2 = data['readings'][reading][10:]
    # Verify that ts names are retrieved correctly
    assert (reading + ' forecast') in names1.values()
    # Get dataIRI
    dataIRI = list(names1.keys())[list(names1.values()).index(reading + ' forecast')]
    # Verify unit is retrieved correctly
    assert units1[dataIRI] == '&#x00B0;C'
    # Verify retrieved values
    val_retrieved1 = list(ts1.getValues(dataIRI))
    assert val1 == val_retrieved1
    val_retrieved2 = list(ts2.getValues(dataIRI))
    assert val2 == val_retrieved2

    # 2) "Visibility"
    reading = 'Visibility'
    val1 = data['readings'][reading]
    val2 = data['readings'][reading][10:]
    # Missing data is indicated as nan before populating the RDB and returned
    # as None using the TS Client --> replace nan's with None for comparison
    val1 = list(pd.Series(val1).replace({nan: None}).values)
    val2 = list(pd.Series(val2).replace({nan: None}).values)
    # Verify that ts names are retrieved correctly
    assert (reading + ' forecast') in names1.values()
    # Get dataIRI
    dataIRI = list(names1.keys())[list(names1.values()).index(reading + ' forecast')]
    # Verify unit is retrieved correctly
    assert units1[dataIRI] == 'm'
    # Verify retrieved values
    val_retrieved1 = list(ts1.getValues(dataIRI))
    assert val1 == val_retrieved1
    val_retrieved2 = list(ts2.getValues(dataIRI))
    assert val2 == val_retrieved2

    # 3) "WindSpeed"
    reading = 'WindSpeed'
    val1 = data['readings'][reading]
    val2 = data['readings'][reading][10:]
    # Missing data is indicated as nan before populating the RDB and returned
    # as None using the TS Client --> replace nan's with None for comparison
    val1 = list(pd.Series(val1).replace({nan: None}).values)
    val2 = list(pd.Series(val2).replace({nan: None}).values)
    # Verify that ts names are retrieved correctly
    assert (reading + ' forecast') in names1.values()
    # Get dataIRI
    dataIRI = list(names1.keys())[list(names1.values()).index(reading + ' forecast')]
    # Verify unit is retrieved correctly
    assert units1[dataIRI] == 'mi/h'
    # Verify retrieved values
    val_retrieved1 = list(ts1.getValues(dataIRI))
    assert val1 == val_retrieved1
    val_retrieved2 = list(ts2.getValues(dataIRI))
    assert val2 == val_retrieved2

    # 4) "RelativeHumidity"
    reading = 'RelativeHumidity'
    val1 = data['readings'][reading]
    val2 = data['readings'][reading][10:]
    # Missing data is indicated as nan before populating the RDB and returned
    # as None using the TS Client --> replace nan's with None for comparison
    val1 = list(pd.Series(val1).replace({nan: None}).values)
    val2 = list(pd.Series(val2).replace({nan: None}).values)
    # Verify that ts names are retrieved correctly
    assert (reading + ' forecast') in names1.values()
    # Get dataIRI
    dataIRI = list(names1.keys())[list(names1.values()).index(reading + ' forecast')]
    # Verify unit is retrieved correctly
    assert units1[dataIRI] == '%'
    # Verify retrieved values
    val_retrieved1 = list(ts1.getValues(dataIRI))
    assert val1 == val_retrieved1
    val_retrieved2 = list(ts2.getValues(dataIRI))
    assert val2 == val_retrieved2

    # Verify that behavior wrt missing data is the same when time series 
    # is retrieved individually
    ts_data3, ts_names3, ts_units3 = get_time_series_data(station_iri, ['WindSpeed'])
    ts3 = ts_data3[0]
    names3 = ts_names3[0]
    units3 = ts_units3[0]
    # Verify times
    times_instant3 = ts3.getTimes()
    times_unix3 = [t.getEpochSecond() for t in times_instant3]
    times_string3 = [dt.datetime.utcfromtimestamp(t).strftime(TIME_FORMAT) for t in times_unix3]
    assert times_string3 == times1
    # Verify readings
    reading = 'WindSpeed'
    val1 = data['readings'][reading]
    val2 = data['readings'][reading][10:]
    # Missing data is indicated as nan before populating the RDB and returned
    # as None using the TS Client --> replace nan's with None for comparison
    val1 = list(pd.Series(val1).replace({nan: None}).values)
    val2 = list(pd.Series(val2).replace({nan: None}).values)
    # Verify that ts names are retrieved correctly
    assert (reading + ' forecast') in names3.values()
    # Get dataIRI
    dataIRI = list(names3.keys())[list(names3.values()).index(reading + ' forecast')]
    # Verify unit is retrieved correctly
    assert units3[dataIRI] == 'mi/h'
    # Verify retrieved values
    val_retrieved1 = list(ts3.getValues(dataIRI))
    assert val1 == val_retrieved1
