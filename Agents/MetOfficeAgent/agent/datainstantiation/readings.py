################################################
# Authors: Markus Hofmeister (mh807@cam.ac.uk) #    
# Date: 05 Apr 2022                            #
################################################

# The purpose of this module is to provide functions to retrieve 
# readings data from the API and instantiate it in the KG

import metoffer
import time
import uuid
import datetime as dt
from math import nan

from py4jps import agentlogging
from agent.datainstantiation.stations import *
from agent.dataretrieval.readings import *
from agent.dataretrieval.stations import *
from agent.errorhandling.exceptions import APIException
from agent.kgutils.kgclient import KGClient
from agent.kgutils.querytemplates import *
from agent.kgutils.tsclient import TSClient
from agent.utils.env_configs import DATAPOINT_API_KEY
from agent.utils.readings_mapping import (COMPASS, DATACLASS, READINGS_MAPPING,
                                          TIME_FORMAT, UNITS_MAPPING,
                                          VISIBILITY)
from agent.utils.stack_configs import (DB_PASSWORD, DB_URL, DB_USER,
                                       QUERY_ENDPOINT, UPDATE_ENDPOINT)

# Initialise logger
logger = agentlogging.get_logger("prod")


def add_readings_timeseries(instantiated_ts_iris: list = None,
                            api_key: str = DATAPOINT_API_KEY,
                            query_endpoint: str = QUERY_ENDPOINT,
                            update_endpoint: str = UPDATE_ENDPOINT) -> int:
    """
        Adds time series data to instantiated time series IRIs
        
        Arguments:
            instantiated_ts_iris - list of IRIs of instantiated time series
    """

    def _create_ts_subsets_to_add(times, dataIRIs, values):
        # TimeSeriesClient faces issues to add data for all dataIRIs at once,
        # if some dataIRIs contain missing values (None, nan); hence, create
        # subsets of data to add without any missing entries
        df = pd.DataFrame(index=times, data=dict(zip(dataIRIs, values)))
        df[df.columns[~df.isnull().any()]]
        non_nulls = df[df.columns[~df.isnull().any()]]
        # Initialise return lists with data for non null quantities
        times = [] if non_nulls.empty else [non_nulls.index.to_list()]
        dataIRIs = [] if non_nulls.empty else [non_nulls.columns.to_list()]
        values = [] if non_nulls.empty else [[non_nulls[c].values.tolist() for c in non_nulls.columns]]
        some_nulls = [c for c in list(df.columns) if c not in list(non_nulls.columns)]
        for c in some_nulls:
            sub = df[c]
            sub.dropna(inplace=True)
            dataIRIs.append([c])
            times.append(sub.index.to_list())
            values.append([sub.values.tolist()])
        
        return times, dataIRIs, values  


    # Create MetOffice client to retrieve readings via API
    try:
        metclient = metoffer.MetOffer(api_key)
    except Exception as ex:
        logger.error("MetOffer client could not be created to retrieve station readings.")
        raise APIException("MetOffer client could not be created to retrieve station readings.") from ex
    
    # Load available observations and forecasts from API
    #print('Retrieving time series data from API ...')
    logger.info('Retrieving time series data from API ...')
    available_obs, available_fcs, issue_time = retrieve_readings_data_per_station(metclient, only_keys=False)
    #print('Time series data successfully retrieved.')
    logger.info('Time series data successfully retrieved.')
    
    # Retrieve information about instantiated time series from KG
    #print('Retrieving time series triples from KG ...')
    logger.info('Retrieving time series triples from KG ...')
    instantiated_obs = get_instantiated_observation_timeseries(query_endpoint=query_endpoint,
                                                               update_endpoint=update_endpoint)
    instantiated_fcs = get_instantiated_forecast_timeseries(query_endpoint=query_endpoint,
                                                            update_endpoint=update_endpoint)
    #print('Time series triples successfully retrieved.')
    logger.info('Time series triples successfully retrieved.')

    # Keep only the relevant subset for instantiated_ts_iris
    if instantiated_ts_iris:
        instantiated_obs = instantiated_obs[instantiated_obs['tsIRI'].isin(instantiated_ts_iris)]
        instantiated_fcs = instantiated_fcs[instantiated_fcs['tsIRI'].isin(instantiated_ts_iris)]
    # Get short version of variable type from full quantity type
    instantiated_obs['reading'] = instantiated_obs['quantityType'].apply(lambda x: x.split('/')[-1])
    instantiated_fcs['reading'] = instantiated_fcs['quantityType'].apply(lambda x: x.split('/')[-1])   

    # Initialise update query for creation time
    query_string = update_forecast_creation_datetime(issue_time)

    # Initialise KG and TimeSeries Clients
    kg_client = KGClient(query_endpoint, update_endpoint)
    ts_client = TSClient(kg_client=kg_client, rdb_url=DB_URL, rdb_user=DB_USER, 
                         rdb_password=DB_PASSWORD)

    added_obs = 0
    added_fcs = 0
    
    # Loop through all observation timeseries
    #print('Adding observation time series data ...')
    logger.info('Adding observation time series data ...')
    ts_list = []
    for tsiri in list(instantiated_obs['tsIRI'].unique()): 
        # Extract relevant data      
        data = instantiated_obs[instantiated_obs['tsIRI'] == tsiri]
        station_id = data['stationID'].iloc[0]
        # Construct time series object to be added (skip if previously
        # instantiated time series not present in latest retrieved data)
        if station_id in available_obs.keys():
            times = available_obs[station_id]['times']
            dataIRIs = data['dataIRI'].to_list()
            # Get instantiated, but potentially missing quantity for reported interval
            # and fill missing values with nans
            missing_data = [i for i in data['reading'].to_list() if i not in available_obs[station_id]['readings'].keys()]
            missing = dict(zip(missing_data, [[nan]*len(times)]*len(missing_data)))
            readings = {**available_obs[station_id]['readings'], **missing}
            values = [readings[i] for i in data['reading'].to_list()]
            # Potentially split time series data addition if None/nan exist in some readings
            times_list, dataIRIs_list, values_list = _create_ts_subsets_to_add(times, dataIRIs, values)
            for i in range(len(times_list)):
                added_obs += len(dataIRIs_list[i])
                ts = TSClient.create_timeseries(times_list[i], dataIRIs_list[i], values_list[i])
                ts_list.append(ts)
    with ts_client.connect() as conn:
        ts_client.tsclient.bulkaddTimeSeriesData(ts_list, conn)
    #print(f'Time series data for {added_obs} observations successfully added to KG.')
    logger.info(f'Time series data for {added_obs} observations successfully added to KG.')
    
    # Loop through all forecast timeseries
    #print('Adding forecast time series data ...')
    logger.info('Adding forecast time series data ...')
    ts_list = []
    for tsiri in list(instantiated_fcs['tsIRI'].unique()):  
        # Extract relevant data      
        data = instantiated_fcs[instantiated_fcs['tsIRI'] == tsiri]
        station_id = data['stationID'].iloc[0]
        # Construct time series object to be added (skip if previously
        # instantiated time series not present in latest retrieved data)
        if station_id in available_fcs.keys():
            times = available_fcs[station_id]['times']
            dataIRIs = data['dataIRI'].to_list()
            # Get instantiated, but potentially missing quantity for reported interval
            # and fill missing values with nans
            missing_data = [i for i in data['reading'].to_list() if i not in available_fcs[station_id]['readings'].keys()]
            missing = dict(zip(missing_data, [[nan]*len(times)]*len(missing_data)))
            readings = {**available_fcs[station_id]['readings'], **missing}
            values = [readings[i] for i in data['reading'].to_list()]
            # Potentially split time series data addition if None/nan exist in some readings
            times_list, dataIRIs_list, values_list = _create_ts_subsets_to_add(times, dataIRIs, values)
            for i in range(len(times_list)):
                added_fcs += len(dataIRIs_list[i])            
                ts = TSClient.create_timeseries(times_list[i], dataIRIs_list[i], values_list[i])
                ts_list.append(ts)
                for iri in dataIRIs_list[i]:
                    query_string += f"<{iri}> , "
    with ts_client.connect() as conn:
        ts_client.tsclient.bulkaddTimeSeriesData(ts_list, conn)
    #print(f'Time series data for {added_fcs} forecasts successfully added.')
    logger.info(f'Time series data for {added_fcs} forecasts successfully added.')

    # Strip trailing comma and close & perform creation date update query
    query_string = query_string[:-2]
    query_string += f") ) }}"
    kg_client.performUpdate(query_string)
    logger.info('Creation time triples successfully updated.')

    return added_obs + added_fcs


def add_all_readings_timeseries(api_key: str = DATAPOINT_API_KEY,
                                query_endpoint: str = QUERY_ENDPOINT,
                                update_endpoint: str = UPDATE_ENDPOINT) -> int:
    """
        Adds latest time series readings for all instantiated time series
    """

    updated_ts = add_readings_timeseries(api_key=api_key,
                                         query_endpoint=query_endpoint,
                                         update_endpoint=update_endpoint)

    return updated_ts


def instantiate_station_readings(instantiated_sites_list: list,
                                 api_key: str = DATAPOINT_API_KEY,
                                 query_endpoint: str = QUERY_ENDPOINT,
                                 update_endpoint: str = UPDATE_ENDPOINT) -> int:
    """
        Instantiates readings for the provided list of measurement stations
        
        Arguments:
            instantiated_sites_list - list of dictionaries with instantiated
                                      stations/sites in the form [{id : iri},]
    """

    # Create MetOffice client to retrieve readings via API
    try:
        metclient = metoffer.MetOffer(api_key)
    except Exception as ex:
        logger.error("MetOffer client could not be created to retrieve station readings. " + ex)
        raise APIException("MetOffer client could not be created to retrieve station readings.") from ex
    
    # Initialise update query
    triples = f""

    # Initialise lists for TimeSeriesClient's bulkInit function
    dataIRIs = []
    dataClasses = []
    timeUnit = []

    # Get already instantiated observations and forecasts (across all stations)
    #print('Retrieving instantiated observation/forecast triples from KG ...')
    logger.info('Retrieving instantiated observation/forecast triples from KG ...')
    instantiated_obs = get_instantiated_observations(query_endpoint=query_endpoint, 
                                                     update_endpoint=update_endpoint)
    instantiated_fcs = get_instantiated_forecasts(query_endpoint=query_endpoint, 
                                                  update_endpoint=update_endpoint)
    #print('Observation/forecast triples successfully retrieved.')
    logger.info('Observation/forecast triples successfully retrieved.')

    # Get short version of variable type from full quantity type
    instantiated_obs['reading'] = instantiated_obs['quantityType'].apply(lambda x: x.split('/')[-1])
    instantiated_fcs['reading'] = instantiated_fcs['quantityType'].apply(lambda x: x.split('/')[-1])                                                        

    # Load available observations and forecasts from API
    #print('Retrieving available observations/forecasts from API ...')
    logger.info('Retrieving available observations/forecasts from API ...')
    available_obs, available_fcs, _ = retrieve_readings_data_per_station(metclient)
    #print('Available observations/forecasts successfully retrieved.')
    logger.info('Available observations/forecasts successfully retrieved.')

    # Initialise number of instantiated readings
    instantiated = 0

    # Loop over all sites   
    #print('Create triples to instantiate static observation/forecast information ...')
    logger.info('Create triples to instantiate static observation/forecast information ...')
    for id in instantiated_sites_list:
        
        # Get lists of instantiated readings for current station
        inst_obs = instantiated_obs[instantiated_obs['stationID'] == id]['reading'].tolist()
        inst_fcs = instantiated_fcs[instantiated_fcs['stationID'] == id]['reading'].tolist()

        # Get available observations and forecasts for that station
        try:
            avail_obs = available_obs[id]
        except KeyError:
            # In case no observation data is available for instantiated station
            avail_obs = []
        try:
            avail_fcs = available_fcs[id]
        except KeyError:
            # In case no forecast data is available for instantiated station
            avail_fcs = []
        
        # Derive quantities to instantiate
        obs = [i for i in avail_obs if i not in inst_obs]
        fcs = [i for i in avail_fcs if i not in inst_fcs]
        both = [i for i in obs if i in fcs]
        obs = list(set(obs) - set(both))
        fcs = list(set(fcs) - set(both))

        # Get station IRI
        station_iri = instantiated_sites_list[id]

        # Initialise
        triples1, triples2, triples3, triples4 = '', '', '', ''
        dataIRIs1, dataIRIs2, dataIRIs3, dataIRIs4 = [], [], [], []
        dataClasses1, dataClasses2, dataClasses3, dataClasses4 = [], [], [], []
        timeUnit3, timeUnit4 = None, None

        # Create triples and input lists for TimeSeriesClient bulkInit
        if obs or fcs or both:
            if both:
                triples1, reading_iris, dataIRIs1, dataClasses1, _ = add_readings_for_station(station_iri, both, is_observation=True)
                triples2, _, dataIRIs2, dataClasses2, _ = add_readings_for_station(station_iri, both, reading_iris, is_observation=False)
                instantiated += len(both)
            if obs:
                triples3, _, dataIRIs3, dataClasses3, timeUnit3 = add_readings_for_station(station_iri, obs, is_observation=True)
                instantiated += len(obs)
            if fcs:
                triples4, _, dataIRIs4, dataClasses4, timeUnit4 = add_readings_for_station(station_iri, fcs, is_observation=False)
                instantiated += len(fcs)

            # Add triples to INSERT DATA query
            triples += triples1
            triples += triples2
            triples += triples3
            triples += triples4

            # Append lists to overarching TimeSeriesClient input lists
            if dataIRIs1 + dataIRIs3:
                dataIRIs.append(dataIRIs1 + dataIRIs3)
                dataClasses.append(dataClasses1 + dataClasses3)
                timeUnit.append(timeUnit3)
            if dataIRIs2 + dataIRIs4:
                dataIRIs.append(dataIRIs2 + dataIRIs4)
                dataClasses.append(dataClasses2 + dataClasses4)
                timeUnit.append(timeUnit4)

            logger.info(f'Readings for station {id:>6} successfully added to query.')

    # Split triples to instantiate into several chunks of max size
    queries = split_insert_query(triples, max=100000)

    # Instantiate all non-time series triples
    kg_client = KGClient(query_endpoint, update_endpoint)
    # Perform SPARQL update query in chunks to avoid heap size/memory issues
    #print(f'Instantiate static observation/forecast triples in {len(queries)} chunks ...')
    logger.info(f'Instantiate static observation/forecast triples in {len(queries)} chunks ...')
    for query in queries:
        kg_client.performUpdate(query)
    #print('Observations/forecasts successfully instantiated/updated.')
    logger.info('Insert query successfully performed.')

    if dataIRIs:
        #print('Instantiate static time series triples ...')
        logger.info('Instantiate static time series triples ...')
        # Instantiate all time series triples
        ts_client = TSClient(kg_client=kg_client, rdb_url=DB_URL, rdb_user=DB_USER, 
                             rdb_password=DB_PASSWORD)
        with ts_client.connect() as conn:
            ts_client.tsclient.bulkInitTimeSeries(dataIRIs, dataClasses, timeUnit, conn)
        #print('Time series triples successfully added.')
        logger.info('Time series triples successfully added.')

    return instantiated


def instantiate_all_station_readings(api_key: str = DATAPOINT_API_KEY,
                                     query_endpoint: str = QUERY_ENDPOINT,
                                     update_endpoint: str = UPDATE_ENDPOINT) -> int:
    """
        Instantiates all readings for all instantiated stations
    """

    stations = get_all_metoffice_stations(query_endpoint=query_endpoint,
                                          update_endpoint=update_endpoint)
    
    instantiated = instantiate_station_readings(instantiated_sites_list=stations,
                                                api_key=api_key, 
                                                query_endpoint=query_endpoint,
                                                update_endpoint=update_endpoint)

    return instantiated


def update_all_stations(api_key: str = DATAPOINT_API_KEY,
                        query_endpoint: str = QUERY_ENDPOINT,
                        update_endpoint: str = UPDATE_ENDPOINT):

    # Instantiate all available stations (ONLY not already existing stations
    # will be newly instantiated)
    print('\nUpdate instantiated stations: ')
    logger.info('Update instantiated stations ...')
    t1 = time.time()
    new_stations = instantiate_all_stations(api_key, query_endpoint, update_endpoint)
    t2 = time.time()
    diff = t2-t1
    print(f'Finished after: {diff//60:5>n} min, {diff%60:4.2f} s \n')
    logger.info(f'Finished after: {diff//60:5>n} min, {diff%60:4.2f} s \n')


    # Instantiate all available station readings (ONLY not already existing
    # readings will be newly instantiated)
    print('\nUpdate instantiated station readings: ')
    logger.info('Update instantiated station readings ...')
    t1 = time.time()
    new_readings = instantiate_all_station_readings(api_key, query_endpoint, update_endpoint)
    t2 = time.time()
    diff = t2-t1
    print(f'Finished after: {diff//60:5>n} min, {diff%60:4.2f} s \n')
    logger.info(f'Finished after: {diff//60:5>n} min, {diff%60:4.2f} s \n')

    # Add latest readings time series to instantiated reading quantities
    print('\nUpdate station readings time series data: ')
    logger.info('Update station readings time series data ...')
    t1 = time.time()
    updated_ts = add_all_readings_timeseries(api_key, query_endpoint, update_endpoint)
    t2 = time.time()
    diff = t2-t1
    print(f'Finished after: {diff//60:5>n} min, {diff%60:4.2f} s \n')
    logger.info(f'Finished after: {diff//60:5>n} min, {diff%60:4.2f} s \n')

    return new_stations, new_readings, updated_ts


def add_readings_for_station(station_iri: str,
                             readings: list, readings_iris: list = None, 
                             is_observation: bool = True,
                             quantity_comments: list = None):
    """
        Return SPARQL update query string to instantiate readings for given 
        station IRI (query string to be included in overarching INSERT DATA query)

        Arguments:
            station_iri - Station IRI without trailing '<' and '>'
            readings - list of station readings to instantiate
                       (i.e. OntoEMS concept names)
            readings_iris - list of IRIs for station readings (only relevant to
                            link observation and forecast readings for same quantity
                            to same instance instead of creating duplicates)
            is_observation - boolean to indicate whether readings are measure
                             or forecast
            quantity_comments - comments to be attached to quantities
        
        Returns
            triples - triples to be added to INSERT DATA query
            created_reading_iris - list of newly created quantity IRIs
            dataIRIs, dataClasses, timeUnit - to be appended to input arguments
                                              to TimSeriesClient bulkInit call
    """

    if readings_iris and (len(readings) != len(readings_iris)):
        logger.error("Length or readings and readings_iris does not match.")
        raise ValueError("Length or readings and readings_iris does not match.")

    # Initialise "creation" time for forecasts
    t = dt.datetime.utcnow().strftime('%Y-%m-%dT%H:00:00Z')

    # Initialise return values : triples for INSERT DATA query & input lists
    # for bulkInit function using TimeSeriesClient
    triples = ''
    dataIRIs = []
    dataClasses = []
    timeUnit = TIME_FORMAT
    # List for all created station readings IRIs
    created_reading_iris = []

    # Get concepts and create IRIs
    for i in range(len(readings)):
        r = readings[i]
        # Create IRI for reported quantity
        quantity_type = EMS + r
        if not readings_iris:
            quantity_iri = KB + r + '_' + str(uuid.uuid4())
            created_reading_iris.append(quantity_iri)
        else:
            quantity_iri = readings_iris[i]
        # Create Measure / Forecast IRI
        if is_observation:
            data_iri = KB + 'Measure_' + str(uuid.uuid4())
            data_iri_type = OM_MEASURE
            creation_time = None
        else:
            data_iri = KB + 'Forecast_' + str(uuid.uuid4())
            data_iri_type = EMS_FORECAST
            creation_time = t

        unit = UNITS_MAPPING[r]

        # Add triples to instantiate
        comment = quantity_comments[i] if quantity_comments else None
        triples += add_om_quantity(station_iri, quantity_iri, quantity_type,
                                   data_iri, data_iri_type, unit, is_observation,
                                   creation_time=creation_time, comment=comment)

        # Get data to bulkInit time series
        dataIRIs.append(data_iri)
        dataClasses.append(DATACLASS)

    return triples, created_reading_iris, dataIRIs, dataClasses, timeUnit


def retrieve_readings_data_per_station(metclient, station_id: str = None,
                                       observations: bool = True,
                                       forecasts: bool = True,
                                       only_keys: bool = True):
    """
        Retrieve station readings via Metoffer client (if station_id is provided, 
        retrieve readings for given station, otherwise for ALL stations)
    """

    # Retrieve data for particular station or ALL stations
    station_id = station_id if station_id else metoffer.ALL

    if observations:
        # Load OBSERVATIONS data
        try:
            obs = metclient.loc_observations(station_id)
        except Exception as ex:
            logger.error('Error while retrieving observation data from DataPoint API')
            raise APIException('Error while retrieving observation data from DataPoint API.') from ex
        observations = readings_dict_gen(obs)
        # Skip entries with potentially missing data
        observations = {k: v for k, v in observations.items() if v}
        available_obs = {key: condition_readings_data(observations[key], only_keys) for key in observations}
        if only_keys:
            available_obs = [(i, list(available_obs[i]['readings'].keys())) for i in available_obs.keys()]
            available_obs = dict(available_obs)


    if forecasts:
        # Load 3-hourly FORECAST data
        try:
            fc = metclient.loc_forecast(station_id, metoffer.THREE_HOURLY)
            creation_time = fc['SiteRep']['DV']['dataDate']
        except Exception as ex:
            logger.error('Error while retrieving observation data from DataPoint API')
            raise APIException('Error while retrieving observation data from DataPoint API.') from ex
        forecasts = readings_dict_gen(fc)
        # Skip entries with potentially missing data
        forecasts = {k: v for k, v in forecasts.items() if v}
        available_fcs = {key: condition_readings_data(forecasts[key], only_keys) for key in forecasts}
        if only_keys:
            available_fcs = [(i, list(available_fcs[i]['readings'].keys())) for i in available_fcs.keys()]
            available_fcs = dict(available_fcs)

    return available_obs, available_fcs, creation_time


def readings_dict_gen(returned_data):
    """
        Create dictionary of all readings from returned data

        Arguments:
            returned_data - readings data as returned by MetOffer
    """

    def _dictionary_generator(loc, data_key):
        # Create dictionary of readings for one station
        returned_reps = loc['Period']
        if type(returned_reps) != list:
            returned_reps = [returned_reps]
        for i in returned_reps:
            y, m, d = i['value'][:-1].split("-")
            date = dt.datetime(int(y), int(m), int(d))
            if 'Rep' not in i.keys():
                # Skip entries with missing readings block
                continue
            ureps = i['Rep']
            if type(ureps) != list:
                ureps = [i['Rep']]
            for rep in ureps:
                try:
                    t = (date + dt.timedelta(seconds=int(rep['$']) * 60), "")  # t always a tuple
                except(ValueError):
                    t = (date, rep['$'])  # Used for "DAILY" (time) step
                except(KeyError):
                    t = (date, '') 
                del rep['$']
                weather = {'timestamp': t}
                for n in rep:
                    try:
                        # -99 is used by the Met Office as a value where no data is held.
                        weather[data_key[n]['text']] = (
                        int(rep[n]) if rep[n] != "-99" else nan, data_key[n]['units'], n)
                    except(ValueError):
                        try:
                            weather[data_key[n]['text']] = (float(rep[n]), data_key[n]['units'], n)
                        except(ValueError):
                            weather[data_key[n]['text']] = (rep[n], data_key[n]['units'], n)
                yield weather

    # Get dict containing measurement 'name', description ('text') and unit of measurement
    data_key = {i["name"]: {"text": i["$"], "units": i["units"]} for i in returned_data["SiteRep"]["Wx"]["Param"]}
    
    # Initialise return dictionaries for all stations
    all_sites = {}
    # Get all station IDs
    returned_locs = returned_data['SiteRep']['DV']['Location']
    if type(returned_locs) != list:
            returned_locs = [returned_locs]
    for loc in returned_locs:
        id = loc['i']
        readings = []
        for reading in _dictionary_generator(loc, data_key):
            readings.append(reading)
        all_sites[id] = readings

    return all_sites


def condition_readings_data(readings_data: list, only_keys: bool = True) -> dict:
    """
        Condition retrieved MetOffer readings as required for query template

        Arguments:
            readings_data - readings data as returned by readings_dict_gen
            only_keys - boolean flag indicating whether only the keys (i.e. variable
                        names) shall be retrieved or also time series data
    """

    # Read all unique measurement variables
    read_variables = [v for data in readings_data for v in list(data.keys())]
    read_variables = list(set(read_variables))
    relevant_variables = list(set(read_variables) & set(READINGS_MAPPING.keys()))
    target_variables = [READINGS_MAPPING[i] for i in relevant_variables]
    # Initialise return dict: times and dict of conditioned readings
    conditioned = {'times': None,
                   'readings': None}
    conditioned['readings'] = dict(zip(target_variables, [None]*len(target_variables)))

    if not only_keys:
        # Read reported times and values for variables and make sure that each
        # variable has and entry for each reported time step
        df = pd.DataFrame(readings_data)
        df = df[['timestamp'] + relevant_variables]
        df['timestamp'] = df['timestamp'].apply(lambda x: dt.datetime.strftime(x[0], TIME_FORMAT))
        for read_var in relevant_variables:
            if read_var == 'Wind Direction':
                df[read_var] = df[read_var].apply(lambda x: float(COMPASS[x[0]]) if (pd.notnull(x)) else x)
            elif read_var == 'Visibility':
                # Visibility is provided as a mix of values and textual codes
                df[read_var] = df[read_var].apply(lambda x: x if not pd.notnull(x) else float(x[0]) if isinstance(x[0], (int, float)) else VISIBILITY[x[0]])
            else:
                df[read_var] = df[read_var].apply(lambda x: float(x[0]) if (pd.notnull(x)) else x)
        
        for c in df.columns:
            if c == 'timestamp':
                conditioned['times'] = df[c].to_list()
            else:
                conditioned['readings'][READINGS_MAPPING[c]] = df[c].to_list()

    return conditioned


if __name__ == '__main__':

    response = update_all_stations()
    print(f"Number of instantiated stations: {response[0]}")
    print(f"Number of instantiated readings: {response[1]}")
    print(f"Number of updated time series readings (i.e. dataIRIs): {response[2]}")
