################################################
# Authors: Markus Hofmeister (mh807@cam.ac.uk) #    
# Date: 13 Jun 2022                            #
################################################

# The purpose of this module is to provide functions to retrieve 
# readings data from the API and instantiate it in the KG

import uuid
import datetime as dt
import time
from math import nan

from agent.dataretrieval.readings import *
from agent.dataretrieval.stations import *
from agent.datainstantiation.stations import *
from agent.kgutils.querytemplates import *
from agent.kgutils.kgclient import KGClient
from agent.kgutils.timeseries import TSClient
from agent.errorhandling.exceptions import APIException
from agent.utils.stack_configs import DB_PASSWORD, DB_URL, DB_USER, QUERY_ENDPOINT, \
                                      UPDATE_ENDPOINT
from agent.utils.readings_mapping import READINGS_MAPPING, UNITS_MAPPING, \
                                              TIME_FORMAT, DATACLASS

# Initialise logger
from py4jps import agentlogging
logger = agentlogging.get_logger("prod")


def add_readings_timeseries(instantiated_ts_iris: list = None,
                            query_endpoint: str = QUERY_ENDPOINT,
                            update_endpoint: str = UPDATE_ENDPOINT):
    """
        Adds time series data to instantiated time series IRIs
        
        Arguments:
            instantiated_ts_iris - list of IRIs of instantiated time series
                                   (add readings data to all iris if no list
                                   is provided)
    """
    
    # Retrieve information about instantiated time series from KG
    # ['station', 'stationID', 'quantityType', 'dataIRI', 'comment', 'tsIRI', 'unit', 'reading']
    logger.info('Retrieving instantiated time series triples from KG ...')
    df = get_instantiated_observation_timeseries(query_endpoint=query_endpoint,
                                                 update_endpoint=update_endpoint) 
    logger.info('Time series triples successfully retrieved.')
    
    if df.empty:
        # In case no instantiated time series are found, break
        return 0
    
    # Extract only relevant information
    instantiated_obs = df[['dataIRI', 'stationID', 'comment', 'tsIRI']].copy()
    instantiated_obs.set_index(['stationID', 'comment'], inplace=True)

    if instantiated_ts_iris:
        instantiated_obs = instantiated_obs[instantiated_obs['tsIRI'].isin(instantiated_ts_iris)]

    # Load available time series data from API
    logger.info('Retrieving time series data from API ...')
    # 1) Get details about ts, i.e. ID and description (to match ts data to tsIRI)
    # ['stationID', 'ts_id', 'pollutant', 'eionet', 'unit']
    available_obs = retrieve_readings_information_from_api()
    available_obs.set_index(['stationID', 'pollutant'], inplace=True)
    instantiated_obs['ts_id'] = available_obs['ts_id']
    instantiated_obs.dropna(subset=['ts_id'], inplace=True)
    ids = list(instantiated_obs['ts_id'].unique())
    # 2) Retrieve ts data for relevant tsIDs
    # {ts_id: {times: [], values: []}, ...}
    ts_data = retrieve_timeseries_data_from_api(ts_ids=ids)
    logger.info('Time series data successfully retrieved.')

    # Map dataIRI to ts_id as dictionary key
    mapping = instantiated_obs[['ts_id', 'dataIRI']].set_index('ts_id')
    mapping = mapping[~mapping.index.duplicated(keep='first')]
    mapping = mapping.to_dict('index')
    ts_data = {mapping[k]['dataIRI']:v for k,v in ts_data.items()}

    kg_client = KGClient(query_endpoint, update_endpoint)
    # Initialise TimeSeriesClient
    ts_client = TSClient(kg_client=kg_client)
    
    # Loop through all observation timeseries - each time series only contains
    # ONE pollutant reading (as sampling frequencies not known)
    logger.info('Adding observation time series data ...')
    ts_list = []
    for dataIRI in ts_data:
        # Construct time series object to be added
        ts = TSClient.create_timeseries(ts_data[dataIRI]['times'], [dataIRI], 
                                        [ts_data[dataIRI]['values']])
        ts_list.append(ts)
    with ts_client.connect() as conn:
        ts_client.tsclient.bulkaddTimeSeriesData(ts_list, conn)

    added_obs = len(ts_list)
    logger.info(f'Time series data for {added_obs} observations successfully added to KG.')

    return added_obs


def add_all_readings_timeseries(query_endpoint: str = QUERY_ENDPOINT,
                                update_endpoint: str = UPDATE_ENDPOINT) -> int:
    """
        Adds latest time series readings for all instantiated time series
    """

    updated_ts = add_readings_timeseries(query_endpoint=query_endpoint,
                                         update_endpoint=update_endpoint)

    return updated_ts


def instantiate_station_readings(instantiated_sites_dict: dict,
                                 query_endpoint: str = QUERY_ENDPOINT,
                                 update_endpoint: str = UPDATE_ENDPOINT) -> int:
    """
        Instantiates all readings' information for the provided list of 
        measurement stations (i.e. "static" readings information, but not time
        series data itself)
        
        Arguments:
            instantiated_sites_dict - dictionary with instantiated stations/
                                      sites in the form {id : iri, ...}
    """
    
    # Initialise update query
    triples = f""

    # Initialise lists for TimeSeriesClient's bulkInit function
    dataIRIs = []
    dataClasses = []
    timeUnit = []

    # Get already instantiated observations (across all stations)
    # ['station', 'stationID', 'quantityType', 'dataIRI', 'comment', 'reading']
    logger.info('Retrieving instantiated observation triples from KG ...')
    instantiated_obs = get_instantiated_observations(query_endpoint=query_endpoint, 
                                                     update_endpoint=update_endpoint)
    logger.info('Observation triples successfully retrieved.')

    # Load available observations from API
    # ['stationID', 'ts_id', 'pollutant', 'eionet', 'unit']
    logger.info('Retrieving available observations from API ...')
    available_obs = retrieve_readings_information_from_api()
    logger.info('Available observations successfully retrieved.')

    # Initialise number of instantiated readings
    instantiated = 0

    # Loop over all sites   
    logger.info('Create triples to instantiate static observation information ...')
    for id in instantiated_sites_dict:
        
        # Get list of instantiated and available readings for current station
        inst_obs = instantiated_obs[instantiated_obs['stationID'] == id]['comment'].tolist()
        avail_obs = available_obs[available_obs['stationID'] == id]['pollutant'].tolist()
        
        # Derive quantities to instantiate
        obs = [i for i in avail_obs if i not in inst_obs]
        if obs:
            # Get station IRI and
            station_iri = instantiated_sites_dict[id]
            # Extract readings details to add
            details = available_obs[available_obs['stationID'] == id][['pollutant', 'eionet', 'unit']]
            details = details[details['pollutant'].isin(obs)]
            details = details.to_dict('index')

            # Create triples and input lists for TimeSeriesClient bulkInit
            _triples, _, _dataIRIs, _dataClasses, _timeUnit = add_readings_for_station(station_iri, readings=details)
            instantiated += len(obs)

            # Add triples to INSERT DATA query
            triples += _triples

            # Append lists to overarching TimeSeriesClient input lists
            # ALL dataIRIs are split into individual time series (as common
            # reporting frequency for individual pollutants unclear)
            dataIRIs.extend([[i] for i in _dataIRIs])
            dataClasses.extend([[i] for i in _dataClasses])
            timeUnit.extend([_timeUnit for i in _dataClasses])

            logger.info(f'Readings for station {id:>30} successfully added to query.')

    # Split triples to instantiate into several chunks of max size
    queries = split_insert_query(triples, max=100000)

    # Instantiate all non-time series triples
    kg_client = KGClient(query_endpoint, update_endpoint)
    # Perform SPARQL update query in chunks to avoid heap size/memory issues
    logger.info(f'Instantiate static observation triples in {len(queries)} chunks ...')
    for query in queries:
        kg_client.performUpdate(query)
    logger.info('Observations successfully instantiated/updated.')

    if dataIRIs:
        logger.info('Instantiate static time series triples ...')
        # Instantiate all time series triples
        ts_client = TSClient(kg_client=kg_client, rdb_url=DB_URL, rdb_user=DB_USER, 
                             rdb_password=DB_PASSWORD)
        with ts_client.connect() as conn:
            ts_client.tsclient.bulkInitTimeSeries(dataIRIs, dataClasses, timeUnit, conn)


        logger.info('Time series triples successfully added.')

    return instantiated


def instantiate_all_station_readings(query_endpoint: str = QUERY_ENDPOINT,
                                     update_endpoint: str = UPDATE_ENDPOINT,
                                     circle_center: str = None,
                                     circle_radius: str = None) -> int:
    """
        Instantiates all readings for all instantiated stations
    """

    stations = get_all_airquality_stations(query_endpoint=query_endpoint,
                                           update_endpoint=update_endpoint, 
                                           circle_center=circle_center,
                                           circle_radius=circle_radius)
       
    instantiated = instantiate_station_readings(instantiated_sites_dict=stations,
                                                query_endpoint=query_endpoint,
                                                update_endpoint=update_endpoint)

    return instantiated


def update_all_stations(query_endpoint: str = QUERY_ENDPOINT,
                        update_endpoint: str = UPDATE_ENDPOINT):

    # Instantiate all available stations (ONLY not already existing stations
    # will be newly instantiated)
    print('Updating instantiated stations ... ')
    t1 = time.time()
    new_stations = instantiate_all_stations(query_endpoint, update_endpoint)
    t2 = time.time()
    diff = t2-t1
    print(f'Finished after: {diff//60:5>n} min, {diff%60:4.2f} s \n')


    # Instantiate all available station readings (ONLY not already existing
    # readings will be newly instantiated)
    print('Updating instantiated station readings ... ')
    t1 = time.time()
    new_readings = instantiate_all_station_readings(query_endpoint, update_endpoint)
    t2 = time.time()
    diff = t2-t1
    print(f'Finished after: {diff//60:5>n} min, {diff%60:4.2f} s \n')

    # Add latest readings time series to instantiated reading quantities
    print('Updating station readings time series data ... ')
    t1 = time.time()
    updated_ts = add_all_readings_timeseries(query_endpoint, update_endpoint)
    t2 = time.time()
    diff = t2-t1
    print(f'Finished after: {diff//60:5>n} min, {diff%60:4.2f} s \n')

    return new_stations, new_readings, updated_ts


def add_readings_for_station(station_iri: str, readings: dict):
    """
        Return SPARQL update query string to instantiate readings for given 
        station IRI (query string to be included in overarching INSERT DATA query)

        Arguments:
            station_iri - Station IRI without trailing '<' and '>'
            readings - dict of station readings to instantiate
        
        Returns
            triples - triples to be added to INSERT DATA query
            created_reading_iris - list of newly created quantity IRIs
            dataIRIs, dataClasses, timeUnit - to be appended to input arguments
                                              to TimSeriesClient bulkInit call
    """

    # Initialise return values: triples for INSERT DATA query & input lists
    # for bulkInit function using TimeSeriesClient
    triples = ''
    dataIRIs = []
    dataClasses = []
    timeUnit = TIME_FORMAT
    # List for all created station readings IRIs
    created_reading_iris = []

    # Get concepts and create IRIs
    for i in readings:
        # Retrieve name and unit of measured pollutant
        p = readings[i].get('pollutant')
        p = None if not p else str(p).strip().lower()
        u = readings[i].get('unit')
        u = None if not u else str(u).strip()
        eionet = readings[i].get('eionet')
        eionet = None if not eionet else str(eionet).strip()

        # Create IRI for reported quantity
        pollutant = READINGS_MAPPING.get(p)
        if pollutant:
            quantity_type = pollutant[1]
            quantity_iri = KB + pollutant[0] + '_' + str(uuid.uuid4())
        else:
            # If pollutant name suggests particular matter
            if 'in pm10' in p or 'in pm2.5' in p:
                quantity_type = EMS_PM_CONCENTRATION
                quantity_iri = KB + PM_CONCENTRATION + '_' + str(uuid.uuid4())
            else:
                # All other ordinary air pollutants
                quantity_type = EMS_AIR_POLLUTANT_CONCENTRATION
                quantity_iri = KB + AIR_POLLUTANT_CONCENTRATION + '_' + str(uuid.uuid4())
        created_reading_iris.append(quantity_iri)

        # Create Measure IRI and assign unit
        data_iri = KB + 'Measure_' + str(uuid.uuid4())
        data_iri_type = OM_MEASURE
        unit = UNITS_MAPPING.get(u)
        if unit:
            om_unit = unit[0]
            om_symbol = unit[1]
        else: 
            om_unit, om_symbol = None, None

        # Add triples to instantiate
        triples += add_om_quantity(station_iri, quantity_iri, quantity_type,
                                   data_iri, data_iri_type, om_unit, om_symbol,
                                   comment=p, sameas=eionet)

        # Get data to bulkInit time series
        dataIRIs.append(data_iri)
        dataClasses.append(DATACLASS)

    return triples, created_reading_iris, dataIRIs, dataClasses, timeUnit


def retrieve_station_details_from_api(crs: str = 'EPSG:4326'):
    """
        Retrieve detailed station information incl. reported time series
        per station (i.e. time series IDs)

        Arguments:
            crs - coordinate reference system in which to return station 
                  locations (as EPSG code, e.g. 'EPSG:4326'). EPSG:4326 coordinates
                  are provided as [lat, long] and specifying any other CRS can 
                  potentially result in switched values for lat and lon

        Returns DataFrame with columns: ['station', 'latitude', 'longitude', 
                                         'elevation', 'ts_id', 'stationID']
            station: non-unique station name / label
            latitude/longitude/elevation: coordinates of station
            ts_id: unique ID of reported reading / time series
            stationID: created unique UK Air station ID for that station
    """

    # Construct API call to get extended information for all stations (i.e. basic
    # station data incl. information about reported readings/timeseries)
    if crs and re.match(r"EPSG:\d+", crs):
        url = f'https://uk-air.defra.gov.uk/sos-ukair/api/v1/stations?{crs}&expanded=true'
        if crs != 'EPSG:4326':
            logger.info('Provided CRS is different from "EPSG:4326". Extraction of ' \
                      + 'latitude and longitude can be erroneous.')
    else:
        raise InvalidInput ("Provided CRS does not match expected 'EPSG:' format.")

    try:
        logger.info('Retrieving station data from API ...')
        stations_raw = requests.get(url=url).json()
        logger.info('Station data successfully retrieved.')
    except Exception as ex:
        logger.error(f"Error while retrieving station data from API: {ex}")
        raise APIException("Error while retrieving station details from API.") from ex

    # Create DataFrame from json response and condition data to ensure 
    # consistency with instantiated station information
    stations = [{'station': None if not s.get('properties') else
                            s.get('properties').get('label').split('-')[0],
                 'latitude': None if not s.get('geometry') else 
                             s.get('geometry').get('coordinates')[0],
                 'longitude': None if not s.get('geometry') else 
                              s.get('geometry').get('coordinates')[1],
                 'elevation': None if not s.get('geometry') else 
                              s.get('geometry').get('coordinates')[2],
                 'ts_id': None if not s.get('properties') else 
                          list(s.get('properties').get('timeseries').keys())
                } for s in stations_raw ]
    df = pd.DataFrame(stations)
    # Create separate rows for each reported time series
    df = df.explode('ts_id', ignore_index=True)
    # Clean and condition returned API data (incl. creation of unique station ID)
    df = clean_api_data(df)

    return df


def retrieve_readings_information_from_api(crs: str = 'EPSG:4326'):
    """
        Retrieve and condition station readings data from UK Air API 
        (but not the time series data itself)
        Readings' data collection is done in 2 steps:
            1) Retrieve detailed station information incl. reported time series
               per station (i.e. time series IDs) - detailed station info required
               to construct unique station ID (name + lat#lon)
            2) Retrieve information about time series IDs

        Arguments:
            crs - coordinate reference system in which to return station 
                  locations (as EPSG code, e.g. 'EPSG:4326')

        Returns DataFrame with columns: ['stationID', 'ts_id', 'pollutant', 
                                         'eionet', 'unit']
            stationID: created unique UK Air station ID for that station
            ts_id: unique ID of reported reading / time series
            pollutant: Label / textual description of measured pollutant
            eionet: Link to corresponding eionet entry for pollutant
            unit: unit of measured pollutant
    """

    # Retrieve and clean detailed station data data (incl. creation of unique station ID)
    df = retrieve_station_details_from_api(crs=crs)

    # Query readings metadata from API
    ts_ids = list(df['ts_id'].unique())

    # Number of days back to check whether the time series has current data
    # --> only time series with recent data will be considered, as API contains
    # lots of outdated or inactive timeseries (e.g. last reading 10 years back)
    days = 14
    readings = retrieve_timeseries_information_from_api(ts_ids=ts_ids, days_back=days)

    # Map time series data into DataFrame and remove all rows without ts info
    df['ts_info'] = df['ts_id'].map(readings)
    df = df.dropna(subset=['ts_info'])
    df[['pollutant', 'eionet', 'unit']] = df['ts_info'].apply(pd.Series)
    # Convert all pollutant names / labels to lower case
    df.loc[:, 'pollutant'] = df['pollutant'].str.lower()

    # Return only relevant information 
    ts_info = df[['stationID', 'ts_id', 'pollutant', 'eionet', 'unit']]
    # Remove duplicates (i.e. multiple time series available for same pollutant
    # at very same location --> impossible to assess which reading is "correct";
    # hence delete duplicates and instantiate only one of the readings)
    ts_info.sort_values(by=['stationID'])
    ts_info = ts_info[~ts_info.duplicated(subset=['stationID', 'pollutant'])]

    return ts_info


def retrieve_timeseries_information_from_api(ts_ids=[], days_back=7) -> dict:
    """
        Retrieve information about the nature of particular timeseries/
        station reading(s) (but not the time series data itself)
        (returns information for all available timeseries if empty list is provided)

        Arguments:
            ts_ids - (list of) ID(s) of reading/timeseries
            days_back - Number of days back (as of now) to check whether time
                        series contains recent data (old/inactive time series
                        will be neglected)
        Returns:
            Dictionary with readings information (timeseries ID as key and
            information as dictionary)
    """

    # API call to get information for provided timeseries ID(s)
    if isinstance(ts_ids, str):
        url = f'https://uk-air.defra.gov.uk/sos-ukair/api/v1/timeseries/{ts_ids}'
        ts_ids = [ts_ids]
    elif isinstance(ts_ids, list):
        url = f'https://uk-air.defra.gov.uk/sos-ukair/api/v1/timeseries?expanded=true'
    else:
        raise InvalidInput('Provided timeseries ID(s) must be a (list of) string(s).')
    
    # Retrieve readings information
    try:
        logger.info('Retrieving time series details from API ...')
        ts_raw = requests.get(url=url).json()
        logger.info('Time series details data successfully retrieved.')        
    except Exception as ex:
        logger.error(f"Error while retrieving timeseries details from API: {ex}")
        raise APIException("Error while retrieving timeseries details from API.") from ex
    # Ensure returned data is list of dictionaries
    if isinstance(ts_raw, dict):
        ts_raw = [ts_raw]

    # Remove meaningless stations / stations with missing data
    stations_to_remove = ['http://environment.data.gov.uk/air', 'GB_SamplingFeature_missingFOI']
    ts_adj = [ts for ts in ts_raw if ts.get('station', {}).get('properties', {}).get('label', {}) 
              not in stations_to_remove]

    # Only keep time series with recent readings
    t1 = dt.datetime.now()
    t2 = t1 - dt.timedelta(days=days_back)
    unix_cutoff = int(t2.timestamp())
    ts_adj = [ts for ts in ts_adj if isinstance(ts.get('lastValue', {}).get('timestamp', {}), (int, float))]
    # API timestamp is provided in ms
    ts_adj = [ts for ts in ts_adj if ts.get('lastValue', {}).get('timestamp', {})/1000 > unix_cutoff]

    # Return information about all timeseries if empty ts_id list is provided
    if not ts_ids:
        ts_ids = [ts['id'] for ts in ts_adj]   

    # Initialise return dictionary
    infos = {}
    # Extract relevant information from JSON response
    for ts in ts_adj:
        info = {}
        # Get information if exists (else returns None)
        id = ts.get('id')
        if id in ts_ids:
            pollutant = ts.get('parameters', {}).get('offering', {}).get('label')
            eionet = ts.get('parameters', {}).get('phenomenon', {}).get('label')
            unit = ts.get('uom')
            # Populate dictionary
            info['pollutant'] = pollutant.split('-')[-1] if pollutant else 'n/a'
            info['eionet'] = eionet if eionet else 'n/a'
            info['unit'] = unit if unit else 'n/a'
            infos[id] = info

    return infos


def retrieve_timeseries_data_from_api(crs: str = 'EPSG:4326', ts_ids=[], 
                                      period='P1D', chunksize=25) -> dict:
    """
        Retrieve time series data for instantiated station readings from UK Air API 
        Time series data collection is done in 2 steps:
            1) Retrieve detailed station information incl. reported time series
               per station (i.e. time series IDs)
            2) Retrieve time series data for IDs

        Arguments:
            crs - coordinate reference system in which to return station 
                  locations (as EPSG code, e.g. 'EPSG:4326')
            ts_ids - (list of) ID(s) of reading/timeseries
                     (returns information for all available timeseries if empty
                     list is provided)
            period - duration for which to retrieve the time series data, in
                     ISO8601 formatted period notation (default: 1 day)
            chunksize - number of time series to request at once (to avoid
                        API time out issues)

        Returns:
            Dictionary with readings information (timeseries ID as key and
            information as dictionary)
    """

    if not ts_ids:
        # Retrieve and clean detailed station data (incl. time seriesID)
        df = retrieve_station_details_from_api(crs=crs)
        ts_ids = list(df['ts_id'].unique())
    
    # Construct POST request to query readings time series data from API
    url = 'https://uk-air.defra.gov.uk/sos-ukair/api/v1/timeseries/getData'
    headers = {'Content-Type': 'application/json'}
    now = dt.datetime.utcnow().strftime(TIME_FORMAT)
    timespan = f'{period}/{now}'

    all_ts = {}

    # Loop to avoid API timeout
    chunks = [ts_ids[i:i+chunksize] for i in range(0,len(ts_ids),chunksize)]
    #TODO: remove testing limit
    chunks = chunks[:2]
    i, j = 1, len(chunks)
    for chunk in chunks: 
        print(f'Retrieving chunk {i:>4}/{j:>4} of time series data from API ...')
        i += 1

        # Request time series data
        body = {"timespan": timespan,
                "timeseries": chunk
        }

        try:
            r = requests.post(url=url, data=json.dumps(body), headers=headers)
            # Extract time series data
            ts_data = r.json()
        except Exception as ex:
            logger.error(f"Error while retrieving time series data from API: {ex}")
            raise APIException("Error while retrieving time series data from API.") from ex

        df = pd.DataFrame.from_dict(ts_data, orient='index')
        # Remove rows without entries
        df = df[df['values'].astype(bool)]
        if df.empty:
            continue
        else:                
            # Unpack time series lists of dicts [{timestamp: value}, ...]
            df = df.explode('values')
            # Unpack dicts to separate columns
            df[['timestamp', 'value']] = df['values'].apply(pd.Series)
            df = df.drop(columns=['values'])
            df['timestamp'] = df['timestamp'].apply(lambda x: 
                        dt.datetime.utcfromtimestamp(x/1000).strftime(TIME_FORMAT))
            # Add time series data to overall dict
            for ts_id in df.index.unique():
                # Remove entries with missing values, i.e. with code "-99"
                non_missing = df.loc[ts_id][df.loc[ts_id, 'value'] != -99]
                if not non_missing.empty:
                    times = non_missing.loc[ts_id]['timestamp']
                    # Handle cases where only single entries might be returned
                    times = [times] if isinstance(times, str) else times.values.tolist()
                    values = non_missing.loc[ts_id]['value']
                    values = [values] if isinstance(values, float) else values.values.tolist()
                    all_ts[ts_id] = {'times': times,
                                     'values': values }

    return all_ts


if __name__ == '__main__':

    response = update_all_stations()
    print(f"Number of instantiated stations: {response[0]}")
    print(f"Number of instantiated readings: {response[1]}")
    print(f"Number of updated time series readings (i.e. dataIRIs): {response[2]}")
