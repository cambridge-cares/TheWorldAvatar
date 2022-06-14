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

#import agentlogging
from airquality.dataretrieval.readings import *
from airquality.dataretrieval.stations import *
from airquality.datainstantiation.stations import *
from airquality.kgutils.querytemplates import *
from airquality.kgutils.kgclient import KGClient
from airquality.kgutils.timeseries import TSClient
from airquality.errorhandling.exceptions import APIException
from airquality.utils.properties import QUERY_ENDPOINT, UPDATE_ENDPOINT
from airquality.utils.readings_mapping import READINGS_MAPPING, UNITS_MAPPING, \
                                              TIME_FORMAT, DATACLASS

# Initialise logger
#logger = agentlogging.get_logger("prod")


def add_readings_timeseries(instantiated_ts_iris: list = None,
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
    
    # Load available observations from API
    print('Retrieving time series data from API ...')
    #logger.info('Retrieving time series data from API ...')
    available_obs, = retrieve_readings_data_per_station(metclient, only_keys=False)
    print('Time series data successfully retrieved.')
    #logger.info('Time series data successfully retrieved.')
    
    # Retrieve information about instantiated time series from KG
    print('Retrieving time series triples from KG ...')
    #logger.info('Retrieving time series triples from KG ...')
    instantiated_obs = get_instantiated_observation_timeseries(query_endpoint=query_endpoint,
                                                               update_endpoint=update_endpoint)
    instantiated_fcs = get_instantiated_forecast_timeseries(query_endpoint=query_endpoint,
                                                            update_endpoint=update_endpoint)
    print('Time series triples successfully retrieved.')
    #logger.info('Time series triples successfully retrieved.')

    # Keep only the relevant subset for instantiated_ts_iris
    if instantiated_ts_iris:
        instantiated_obs = instantiated_obs[instantiated_obs['tsIRI'].isin(instantiated_ts_iris)]
        instantiated_fcs = instantiated_fcs[instantiated_fcs['tsIRI'].isin(instantiated_ts_iris)]
    # Get short version of variable type from full quantity type
    instantiated_obs['reading'] = instantiated_obs['quantityType'].apply(lambda x: x.split('#')[-1])
    instantiated_fcs['reading'] = instantiated_fcs['quantityType'].apply(lambda x: x.split('#')[-1])   

    # Initialise update query for creation time
    query_string = update_forecast_creation_datetime(issue_time)

    # Initialise TimeSeriesClient
    ts_client = TSClient.tsclient_with_default_settings()

    added_obs = 0
    added_fcs = 0
    
    # Loop through all observation timeseries
    print('Adding observation time series data ...')
    #logger.info('Adding observation time series data ...')
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
    ts_client.bulkaddTimeSeriesData(ts_list)
    print(f'Time series data for {added_obs} observations successfully added to KG.')
    #logger.info(f'Time series data for {added_obs} observations successfully added to KG.')
    
    # Strip trailing comma and close & perform creation date update query
    query_string = query_string[:-2]
    query_string += f") ) }}"
    kg_client = KGClient(query_endpoint, update_endpoint)
    kg_client.performUpdate(query_string)
    #logger.info('Creation time triples successfully updated.')

    return added_obs + added_fcs


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
    print('Retrieving instantiated observation triples from KG ...')
    #logger.info('Retrieving instantiated observation/forecast triples from KG ...')
    instantiated_obs = get_instantiated_observations(query_endpoint=query_endpoint, 
                                                     update_endpoint=update_endpoint)
    print('Observation triples successfully retrieved.')
    #logger.info('Observation/forecast triples successfully retrieved.')                                                    

    # Load available observations from API
    # ['stationID', 'ts_id', 'pollutant', 'eionet', 'unit']
    print('Retrieving available observations from API ...')
    #logger.info('Retrieving available observations/forecasts from API ...')
    available_obs = retrieve_readings_information_from_api()
    print('Available observations successfully retrieved.')
    #logger.info('Available observations/forecasts successfully retrieved.')

    # Initialise number of instantiated readings
    instantiated = 0

    # Loop over all sites   
    print('Create triples to instantiate static observation information ...')
    #logger.info('Create triples to instantiate static observation/forecast information ...')
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
            # TODO: Potentially split dataIRIs is individual time series (as 
            #       common reporting frequency for individual pollutants unclear)
            dataIRIs.append(_dataIRIs)
            dataClasses.append(_dataClasses)
            timeUnit.append(_timeUnit)

            #logger.info(f'Readings for station {id:>30} successfully added to query.')

    # Split triples to instantiate into several chunks of max size
    queries = split_insert_query(triples, max=100000)

    # Instantiate all non-time series triples
    kg_client = KGClient(query_endpoint, update_endpoint)
    # Perform SPARQL update query in chunks to avoid heap size/memory issues
    print(f'Instantiate static observation triples in {len(queries)} chunks ...')
    #logger.info(f'Instantiate static observation/forecast triples in {len(queries)} chunks ...')
    for query in queries:
        kg_client.performUpdate(query)
    print('Observations successfully instantiated/updated.')
    #logger.info('Insert query successfully performed.')

    if dataIRIs:
        print('Instantiate static time series triples ...')
        #logger.info('Instantiate static time series triples ...')
        # Instantiate all time series triples
        ts_client = TSClient.tsclient_with_default_settings()
        ts_client.bulkInitTimeSeries(dataIRIs, dataClasses, timeUnit)
        print('Time series triples successfully added.')
        #logger.info('Time series triples successfully added.')

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
    print('\nUpdate instantiated stations: ')
    #logger.info('Update instantiated stations ...')
    t1 = time.time()
    new_stations = instantiate_all_stations(query_endpoint, update_endpoint)
    t2 = time.time()
    diff = t2-t1
    print(f'Finished after: {diff//60:5>n} min, {diff%60:4.2f} s \n')


    # Instantiate all available station readings (ONLY not already existing
    # readings will be newly instantiated)
    print('\nUpdate instantiated station readings: ')
    #logger.info('Update instantiated station readings ...')
    t1 = time.time()
    new_readings = instantiate_all_station_readings(query_endpoint, update_endpoint)
    t2 = time.time()
    diff = t2-t1
    print(f'Finished after: {diff//60:5>n} min, {diff%60:4.2f} s \n')

    # Add latest readings time series to instantiated reading quantities
    print('\nUpdate station readings time series data: ')
    #logger.info('Update station readings time series data ...')
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
            print('Provided CRS is different from "EPSG:4326". Extraction of ' \
                + 'latitude and longitude can be erroneous.')
            #logger.info('Provided CRS is different from "EPSG:4326". Extraction of ' \
            #          + 'latitude and longitude can be erroneous.')
    else:
        raise InvalidInput ("Provided CRS does not match expected 'EPSG:' format.")

    try:
        print('Retrieving station details from API ...')
        #logger.info('Retrieving station data from API ...')
        stations_raw = requests.get(url=url).json()
        print('Station details successfully retrieved.')
        #logger.info('Station data successfully retrieved.')
    except Exception as ex:
        #logger.error("Error while retrieving station data from API.")
        raise APIException("Error while retrieving station details from API.")  

    # Create DataFrame from json response and condition data to ensure 
    # consistency with instantiated station information
    stations = [{'station': s['properties']['label'].split('-')[0],
                 'latitude': s['geometry']['coordinates'][0],
                 'longitude': s['geometry']['coordinates'][1], 
                 'elevation': s['geometry']['coordinates'][2],
                 'ts_id': list(s['properties']['timeseries'].keys())
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
        print('Retrieving time series details from API ...')
        #logger.info('Retrieving time series details from API ...')
        ts_raw = requests.get(url=url).json()
        print('Time series details data successfully retrieved.')
        #logger.info('Time series details data successfully retrieved.')        
    except Exception as ex:
        #logger.error("Error while retrieving timeseries details from API.")
        raise APIException("Error while retrieving timeseries details from API.")
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
                                      period='P1D', chunksize=100) -> dict:
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

    ts_ids = ts_ids[:500]
    
    # Construct POST request to query readings time series data from API
    url = 'https://uk-air.defra.gov.uk/sos-ukair/api/v1/timeseries/getData'
    headers = {'Content-Type': 'application/json'}
    now = dt.datetime.utcnow().strftime(TIME_FORMAT)
    timespan = f'{period}/{now}'

    all_ts = {}

    # Loop to avoid API timeout
    chunks = [ts_ids[i:i+chunksize] for i in range(0,len(ts_ids),chunksize)]
    i, j = 1, len(chunks)
    for chunk in chunks:   
        body = {"timespan": timespan,
                "timeseries": chunk
        }
        print(f'Retrieving chunk {i:>4}/{j:>4} of time series data from API ...')
        r = requests.post(url=url, data=json.dumps(body), headers=headers)

        # Extract time series data
        ts_data = r.json()
        df = pd.DataFrame.from_dict(ts_data, orient='index')
        # Remove rows without entries
        df = df[df['values'].astype(bool)]
        # Unpack time series lists
        df = df.explode('values')
        # Unpack dicts to separate columns
        df[['timestamp', 'value']] = df['values'].apply(pd.Series)
        df = df.drop(columns=['values'])
        df['timestamp'] = df['timestamp'].apply(lambda x: 
                    dt.datetime.utcfromtimestamp(x/1000).strftime(TIME_FORMAT))
        # Add time series data to overall dict
        for ts_id in df.index.unique():
            all_ts[ts_id] = {'times': df.loc[ts_id,:]['timestamp'].values.tolist(),
                             'values': df.loc[ts_id,:]['value'].values.tolist() }

        i += 1

    print(f'Done: {len(all_ts.keys)}')


if __name__ == '__main__':

    retrieve_timeseries_data_from_api()

    response = update_all_stations()
    print(f"Number of instantiated stations: {response[0]}")
    print(f"Number of instantiated readings: {response[1]}")
    print(f"Number of updated time series readings (i.e. dataIRIs): {response[2]}")
