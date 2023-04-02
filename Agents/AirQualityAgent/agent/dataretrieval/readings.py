################################################
# Authors: Markus Hofmeister (mh807@cam.ac.uk) #    
# Date: 05 Apr 2022                            #
################################################

# The purpose of this module is to provide functions to retrieve 
# readings data from KG

import re
import datetime as dt
import pandas as pd
from agent.kgutils import kgclient

from agent.kgutils.kgclient import KGClient
from agent.kgutils.timeseries import TSClient
from agent.kgutils.querytemplates import *
from agent.errorhandling.exceptions import InvalidInput, TSException
from agent.utils.stack_configs import DB_PASSWORD, DB_URL, DB_USER, QUERY_ENDPOINT, UPDATE_ENDPOINT
from agent.utils.readings_mapping import TIME_FORMAT

# Initialise logger
from py4jps import agentlogging
logger = agentlogging.get_logger("prod")


def get_instantiated_observations(stations: list = None,
                                  query_endpoint: str = QUERY_ENDPOINT,
                                  update_endpoint: str = UPDATE_ENDPOINT):
    """
        Returns DataFrame of (all) instantiated observations in KG
        (data for all stations is returned if no stations list is provided)

        Arguments:
            stations - list of ReportingStation IRIs (WITHOUT trailing '<' and '>'
                       for which to retrieve data)

        Returns DataFrame with columns: ['station', 'stationID', 'quantityType', 
                                         'dataIRI', 'comment', 'reading']
            station: station IRI
            stationID: created unique UK Air station ID for that station
            quantityType: IRI of OntoEMS quantity, e.g. https://www.theworldavatar.com/kg/ontoems/OzoneConcentration
            dataIRI: IRI of quantity instance to which time series is attached
            comment: label of measured pollutant
            reading: shorthand of OntoEMS quantity, e.g. OzoneConcentration
    """

    # Construct KG client and execute query
    query_string = instantiated_observations(station_iris=stations)
    kg_client = KGClient(query_endpoint, update_endpoint)
    results = kg_client.performQuery(query=query_string)
    # Parse results into DataFrame
    df = pd.DataFrame(columns=['station', 'stationID', 'quantityType', 'dataIRI', 'comment'], 
                       data=results)
    # Add column with shorthand of quantity type
    df['reading'] = df['quantityType'].apply(lambda x: x.split('/')[-1])
    
    return df


def get_instantiated_observation_timeseries(stations: list = None,
                                            query_endpoint: str = QUERY_ENDPOINT,
                                            update_endpoint: str = UPDATE_ENDPOINT):
    """
        Returns DataFrame of (all) instantiated observation timeseries in KG
        (data for all stations is returned if no stations list is provided)

        Arguments:
            stations - list of ReportingStation IRIs (WITHOUT trailing '<' and '>'
                       for which to retrieve data)

        Returns DataFrame with columns: ['station', 'stationID', 'quantityType', 
                                         'dataIRI', 'comment', 'tsIRI', 'unit', 'reading']
            station: station IRI
            stationID: created unique UK Air station ID for that station
            quantityType: IRI of OntoEMS quantity, e.g. https://www.theworldavatar.com/kg/ontoems/OzoneConcentration
            dataIRI: IRI of quantity instance to which time series is attached
            comment: label of measured pollutant
            tsIRI: IRI of time series instance
            unit - unit for time series, e.g. mg/m3
            reading: shorthand of OntoEMS quantity, e.g. OzoneConcentration
    """

    # Construct KG client and execute query
    query_string = instantiated_observation_timeseries(stations)
    kg_client = KGClient(query_endpoint, update_endpoint)
    results = kg_client.performQuery(query=query_string)
    # Parse results into DataFrame
    df = pd.DataFrame(columns=['station', 'stationID', 'quantityType', 'dataIRI', 
                               'comment', 'tsIRI', 'unit'], data=results)
    # Add column with shorthand of quantity type
    df['reading'] = df['quantityType'].apply(lambda x: x.split('/')[-1])
    
    return df


def get_time_series_data(station_iris: list = None,
                         observation_types: list = None,
                         tmin: str = None, tmax: str = None,
                         query_endpoint: str = QUERY_ENDPOINT,
                         update_endpoint: str = UPDATE_ENDPOINT):
    """
        Retrieve time series data for provided observation types and stations from KG

        Arguments
            station_iris - list of station IRIs for which to retrieve time series data
                           (all stations if None)
            observation_types - list of observation types (e.g., PM10Concentration)
                                for which to retrieve data (all if None)
            tmin - oldest time step for which to retrieve data
            tmax - latest time step for which to retrieve data

        Returns
            List of (Java) time series objects
            List of dictionaries with ts names (i.e. [{dataIRI: name}, ...])
            List of dictionaries with ts units (i.e. [{dataIRI: unit}, ...])
    """

    def _validate_time_format(time_string):
        rec = re.compile(r'\d{4}-\d{1,2}-\d{1,2}T\d{1,2}:\d{1,2}:\d{1,2}Z')
        if bool(rec.match(time_string)):
            return time_string
        else:
            t = None
            # Adding potentially missing Z at end of time string
            rec = re.compile(r'Z$')
            if not bool(rec.match(time_string)):
                time_string += 'Z'
                #logger.info('Provided time string assumed in UTC.')
            rec = re.compile(r'\d{4}-\d{1,2}-\d{1,2}T\d{1,2}:\d{1,2}Z')
            if bool(rec.match(time_string)):
                t = dt.datetime.strptime(time_string, '%Y-%m-%dT%H:%MZ')
            else: 
                rec = re.compile(r'\d{4}-\d{1,2}-\d{1,2}T\d{1,2}Z')
                if bool(rec.match(time_string)):
                    t = dt.datetime.strptime(time_string, '%Y-%m-%dT%HZ')
                else:
                    rec = re.compile(r'\d{4}-\d{1,2}-\d{1,2}Z')
                    if bool(rec.match(time_string)):
                        t = dt.datetime.strptime(time_string, '%Y-%m-%dZ')
        # Return properly formatted time string if format could be derived
        return dt.datetime.strftime(t, TIME_FORMAT)
    
    # Validate format of provided tmin and tmax
    if tmin:
        try:
            tmin = _validate_time_format(tmin)
        except:
            #logger.info(f'Provided format of tmin could not be derived. Expected format: {TIME_FORMAT}')
            raise InvalidInput(f'Provided format of tmin could not be derived. Expected format: {TIME_FORMAT}')
    if tmax:
        try:
            tmax = _validate_time_format(tmax)
        except:
            #logger.info(f'Provided format of tmax could not be derived. Expected format: {TIME_FORMAT}')
            raise InvalidInput(f'Provided format of tmax could not be derived. Expected format: {TIME_FORMAT}')

    # Create DataFrame from instantiated observation time series
    # ['station', 'stationID', 'quantityType', 'dataIRI', 'comment', 'tsIRI', 'unit', 'reading']
    df = get_instantiated_observation_timeseries(station_iris, query_endpoint, update_endpoint)
    # Get relevant subset of available time series data
    if observation_types:
        observation_types = [str(i).lower() for i in observation_types]
        df = df[df['reading'].str.lower().isin(observation_types)]

    # Get list of lists of dataIRIs to retrieve
    dataIRIs_list = [list(df.loc[df['tsIRI'] == tsIRI, 'dataIRI']) for tsIRI in df['tsIRI'].unique()]
    
    # Initialise return list
    ts_data = []
    ts_names = []
    ts_units = []
    
    # Initialise TimeSeriesClient
    # ts_client = TSClient.tsclient_with_default_settings()
    ts_client = TSClient(kg_client=kgclient, rdb_url=DB_URL, rdb_user=DB_USER, 
                         rdb_password=DB_PASSWORD)

    for dataIRIs in dataIRIs_list:

        # Get time series within desired bounds
        try:
            ts_data.append(ts_client.getTimeSeriesWithinBounds(dataIRIs, tmin, tmax))
        except:
            #logger.error(f'Error while retrieving time series data for dataIRIs: {dataIRIs}')
            raise TSException(f'Error while retrieving time series data for dataIRIs: {dataIRIs}')
        
        # Get time series names and units (as dict with dataIRIs as key)
        df_sub = df.loc[df['dataIRI'].isin(dataIRIs), ['dataIRI','unit', 'comment']]
        ts_names.append(dict(zip(df_sub['dataIRI'], df_sub['comment'].str.capitalize())))
        ts_units.append(dict(zip(df_sub['dataIRI'], df_sub['unit'])))

    return ts_data, ts_names, ts_units
