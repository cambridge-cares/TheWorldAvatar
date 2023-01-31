###############################################
# Authors: Markus Hofmeister (mh807cam.ac.uk) #    
# Date: 05 Apr 2022                           #
###############################################

# The purpose of this module is to provide functions to retrieve 
# readings data from KG

import re
import datetime as dt
import pandas as pd

from py4jps import agentlogging
from agent.errorhandling.exceptions import InvalidInput, TSException
from agent.kgutils.kgclient import KGClient
from agent.kgutils.querytemplates import *
from agent.kgutils.tsclient import TSClient
from agent.utils.readings_mapping import TIME_FORMAT
from agent.utils.stack_configs import (DB_PASSWORD, DB_URL, DB_USER,
                                       QUERY_ENDPOINT, UPDATE_ENDPOINT)

# Initialise logger
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

        Returns DataFrame with columns: ['station', 'stationID', 'quantityType', 'dataIRI', 'reading']
            station: station IRI
            stationID: unique Met Office ID for that station
            quantityType: IRI of OntoEMS quantity, e.g. https://www.theworldavatar.com/kg/ontoems/AirTemperature
            dataIRI: IRI of quantity instance to which time series is attached
            reading: shorthand of OntoEMS quantity, e.g. AirTemperature
    """

    # Construct KG client and execute query
    query_string = instantiated_observations(station_iris=stations)
    kg_client = KGClient(query_endpoint, update_endpoint)
    results = kg_client.performQuery(query=query_string)
    # Parse results into DataFrame
    df = pd.DataFrame(columns=['station', 'stationID', 'quantityType', 'dataIRI'])
    df = df.append(results)
    # Add column with shorthand of quantity type
    df['reading'] = df['quantityType'].apply(lambda x: x.split('/')[-1])
    
    return df


def get_instantiated_forecasts(stations: list = None,
                               query_endpoint: str = QUERY_ENDPOINT,
                               update_endpoint: str = UPDATE_ENDPOINT):
    """
        Returns DataFrame of (all) instantiated forecasts in KG
        (data for all stations is returned if no stations list is provided)

        Arguments:
            stations - list of ReportingStation IRIs (WITHOUT trailing '<' and '>'
                       for which to retrieve data)

        Returns DataFrame with columns: ['station', 'stationID', 'quantityType', 'dataIRI', 'reading']
            station: station IRI
            stationID: unique Met Office ID for that station
            quantityType: IRI of OntoEMS quantity, e.g. https://www.theworldavatar.com/kg/ontoems/AirTemperature
            dataIRI: IRI of quantity instance to which time series is attached
            reading: shorthand of OntoEMS quantity, e.g. AirTemperature
    """

    # Construct KG client and execute query
    query_string = instantiated_forecasts(station_iris=stations)
    kg_client = KGClient(query_endpoint, update_endpoint)
    results = kg_client.performQuery(query=query_string)
    # Parse results into DataFrame
    df = pd.DataFrame(columns=['station', 'stationID', 'quantityType', 'dataIRI'])
    df = df.append(results)
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

        Returns DataFrame with columns: ['station', 'stationID', 'quantityType', 'dataIRI', 'unit', 'tsIRI', 'reading']
            station: station IRI
            stationID: unique Met Office ID for that station
            quantityType: IRI of OntoEMS quantity, e.g. https://www.theworldavatar.com/kg/ontoems/AirTemperature
            dataIRI: IRI of quantity instance to which time series is attached
            unit - unit for time series, e.g. hPa
            tsIRI: IRI of time series instance
            reading: shorthand of OntoEMS quantity, e.g. AirTemperature
    """

    # Construct KG client and execute query
    query_string = instantiated_observation_timeseries(stations)
    kg_client = KGClient(query_endpoint, update_endpoint)
    results = kg_client.performQuery(query=query_string)
    # Parse results into DataFrame
    df = pd.DataFrame(columns=['station', 'stationID', 'quantityType', 'dataIRI', 'tsIRI'])
    df = df.append(results)
    # Add column with shorthand of quantity type
    df['reading'] = df['quantityType'].apply(lambda x: x.split('/')[-1])
    
    return df


def get_instantiated_forecast_timeseries(stations: list = None,
                                         query_endpoint: str = QUERY_ENDPOINT,
                                         update_endpoint: str = UPDATE_ENDPOINT):
    """
        Returns DataFrame of (all) instantiated forecast timeseries in KG
        (data for all stations is returned if no stations list is provided)
        
        Arguments:
            stations - list of ReportingStation IRIs (WITHOUT trailing '<' and '>'
                       for which to retrieve data)

        Returns DataFrame with columns: ['station', 'stationID', 'quantityType', 'dataIRI', 'unit', 'tsIRI', 'reading']
            station: station IRI
            stationID: unique Met Office ID for that station
            quantityType: IRI of OntoEMS quantity, e.g. https://www.theworldavatar.com/kg/ontoems/AirTemperature
            dataIRI: IRI of quantity instance to which time series is attached
            unit - unit for time series, e.g. hPa
            tsIRI: IRI of time series instance
            reading: shorthand of OntoEMS quantity, e.g. AirTemperature
    """

    # Construct KG client and execute query
    query_string = instantiated_forecast_timeseries(stations)
    kg_client = KGClient(query_endpoint, update_endpoint)
    results = kg_client.performQuery(query=query_string)
    # Parse results into DataFrame
    df = pd.DataFrame(columns=['station', 'stationID', 'quantityType', 'dataIRI', 'tsIRI'])
    df = df.append(results)
    # Add column with shorthand of quantity type
    df['reading'] = df['quantityType'].apply(lambda x: x.split('/')[-1])
    
    return df


def get_time_series_data(station_iris: list = None,
                         observation_types: list = None,
                         observations: bool = True,
                         forecasts: bool = True,
                         tmin: str = None, tmax: str = None,
                         query_endpoint: str = QUERY_ENDPOINT,
                         update_endpoint: str = UPDATE_ENDPOINT):
    """
        Retrieve time series data for provided observation types and stations from KG

        Arguments
            station_iris - list of station IRIs for which to retrieve time series data
                           (all stations if None)
            observation_types - list of observation types (e.g., AirTemperature)
                                for which to retrieve data (all if None)
            observations - boolean flag whether or not to retrieve observation data
            forecasts - boolean flag whether or not to retrieve forecast data
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
                logger.info('Provided time string assumed in UTC.')
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
    
    # Validate inputs
    # Requested time series data
    if not observations and not forecasts:
        return [], [], []
    # Format of provided tmin and tmax
    if tmin:
        try:
            tmin = _validate_time_format(tmin)
        except Exception as ex:
            logger.info(f'Provided format of tmin could not be derived. Expected format: {TIME_FORMAT}')
            raise InvalidInput(f'Provided format of tmin could not be derived. Expected format: {TIME_FORMAT}') from ex
    if tmax:
        try:
            tmax = _validate_time_format(tmax)
        except Exception as ex:
            logger.info(f'Provided format of tmax could not be derived. Expected format: {TIME_FORMAT}')
            raise InvalidInput(f'Provided format of tmax could not be derived. Expected format: {TIME_FORMAT}') from ex

    # Get DataFrames for observation and forecast time series
    if observations and forecasts:
        df1 = get_instantiated_observation_timeseries(station_iris, query_endpoint, update_endpoint)
        df1['reading_type'] = ' observation'
        df2 = get_instantiated_forecast_timeseries(station_iris, query_endpoint, update_endpoint)
        df2['reading_type'] = ' forecast'
        # Merge DataFrames
        df = pd.concat([df1, df2], ignore_index=True)
    elif observations:
        df = get_instantiated_observation_timeseries(station_iris, query_endpoint, update_endpoint)
        df['reading_type'] = ' observation'
    elif forecasts:
        df = get_instantiated_forecast_timeseries(station_iris, query_endpoint, update_endpoint)
        df['reading_type'] = ' forecast'

    # Get relevant subset of available time series data
    if observation_types:
        df = df[df['reading'].isin(observation_types)]

    # Get list of lists of dataIRIs to retrieve
    dataIRIs_list = [list(df.loc[df['tsIRI'] == tsIRI, 'dataIRI']) for tsIRI in df['tsIRI'].unique()]
    
    # Initialise return list
    ts_data = []
    ts_names = []
    ts_units = []
    
    # Initialise KG and TimeSeries Clients
    kg_client = KGClient(query_endpoint, update_endpoint)
    ts_client = TSClient(kg_client=kg_client, rdb_url=DB_URL, rdb_user=DB_USER, 
                         rdb_password=DB_PASSWORD)

    for dataIRIs in dataIRIs_list:

        # Get time series within desired bounds
        try:
            with ts_client.connect() as conn:
                ts_data.append(ts_client.tsclient.getTimeSeriesWithinBounds(dataIRIs, tmin, tmax, conn))
        except Exception as ex:
            logger.error(f'Error while retrieving time series data for dataIRIs: {dataIRIs}')
            raise TSException(f'Error while retrieving time series data for dataIRIs: {dataIRIs}') from ex
        
        # Get time series names and units (as dict with dataIRIs as key)
        df_sub = df.loc[df['dataIRI'].isin(dataIRIs), ['dataIRI','unit', 'reading', 'reading_type']]
        ts_names.append(dict(zip(df_sub['dataIRI'], df_sub['reading']+df_sub['reading_type'])))
        # NOTE: Fix encoding issue with special characters
        df_sub['unit'] = df_sub['unit'].apply(lambda x: x.encode('ISO-8859-1').decode('utf-8'))
        ts_units.append(dict(zip(df_sub['dataIRI'], df_sub['unit'])))

    return ts_data, ts_names, ts_units
