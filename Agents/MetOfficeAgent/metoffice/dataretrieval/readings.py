###############################################
# Authors: Markus Hofmeister (mh807cam.ac.uk) #    
# Date: 05 Apr 2022                           #
###############################################

# The purpose of this module is to provide functions to retrieve 
# readings data from KG

import pandas as pd

#import agentlogging
from metoffice.kgutils.kgclient import KGClient
from metoffice.kgutils.querytemplates import *
from metoffice.utils.properties import QUERY_ENDPOINT, UPDATE_ENDPOINT


# # Initialise logger
# logger = agentlogging.get_logger("dev")


def get_all_instantiated_observations(query_endpoint: str = QUERY_ENDPOINT,
                                      update_endpoint: str = UPDATE_ENDPOINT):
    """
        Returns DataFrame of all instantiated observations in KG
    """

    # Construct KG client and execute query
    query_string = all_instantiated_observations()
    kg_client = KGClient(query_endpoint, update_endpoint)
    results = kg_client.performQuery(query=query_string)
    # Parse results into DataFrame
    df = pd.DataFrame(columns=['station', 'stationID', 'quantityType', 'dataIRI', 'tsIRI'])
    df = df.append(results)
    
    return df


def get_all_instantiated_forecasts(query_endpoint: str = QUERY_ENDPOINT,
                                   update_endpoint: str = UPDATE_ENDPOINT):
    """
        Returns DataFrame of all instantiated forecasts in KG
    """

    # Construct KG client and execute query
    query_string = all_instantiated_forecasts()
    kg_client = KGClient(query_endpoint, update_endpoint)
    results = kg_client.performQuery(query=query_string)
    # Parse results into DataFrame
    df = pd.DataFrame(columns=['station', 'stationID', 'quantityType', 'dataIRI', 'tsIRI'])
    df = df.append(results)
    
    return df


def get_all_instantiated_observation_timeseries(query_endpoint: str = QUERY_ENDPOINT,
                                                update_endpoint: str = UPDATE_ENDPOINT):
    """
        Returns DataFrame of all instantiated observation timeseries in KG
    """

    # Construct KG client and execute query
    query_string = all_instantiated_observation_timeseries()
    kg_client = KGClient(query_endpoint, update_endpoint)
    results = kg_client.performQuery(query=query_string)
    # Parse results into DataFrame
    df = pd.DataFrame(columns=['station', 'stationID', 'quantityType', 'dataIRI', 'tsIRI'])
    df = df.append(results)
    
    return df


def get_all_instantiated_forecast_timeseries(query_endpoint: str = QUERY_ENDPOINT,
                                             update_endpoint: str = UPDATE_ENDPOINT):
    """
        Returns DataFrame of all instantiated observation timeseries in KG
    """

    # Construct KG client and execute query
    query_string = all_instantiated_forecast_timeseries()
    kg_client = KGClient(query_endpoint, update_endpoint)
    results = kg_client.performQuery(query=query_string)
    # Parse results into DataFrame
    df = pd.DataFrame(columns=['station', 'stationID', 'quantityType', 'dataIRI', 'tsIRI'])
    df = df.append(results)
    
    return df
