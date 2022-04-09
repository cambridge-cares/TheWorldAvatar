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

# Initialise logger
#logger = agentlogging.get_logger("dev")


def get_all_instantiated_observations(query_endpoint: str = QUERY_ENDPOINT,
                                      update_endpoint: str = UPDATE_ENDPOINT):
    """
        Returns DataFrame of all instantiated observations in KG
        
        Columns: ['station', 'stationID', 'quantityType', 'dataIRI', 'reading']
            station: station IRI
            stationID: unique Met Office ID for that station
            quantityType: IRI of OntoEMS quantity, e.g. http://www.theworldavatar.com/ontology/ontoems/OntoEMS.owl#AirTemperature
            dataIRI: IRI of quantity instance to which time series is attached
            reading: shorthand of OntoEMS quantity, e.g. AirTemperature
    """

    # Construct KG client and execute query
    query_string = all_instantiated_observations()
    kg_client = KGClient(query_endpoint, update_endpoint)
    results = kg_client.performQuery(query=query_string)
    # Parse results into DataFrame
    df = pd.DataFrame(columns=['station', 'stationID', 'quantityType', 'dataIRI'])
    df = df.append(results)
    # Add column with shorthand of quantity type
    df['reading'] = df['quantityType'].apply(lambda x: x.split('#')[-1])
    
    return df


def get_all_instantiated_forecasts(query_endpoint: str = QUERY_ENDPOINT,
                                   update_endpoint: str = UPDATE_ENDPOINT):
    """
        Returns DataFrame of all instantiated forecasts in KG

        Columns: ['station', 'stationID', 'quantityType', 'dataIRI', 'reading']
            station: station IRI
            stationID: unique Met Office ID for that station
            quantityType: IRI of OntoEMS quantity, e.g. http://www.theworldavatar.com/ontology/ontoems/OntoEMS.owl#AirTemperature
            dataIRI: IRI of quantity instance to which time series is attached
            reading: shorthand of OntoEMS quantity, e.g. AirTemperature
    """

    # Construct KG client and execute query
    query_string = all_instantiated_forecasts()
    kg_client = KGClient(query_endpoint, update_endpoint)
    results = kg_client.performQuery(query=query_string)
    # Parse results into DataFrame
    df = pd.DataFrame(columns=['station', 'stationID', 'quantityType', 'dataIRI'])
    df = df.append(results)
    # Add column with shorthand of quantity type
    df['reading'] = df['quantityType'].apply(lambda x: x.split('#')[-1])
    
    return df


def get_all_instantiated_observation_timeseries(query_endpoint: str = QUERY_ENDPOINT,
                                                update_endpoint: str = UPDATE_ENDPOINT):
    """
        Returns DataFrame of all instantiated observation timeseries in KG

        Columns: ['station', 'stationID', 'quantityType', 'dataIRI', 'tsIRI', 'reading']
            station: station IRI
            stationID: unique Met Office ID for that station
            quantityType: IRI of OntoEMS quantity, e.g. http://www.theworldavatar.com/ontology/ontoems/OntoEMS.owl#AirTemperature
            dataIRI: IRI of quantity instance to which time series is attached
            tsIRI: IRI of time series instance
            reading: shorthand of OntoEMS quantity, e.g. AirTemperature
    """

    # Construct KG client and execute query
    query_string = all_instantiated_observation_timeseries()
    kg_client = KGClient(query_endpoint, update_endpoint)
    results = kg_client.performQuery(query=query_string)
    # Parse results into DataFrame
    df = pd.DataFrame(columns=['station', 'stationID', 'quantityType', 'dataIRI', 'tsIRI'])
    df = df.append(results)
    # Add column with shorthand of quantity type
    df['reading'] = df['quantityType'].apply(lambda x: x.split('#')[-1])
    
    return df


def get_all_instantiated_forecast_timeseries(query_endpoint: str = QUERY_ENDPOINT,
                                             update_endpoint: str = UPDATE_ENDPOINT):
    """
        Returns DataFrame of all instantiated observation timeseries in KG

        Columns: ['station', 'stationID', 'quantityType', 'dataIRI', 'tsIRI', 'reading']
            station: station IRI
            stationID: unique Met Office ID for that station
            quantityType: IRI of OntoEMS quantity, e.g. http://www.theworldavatar.com/ontology/ontoems/OntoEMS.owl#AirTemperature
            dataIRI: IRI of quantity instance to which time series is attached
            tsIRI: IRI of time series instance
            reading: shorthand of OntoEMS quantity, e.g. AirTemperature
    """

    # Construct KG client and execute query
    query_string = all_instantiated_forecast_timeseries()
    kg_client = KGClient(query_endpoint, update_endpoint)
    results = kg_client.performQuery(query=query_string)
    # Parse results into DataFrame
    df = pd.DataFrame(columns=['station', 'stationID', 'quantityType', 'dataIRI', 'tsIRI'])
    df = df.append(results)
    # Add column with shorthand of quantity type
    df['reading'] = df['quantityType'].apply(lambda x: x.split('#')[-1])
    
    return df

if __name__ == '__main__':

    observation = get_all_instantiated_observations()
