################################################
# Authors: Markus Hofmeister (mh807@cam.ac.uk) #    
# Date: 10 Feb 2023                            #
################################################

# The purpose of this module is to provide functions to retrieve 
# flood warnings and alerts from the API and instantiate it in the KG

import uuid

from agent.datainstantiation.ea_data import retrieve_current_warnings

from agent.errorhandling.exceptions import APIException
from agent.kgutils.kgclient import KGClient
from agent.kgutils.querytemplates import *
from agent.utils.stackclients import (GdalClient, GeoserverClient,
                                      OntopClient, PostGISClient,
                                      create_geojson_for_postgis)
from agent.utils.stack_configs import QUERY_ENDPOINT, UPDATE_ENDPOINT

from py4jps import agentlogging

# Initialise logger
logger = agentlogging.get_logger("prod")


def update_warnings(county=None):
    """
    Update instantiated flood warnings/alerts incl. associated flood areas in the KG, 
    i.e. instantiate missing ones, update existing ones, and archive outdated ones

    Arguments:
    county (str): County name for which to instantiate flood warnings (e.g. 'Hampshire')
                  Instantiates ALL current warnings if no county given
    """

    # Create KG client 
    kgclient = KGClient(QUERY_ENDPOINT, UPDATE_ENDPOINT)

    if county:
        #TODO: Implement county-specific instantiation, see note in agent\flaskapp\inputtasks\routes.py
        logger.warning("County-specific instantiation not yet implemented")
    else:
        # Retrieve current flood warnings from API
        warnings_api = retrieve_current_warnings()
        # Retrieve instantiated flood warnings and flood areas from KG
        #areas_kg = get_instantiated_flood_areas(kgclient=kgclient)
        #warnings_kg = get_instantiated_flood_warnings(kgclient=kgclient)
        instantiate_flood_warnings(warnings_api)

    return None, None, None


def get_instantiated_flood_warnings(query_endpoint=QUERY_ENDPOINT,
                                    kgclient=None):
    """
    Retrieve all instantiated flood warnings with latest update timestamp

    Arguments:
        query_endpoint - SPARQL endpoint from which to retrieve data
        kgclient - pre-initialized KG client with endpoints

    Returns:
        warnings (dict): Dictionary with flood warning/alert IRIs as keys and
                         latest update timestamp as values
    """

    # Create KG client if not provided
    if not kgclient:
        kgclient = KGClient(query_endpoint, query_endpoint)
    
    # Retrieve instantiated flood warnings
    query = get_all_flood_warnings()
    res = kgclient.performQuery(query)

    # Unwrap results
    # uprns = [r['uprns'] for r in res]

    # # Extract latest update time stamp
    # last_altered = [d['timeRaised'], d['timeMsgChanged'], d['timeSevChanged']]
    # last_altered = [dt.strptime(last, '%Y-%m-%dT%H:%M:%S') for last in last_altered]
    # d['last_altered'] = max(last_altered).strftime('%Y-%m-%dT%H:%M:%S')

    return res


def get_instantiated_flood_areas(query_endpoint=QUERY_ENDPOINT,
                                 kgclient=None):
    """
    Retrieve all instantiated flood areas with associated 'hasLocation' Location IRIs

    Arguments:
        query_endpoint - SPARQL endpoint from which to retrieve data
        kgclient - pre-initialized KG client with endpoints

    Returns:
        areas (dict): Dictionary with flood area IRIs as keys and associated 
                      Location IRIs as values
    """

    # Create KG client if not provided
    if not kgclient:
        kgclient = KGClient(query_endpoint, query_endpoint)
    
    # Retrieve instantiated flood warnings
    query = get_all_flood_areas()
    res = kgclient.performQuery(query)

    # Unwrap results
    # uprns = [r['uprns'] for r in res]

    # # Extract latest update time stamp
    # last_altered = [d['timeRaised'], d['timeMsgChanged'], d['timeSevChanged']]
    # last_altered = [dt.strptime(last, '%Y-%m-%dT%H:%M:%S') for last in last_altered]
    # d['last_altered'] = max(last_altered).strftime('%Y-%m-%dT%H:%M:%S')

    return res


def instantiate_flood_areas(areas_data: list=[],
                            query_endpoint=QUERY_ENDPOINT,
                            kgclient=None):
    """
    Instantiate list of flood area data dicts as retrieved from API by 'retrieve_flood_area_data'

    Arguments:
        areas_data (list): List of dicts with relevant flood area data
        query_endpoint - SPARQL endpoint from which to retrieve data
        kgclient - pre-initialized KG client with endpoints

    Returns:
        Number (int) of instantiated flood areas
    """

    # Create KG client if not provided
    if not kgclient:
        kgclient = KGClient(query_endpoint, query_endpoint)

    triples = ''
    for area in areas_data:
        # Prepare instantiation in KG
        triples += flood_area_instantiation_triples(**area)
        # Upload polygon to PostGIS

    # Create INSERT query and perform update
    query = f"INSERT DATA {{ {triples} }}"
    kgclient.performUpdate(query)

    return len(areas_data)


def instantiate_flood_warnings(warnings_data: list=[],
                               query_endpoint=QUERY_ENDPOINT,
                               kgclient=None):
    """
    Instantiate list of flood warning data dicts as retrieved from API by 'retrieve_current_warnings'

    Arguments:
        warnings_data (list): List of dicts with relevant flood warnings/alerts data
        query_endpoint - SPARQL endpoint from which to retrieve data
        kgclient - pre-initialized KG client with endpoints

    Returns:
        Number (int) of instantiated flood warnings/alerts
    """

    # Create KG client if not provided
    if not kgclient:
        kgclient = KGClient(query_endpoint, query_endpoint)

    triples = ''
    for warning in warnings_data:
        triples += flood_warning_instantiation_triples(**warning)

    # Create INSERT query and perform update
    query = f"INSERT DATA {{ {triples} }}"
    kgclient.performUpdate(query)

    return len(warnings_data)
     