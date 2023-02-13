################################################
# Authors: Markus Hofmeister (mh807@cam.ac.uk) #    
# Date: 10 Feb 2023                            #
################################################

# The purpose of this module is to provide functions to retrieve 
# flood warnings and alerts from the API and instantiate it in the KG

import uuid

from datetime import datetime as dt

from agent.datainstantiation.ea_data import retrieve_current_warnings, \
                                            retrieve_flood_area_data
from agent.datainstantiation.ons_data import retrieve_ons_county

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
        warnings_data_api = retrieve_current_warnings()
        # Retrieve (currently) active flood warning and area URIs
        warning_uri = [w.get('warning_uri') for w in warnings_data_api]
        area_uris = [w.get('area_uri') for w in warnings_data_api]
        areas_data_api = []
        for area in area_uris:
            areas_data_api.append(retrieve_flood_area_data(area))

        # Retrieve instantiated flood warnings and flood areas from KG
        areas_kg = get_instantiated_flood_areas(kgclient=kgclient)
        warnings_kg = get_instantiated_flood_warnings(kgclient=kgclient)

        # Instantiate missing flood warnings and flood areas
        instantiated_areas = instantiate_flood_areas(areas_data_api, kgclient=kgclient)
        instantiated_warnings = instantiate_flood_warnings(warnings_data_api, kgclient=kgclient)


    return instantiated_warnings, None, None


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
    warning = [r.pop('warning_iri') for r in res]
    last_altered = [list(r.values()) for r in res]
    for i in range(len(last_altered)):
        last_altered[i] = [dt.strptime(t, BLAZEGRAPH_TIME_FORMAT) for t in last_altered[i]]
        last_altered[i] = max(last_altered[i])
    warnings = dict(zip(warning, last_altered))

    return warnings


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
    areas = {r['area_iri']: r['location_iri'] for r in res}

    return areas


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
        # Create IRIs of Location associated with flood area (and potential flood event)
        area['location_iri'] = KB + 'Location' + str(uuid.uuid4())
        area['admin_district_iri'] = KB + 'AdministrativeDistrict' + str(uuid.uuid4())
        # Create waterbody IRI
        area['waterbody_iri'] = KB + area['water_body_type'].capitalize() + str(uuid.uuid4())

        # Retrieve county IRI from ONS API
        area['county_iri'] = retrieve_ons_county(area['county'])

        # Remove dictionary keys not required for instantiation
        area.pop('county', None)
        area.pop('polygon_uri', None)

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
    Instantiate list of flood warning data dicts as retrieved from API by 'retrieve_current_warnings',
    further enriched with location_iri created by 'flood_area_instantiation_triples'

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
        # Create IRI of potential flood event
        warning['flood_event_iri'] = KB + 'Flood_' + str(uuid.uuid4())
        #TODO Remove
        warning['location_iri'] = KB + 'Location' + str(uuid.uuid4())
        # Remove dictionary keys not required for instantiation
        warning.pop('last_altered', None)

        triples += flood_warning_instantiation_triples(**warning)

    # Create INSERT query and perform update
    query = f"INSERT DATA {{ {triples} }}"
    kgclient.performUpdate(query)

    return len(warnings_data)
     