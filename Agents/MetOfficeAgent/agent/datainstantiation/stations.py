################################################
# Authors: Markus Hofmeister (mh807@cam.ac.uk) #    
# Date: 01 Apr 2022                            #
################################################

# The purpose of this module is to provide functions to retrieve 
# station data from the API and instantiate it in the KG

import uuid
import metoffer

from py4jps import agentlogging
from agent.dataretrieval.stations import get_all_metoffice_station_ids
from agent.errorhandling.exceptions import APIException
from agent.kgutils.kgclient import KGClient
from agent.kgutils.querytemplates import *
from agent.kgutils.stackclients import (GdalClient, GeoserverClient,
                                        OntopClient, PostGISClient,
                                        create_geojson_for_postgis)
from agent.utils.env_configs import DATAPOINT_API_KEY
from agent.utils.stack_configs import QUERY_ENDPOINT, UPDATE_ENDPOINT

# Initialise logger
logger = agentlogging.get_logger("prod")


def instantiate_stations(station_data: list,
                         query_endpoint: str = QUERY_ENDPOINT,
                         update_endpoint: str = UPDATE_ENDPOINT) -> None:
    """
        Instantiates a list of measurement stations in a single SPARQL update
        
        Arguments:
            station_data - list of dictionaries with station parameters as returned
                           from MetOffice DataPoint using metoffer
    """

    # Initialise relevant Stack Clients and parameters
    postgis_client = PostGISClient()
    gdal_client = GdalClient()
    geoserver_client = GeoserverClient()
    station_type = 'MetOffice Reporting station'
    
    # Initialise update SPARQL query
    query_string = f"""
        INSERT DATA {{
    """

    # Add station details
    for data in station_data:
        station_IRI = KB + 'ReportingStation_' + str(uuid.uuid4())
        # Extract station information from API result
        to_instantiate = _condition_metoffer_data(data)
        to_instantiate['station_iri'] = station_IRI
        # Remove location and subtype info to be handled separately
        lat, lon = to_instantiate.pop('location').split('#')
        station_subtype = to_instantiate.pop('subtype')

        # Create SPARQL update for Blazegraph
        query_string += add_station_data(**to_instantiate)

        # Create GeoJSON file for upload to PostGIS
        lat = float(lat)
        lon = float(lon)
        station_name = station_type + f' at {lat},{lon}' if not \
                       to_instantiate.get('label') else to_instantiate.get('label')        
        geojson = create_geojson_for_postgis(station_iri=station_IRI, station_name=station_name,
                                             station_type=station_type, station_subtype=station_subtype,
                                             lat=lat, long=lon, kg_endpoint=query_endpoint)

        # Upload OBDA mapping and create Geoserver layer when first geospatial
        # data is uploaded to PostGIS
        if not postgis_client.check_table_exists():
            logger.info('Uploading OBDA mapping ...')
            OntopClient.upload_ontop_mapping()
            # Initial data upload required to create postGIS table and Geoserver layer            
            logger.info('Uploading GeoJSON to PostGIS ...')
            gdal_client.uploadGeoJSON(geojson)
            logger.info('Creating layer in Geoserver ...')
            geoserver_client.create_workspace()
            geoserver_client.create_postgis_layer()
        else:        
            # Upload new geospatial information
            if not postgis_client.check_point_feature_exists(lat, lon, station_type):
                logger.info('Uploading GeoJSON to PostGIS ...')
                gdal_client.uploadGeoJSON(geojson)

    # Close query
    query_string += f"}}"

    # Execute query
    kg_client = KGClient(query_endpoint, update_endpoint)
    kg_client.performUpdate(query_string)


def retrieve_station_data_from_api(api_key: str = None) -> list:
    """
        Retrieve station data from Met Office DataPoint via MetOffer wrapper

        Arguments:
            api_key - API key for MetOffice DataPoint
        Returns:
            List of dicts with station data as returned by MetOffer wrapper
    """

    # Create MetOffice client
    if not api_key:
        logger.error("No Met Office DataPoint API key provided.")
        raise APIException("No Met Office DataPoint API key provided.")
    else:
        # Initialise MetOffer client
        metclient = metoffer.MetOffer(api_key)
        obs_sites = fcs_sites = []
        try:
            #print('Retrieving station data from API ...')
            logger.info('Retrieving station data from API ...')
            # 1) Get all observations sites
            sites = metclient.loc_observations(metoffer.SITELIST)
            obs_sites = sites['Locations']['Location']
            # 2) Get all forecasts sites
            sites = metclient.loc_forecast(metoffer.SITELIST, metoffer.THREE_HOURLY)
            fcs_sites = sites['Locations']['Location']
            #print('Station data successfully retrieved.')
            logger.info('Station data successfully retrieved.')
        except Exception as ex:
            logger.error("Error while retrieving station data from DataPoint.")
            raise APIException("Error while retrieving station data from DataPoint.") from ex
        sites = []
        sites += obs_sites 
        sites += fcs_sites
        # Remove potential station duplicates
        unique_sites = [s for n, s in enumerate(sites) if s not in sites[n + 1:]]

        # Add observation type to stations, i.e. use 'forecast' station for stations which 
        # report both (as there are much more forecast stations than observation stations)
        obs_ids = set([site['id'] for site in obs_sites])
        unique_sites = [dict(site, **{'type':'observation'}) if site['id'] in obs_ids else \
                        dict(site, **{'type':'forecast'}) for site in unique_sites]

    return unique_sites


def instantiate_all_stations(api_key: str = DATAPOINT_API_KEY,
                             query_endpoint: str = QUERY_ENDPOINT,
                             update_endpoint: str = UPDATE_ENDPOINT) -> int:
    """
        Instantiate all weather stations available via Met Office DataPoint

        Returns:
            Number of instantiated stations
    """

    # Get all available stations from API
    # MetOffice station IDs as unique references for stations
    available = retrieve_station_data_from_api(api_key)
    available_ids = [s['id'] for s in available]

    # Get already instantiated stations
    instantiated_ids = get_all_metoffice_station_ids(query_endpoint=query_endpoint)

    # Derive non yet instantiated stations
    missing_ids = [s for s in available_ids if not s in instantiated_ids]
    to_instantiate = [s for s in available if s['id'] in missing_ids]

    # Instantiate missing stations
    #print('Instantiate/update stations in KG ...')
    logger.info('Instantiate/update stations in KG ...')
    instantiate_stations(station_data=to_instantiate,
                         query_endpoint=query_endpoint,
                         update_endpoint=update_endpoint)
    #print('Stations successfully instantiated/updated.')
    logger.info('Stations successfully instantiated/updated.')
    
    return len(missing_ids)


def _condition_metoffer_data(station_data: dict) -> dict:
    """
        Condition retrieved MetOffer data as required for query template
        Arguments:
            station_data - station data dict as returned by MetOffer
    """

    # Data format as required for "add_station_data" query template
    conditioned = {'dataSource': "Met Office DataPoint",
                   'id': None,
                   'label': None,
                   'location': None,
                   'elevation': None,
                   # Station subtype defaults to Forecast station (as there are much more of them)
                   'subtype': 'forecast'
    }
    # Extract relevant data
    if 'id' in station_data.keys(): conditioned['id'] = station_data['id']
    if 'name' in station_data.keys(): conditioned['label'] = station_data['name']
    if 'elevation' in station_data.keys(): conditioned['elevation'] = station_data['elevation']
    if 'type' in station_data.keys(): conditioned['subtype'] = station_data['type']
    if ('latitude' in station_data.keys()) and ('longitude' in station_data.keys()):
        conditioned['location'] = station_data['latitude'] + '#' + station_data['longitude']
    else:
        logger.warning(f"Station {station_data['id']} does not have location data.")
        #print(f"Station {station_data['id']} does not have location data.")
    
    return conditioned
