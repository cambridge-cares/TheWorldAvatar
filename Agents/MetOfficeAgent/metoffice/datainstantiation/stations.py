###############################################
# Authors: Markus Hofmeister (mh807cam.ac.uk) #    
# Date: 01 Apr 2022                           #
###############################################

# The purpose of this module is to provide functions to retrieve 
# station data from the API and instantiate it in the KG

import uuid
import metoffer

#import agentlogging
from metoffice.kgutils.querytemplates import *
from metoffice.kgutils.prefixes import create_sparql_prefix
from metoffice.dataretrieval.stations import get_all_metoffice_station_ids
from metoffice.kgutils.kgclient import KGClient
from metoffice.errorhandling.exceptions import APIException
from metoffice.kgutils.prefixes import PREFIXES
from metoffice.utils.properties import QUERY_ENDPOINT, UPDATE_ENDPOINT, DATAPOINT_API_KEY

# Initialise logger
#logger = agentlogging.get_logger("dev")


def instantiate_stations(station_data: list,
                         query_endpoint: str = QUERY_ENDPOINT,
                         update_endpoint: str = UPDATE_ENDPOINT) -> None:
    """
        Instantiates a list of measurement stations in a single SPARQL update
        
        Arguments:
            station_data - list of dictionaries with station parameters as returned
                           from MetOffice DataPoint using metoffer
    """
    
    # Initialise update query
    query_string = f"""
        {create_sparql_prefix('geolit')}
        {create_sparql_prefix('rdf')}
        {create_sparql_prefix('rdfs')}
        {create_sparql_prefix('xsd')}
        {create_sparql_prefix('ems')}
        {create_sparql_prefix('kb')}
        INSERT DATA {{
    """

    # Add station details
    for data in station_data:
        station_IRI = PREFIXES['kb'] + 'ReportingStation_' + str(uuid.uuid4())
        # Extract station information from API result
        to_instantiate = _condition_metoffer_data(data)
        to_instantiate['station_iri'] = station_IRI
        query_string += add_station_data(**to_instantiate)

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
        #logger.error("No Met Office DataPoint API key provided.")
        raise APIException("No Met Office DataPoint API key provided.")
    else:
        # Initialise MetOffer client
        metclient = metoffer.MetOffer(api_key)
        obs_sites = fcs_sites = []
        try:
            # 1) Get all observations sites
            sites = metclient.loc_observations(metoffer.SITELIST)
            obs_sites = sites['Locations']['Location']
            # 2) Get all forecasts sites
            sites = metclient.loc_forecast(metoffer.SITELIST, step=metoffer.THREE_HOURLY)
            fcs_sites = sites['Locations']['Location']
        except Exception as ex:
            #logger.error("Error while retrieving station data from DataPoint.")
            raise APIException("Error while retrieving station data from DataPoint")
        sites = []
        sites += obs_sites 
        sites += fcs_sites
        # Remove potential duplicates
        unique_sites = [s for n, s in enumerate(sites) if s not in sites[n + 1:]]
    
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
    instantiate_stations(station_data=to_instantiate,
                         query_endpoint=query_endpoint,
                         update_endpoint=update_endpoint)
    
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
                   'comment': None,
                   'location': None,
                   'elevation': None
    }
    # Extract relevant data
    if 'id' in station_data.keys(): conditioned['id'] = station_data['id']
    if 'name' in station_data.keys(): conditioned['comment'] = station_data['name']
    if 'elevation' in station_data.keys(): conditioned['elevation'] = station_data['elevation']
    if ('latitude' in station_data.keys()) and ('longitude' in station_data.keys()):
        conditioned['location'] = station_data['latitude'] + '#' + station_data['longitude']
    else:
        #logger.warning(f"Station {station_data['id']} does not have location data.")
        print(f"Station {station_data['id']} does not have location data.")
    
    return conditioned


if __name__ == '__main__':

    response = instantiate_all_stations()
    print(f"Number of instantiated stations: {response}")
    