###############################################
# Authors: Markus Hofmeister (mh807cam.ac.uk) #    
# Date: 05 Apr 2022                           #
###############################################

# The purpose of this module is to provide functions to retrieve 
# readings data from the API and instantiate it in the KG

import uuid
import metoffer
import datetime as dt

#import agentlogging
from metoffice.dataretrieval.stations import get_all_metoffice_station_ids
from metoffice.errorhandling.exceptions import APIException
from metoffice.kgutils.kgclient import KGClient
from metoffice.kgutils.prefixes import create_sparql_prefix
from metoffice.kgutils.prefixes import PREFIXES
from metoffice.kgutils.querytemplates import *
from metoffice.utils.properties import QUERY_ENDPOINT, UPDATE_ENDPOINT, DATAPOINT_API_KEY
from metoffice.utils.readings_mapping import READINGS_MAPPING, COMPASS

# # Initialise logger
# logger = agentlogging.get_logger("dev")


# def instantiate_station_readings(instantiated_sites_list: list,
#                                  query_endpoint: str = QUERY_ENDPOINT,
#                                  update_endpoint: str = UPDATE_ENDPOINT) -> None:
#     """
#         Instantiates readings for the provided list of measurement stations
        
#         Arguments:
#             instantiated_sites_list - list of dictionaries with instantiated
#                                       stations/sites in the form [{id : iri},]
#     """
    
#     # Initialise update query
#     query_string = f"""
#         {create_sparql_prefix('geolit')}
#         {create_sparql_prefix('rdf')}
#         {create_sparql_prefix('rdfs')}
#         {create_sparql_prefix('xsd')}
#         {create_sparql_prefix('ems')}
#         {create_sparql_prefix('kb')}
#         INSERT DATA {{
#     """


#     # # Loop over all sites
#     # for site in instantiated_sites_list:


#     # # Add station details
#     # for data in station_data:
#     #     station_IRI = PREFIXES['kb'] + 'ReportingStation_' + str(uuid.uuid4())
#     #     # Extract station information from API result
#     #     to_instantiate = _condition_metoffer_data(data)
#     #     to_instantiate['station_iri'] = station_IRI
#     #     query_string += add_station_data(**to_instantiate)

#     # # Close query
#     # query_string += f"}}"

#     # # Execute query
#     # kg_client = KGClient(query_endpoint, update_endpoint)
#     # kg_client.performUpdate(query_string)


# def instantiate_readings_for_station(station_iri: str,
#                                      readings: list) -> str:
#     """

#     """

#     # Condition readings data

#     return ''


# def retrieve_station_readings_from_api(api_key: str = None) -> list:
#     """
#         Retrieve station data from Met Office DataPoint via MetOffer wrapper

#         Arguments:
#             api_key - API key for MetOffice DataPoint
#         Returns:
#             List of dicts with station data as returned by MetOffer wrapper
#     """

#     # Create MetOffice client
#     if not api_key:
#         #logger.error("No Met Office DataPoint API key provided.")
#         raise APIException("No Met Office DataPoint API key provided.")
#     else:
#         # Initialise MetOffer client
#         metclient = metoffer.MetOffer(api_key)
#         obs_sites = fcs_sites = []
#         try:
#             # 1) Get all observations sites
#             sites = metclient.loc_observations(metoffer.SITELIST)
#             obs_sites = sites['Locations']['Location']
#             # 2) Get all forecasts sites
#             sites = metclient.loc_forecast(metoffer.SITELIST, step=metoffer.THREE_HOURLY)
#             fcs_sites = sites['Locations']['Location']
#         except:
#             #logger.error("Error while retrieving station data from DataPoint.")
#             raise APIException("Error while retrieving station data from DataPoint.")
#         sites = []
#         sites += obs_sites 
#         sites += fcs_sites
#         # Remove potential duplicates
#         unique_sites = [s for n, s in enumerate(sites) if s not in sites[n + 1:]]
    
#     return unique_sites


def condition_readings_data(readings_data: list, only_keys: bool = True) -> dict:
    """
        Condition retrieved MetOffer readings as required for query template

        Arguments:
            readings_data - readings data dict as returned by MetOffer
            only_keys - boolean flag indicating whether only the keys (i.e. variable
                        names) shall be retrieved or also time series data
    """
    
    # Read all unique measurement variables
    read_variables = [v for data in readings_data for v in list(data.keys())]
    read_variables = list(set(read_variables))
    # Initialise dict of conditioned readings
    conditioned = dict(zip(list(READINGS_MAPPING.keys()), 
                                [None]*len(list(READINGS_MAPPING.keys()))))

    if not only_keys:
        # Read reported times and values for variables
        for read_var in read_variables:
            if read_var == 'timestamp':
                conditioned[read_var] = [dt.datetime.strftime(r[read_var][0], '%Y-%m-%dT%H:%M:%SZ') for r in readings_data]
            elif read_var in READINGS_MAPPING:
                if read_var == 'Wind Direction':
                    conditioned[read_var] = [float(COMPASS[r[read_var][0]]) for r in readings_data]
                else:
                    conditioned[read_var] = [float(r[read_var][0]) for r in readings_data]
    
    return conditioned
