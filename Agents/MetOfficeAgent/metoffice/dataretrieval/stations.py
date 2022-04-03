###############################################
# Authors: Markus Hofmeister (mh807cam.ac.uk) #    
# Date: 01 Apr 2022                           #
###############################################

# The purpose of this module is to provide functions to retrieve 
# station data from the KG

import agentlogging

from metoffice.utils.properties import QUERY_ENDPOINT, UPDATE_ENDPOINT
from metoffice.kgutils.kgclient import KGClient
from metoffice.kgutils.querytemplates import *


# Initialise logger
logger = agentlogging.get_logger("dev")


def get_all_metoffice_stations(query_endpoint: str = QUERY_ENDPOINT,
                               update_endpoint: str = UPDATE_ENDPOINT):
    """
        Returns list of MetOffice IDs of all instantiated stations
    """
    
    # Construct KG client with correct query
    query_string = all_metoffice_station_ids()
    kg_client = KGClient(query_endpoint, update_endpoint)
    # Execute query
    results = kg_client.performQuery(query=query_string)
    # Extract results
    res = [r['id'] for r in results] if results else []
    
    return res
