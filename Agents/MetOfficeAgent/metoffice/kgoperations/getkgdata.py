###############################################
# Authors: Markus Hofmeister (mh807cam.ac.uk) #    
# Date: 01 Apr 2022                           #
###############################################

# The purpose of this module is to provide templates for (frequently)

from metoffice.utils.settings import QUERY_ENDPOINT, UPDATE_ENDPOINT
from metoffice.kgoperations.kgclient import KGClient
from metoffice.kgoperations.querytemplates import *


def get_all_metoffice_stations():
    # Construct KG client with correct query
    query_string = all_metoffice_station_ids()
    kg_client = KGClient(QUERY_ENDPOINT, UPDATE_ENDPOINT)
    # Execute query
    results = kg_client.performQuery(query=query_string)
    # Extract results
    res = [r['id'] for r in results] if results else []
    
    return res
