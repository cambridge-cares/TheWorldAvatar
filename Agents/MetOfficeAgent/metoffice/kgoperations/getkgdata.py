###############################################
# Authors: Markus Hofmeister (mh807cam.ac.uk) #    
# Date: 01 Apr 2022                           #
###############################################

# The purpose of this module is to provide templates for (frequently)

from metoffice.utils.settings import QUERY_ENDPOINT, UPDATE_ENDPOINT
from metoffice.kgoperations.kgclient import KGClient
from metoffice.kgoperations.querytemplates import *


def get_all_metoffice_stations():
    query_string = all_metoffice_station_ids()
    kg_client = KGClient(QUERY_ENDPOINT, UPDATE_ENDPOINT)

    data = kg_client.performQuery(query=query_string)
    if data:
        pass
    return data

get_all_metoffice_stations()
