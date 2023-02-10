################################################
# Authors: Markus Hofmeister (mh807@cam.ac.uk) #    
# Date: 10 Feb 2023                            #
################################################

# The purpose of this module is to provide functions to retrieve 
# flood warnings and alerts from the API and instantiate it in the KG

import uuid

from py4jps import agentlogging
from agent.errorhandling.exceptions import APIException
from agent.kgutils.kgclient import KGClient
from agent.kgutils.querytemplates import *
from agent.utils.stackclients import (GdalClient, GeoserverClient,
                                      OntopClient, PostGISClient,
                                      create_geojson_for_postgis)
from agent.utils.stack_configs import QUERY_ENDPOINT, UPDATE_ENDPOINT

# Initialise logger
logger = agentlogging.get_logger("prod")


def update_warnings(county=None):
    """
    Update flood warnings and alerts in the KG
    """

    return None

