################################################
# Authors: Markus Hofmeister (mh807@cam.ac.uk) #    
# Date: 25 Jul 2023                            #
################################################

# The purpose of this module is to provide functionality to execute
# KG queries and updates using the StoreRouter from the JPS_BASE_LIB

import json

from py4jps import agentlogging

from pyderivationagent.kg_operations import PySparqlClient

from forecastingagent.errorhandling.exceptions import KGException
from forecastingagent.utils.baselib_gateway import jpsBaseLibGW

# Initialise logger instance (ensure consistent logger level`)
logger = agentlogging.get_logger('prod')


class KGClient(PySparqlClient):
    
    #
    # SPARQL QUERIES
    #

    #
    # SPARQL UPDATES
    # 

    pass    