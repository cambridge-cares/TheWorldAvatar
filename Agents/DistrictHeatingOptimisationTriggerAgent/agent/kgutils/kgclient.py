################################################
# Authors: Markus Hofmeister (mh807@cam.ac.uk) #    
# Date: 08 Apr 2022                            #
################################################

# The purpose of this module is to provide functionality to execute
# KG queries and updates using the StoreRouter from the JPS_BASE_LIB

import json

from py4jps import agentlogging
from pyderivationagent.kg_operations import PySparqlClient

from agent.datamodel.iris import *

# Initialise logger instance (ensure consistent logger level`)
logger = agentlogging.get_logger('prod')


class KGClient(PySparqlClient):
    #
    # SPARQL UPDATES
    #
    def instantiate_time_instances(self, ):
        """
        ...

        """
        update = f"""            
        """
        update = self.remove_unnecessary_whitespace(update)
        self.performUpdate(update)

    #
    # Helper functions
    #
    def remove_unnecessary_whitespace(self, query: str) -> str:
        # Remove unnecessary whitespaces
        query = ' '.join(query.split())

        return query
