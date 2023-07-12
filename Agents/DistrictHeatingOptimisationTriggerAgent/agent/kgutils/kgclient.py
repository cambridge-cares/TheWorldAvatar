################################################
# Authors: Markus Hofmeister (mh807@cam.ac.uk) #    
# Date: 08 Apr 2022                            #
################################################

# The purpose of this module is to provide functionality to execute
# KG queries and updates using the StoreRouter from the JPS_BASE_LIB

import json
import uuid

from py4jps import agentlogging
from pyderivationagent.kg_operations import PySparqlClient

from agent.datamodel.iris import *

# Initialise logger instance (ensure consistent logger level`)
logger = agentlogging.get_logger('prod')


class KGClient(PySparqlClient):
    #
    # SPARQL UPDATES
    #
    def instantiate_time_instance(self, instance_iri: str, unix_time_iri: int):
        # Instantiate new time instance
        update = self.instantiate_time_instance_query(instance_iri, unix_time_iri)
        update = self.remove_unnecessary_whitespace(update)
        self.performUpdate(update)

    def update_time_instance(self, instance_iri: str, unix_time_iri: int):
        # Update existing time instance
        update = self.update_time_instance_query(instance_iri, unix_time_iri)
        update = self.remove_unnecessary_whitespace(update)
        self.performUpdate(update)


    def instantiate_time_instance_query(self, instance_iri: str, unix_time_iri: int) -> str:
        # Create SPARQL update to instantiate new time instance
        time_pos_iri = KB + 'TimePosition_' + str(uuid.uuid4())

        query = f"""
        INSERT DATA {{
            <{instance_iri}> <{RDF_TYPE}> <{OD_SIMULATION_TIME}> ; 
                             <{TIME_INTIMEPOSITION}> <{time_pos_iri}> .
            <{time_pos_iri}> <{RDF_TYPE}> <{TIME_TIMEPOSITION}> ;
                             <{TIME_NUMERICPOSITION}> "{unix_time_iri}"^^<{XSD_DECIMAL}> ;
                             <{TIME_HASTRS}> <{UNIX_TIME}> .
        }}
        """

        return query
    
    def update_time_instance_query(self, instance_iri: str, unix_time_iri: int) -> str:
        # Create SPARQL update to update time stamp of instantiate time instance

        query = f"""
        DELETE {{
            ?time_pos <{TIME_NUMERICPOSITION}> ?old_unix . 
        }} INSERT {{
            ?time_pos <{TIME_NUMERICPOSITION}> "{unix_time_iri}"^^<{XSD_DECIMAL}> .
        }} WHERE {{
            <{instance_iri}> <{TIME_INTIMEPOSITION}> ?time_pos .
            ?time_pos <{TIME_NUMERICPOSITION}> ?old_unix .
        }}
        """

        return query

    #
    # Helper functions
    #
    def remove_unnecessary_whitespace(self, query: str) -> str:
        # Remove unnecessary whitespaces
        query = ' '.join(query.split())

        return query
