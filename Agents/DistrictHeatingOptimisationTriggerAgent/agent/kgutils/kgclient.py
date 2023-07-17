################################################
# Authors: Markus Hofmeister (mh807@cam.ac.uk) #    
# Date: 12 Jul 2022                            #
################################################

# The purpose of this module is to provide functionality to execute
# KG queries and updates using the StoreRouter from the JPS_BASE_LIB

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
    def instantiate_time_interval(self, interval_iri: str, beginning_iri: str, end_iri: str,
                                  unix_beginning: int, unix_end: int):
        # Instantiate new time interval instance
        update = self.instantiate_time_interval_query(interval_iri, beginning_iri, end_iri)
        update = self.remove_unnecessary_whitespace(update)
        self.performUpdate(update)
        # Instantiate corresponding time:Instant instances
        self.instantiate_time_instant(beginning_iri, unix_beginning)
        self.instantiate_time_instant(end_iri, unix_end)


    def instantiate_time_instant(self, instance_iri: str, unix_time_iri: int,
                                  instance_type:str = TIME_INSTANT):
        # Instantiate new time:Instant instance
        update = self.instantiate_time_instant_query(instance_iri, unix_time_iri,
                                                     instance_type)
        update = self.remove_unnecessary_whitespace(update)
        self.performUpdate(update)


    def instantiate_time_duration(self, duration_iri: str, unit_iri: str, value: float,
                                  rdf_type: str = TIME_DURATION):
        # Instantiate new time:Duration instance
        update = self.instantiate_time_duration_query(duration_iri, rdf_type,
                                                      unit_iri, value)
        update = self.remove_unnecessary_whitespace(update)
        self.performUpdate(update)


    def update_time_instant(self, instance_iri: str, unix_time_iri: int):
        # Update existing time:Instant instance
        update = self.update_time_instant_query(instance_iri, unix_time_iri)
        update = self.remove_unnecessary_whitespace(update)
        self.performUpdate(update)


    def instantiate_time_interval_query(self, interval_iri: str, beginning_iri: str, end_iri: str) -> str:
        # Create SPARQL update to instantiate new time interval
        query = f"""
        INSERT DATA {{
            <{interval_iri}> <{RDF_TYPE}> <{TIME_INTERVAL}> ; 
                             <{TIME_HASBEGINNING}> <{beginning_iri}> ;
                             <{TIME_HASEND}> <{end_iri}> .
        }}
        """

        return query
    

    def instantiate_time_duration_query(self, duration_iri: str, rdf_type: str,
                                        unit_iri: str, value: float) -> str:
        # Create SPARQL update to instantiate new time duration
        # rdf:type shall be either TIME_DURATION or TS_FREQUENCY
        query = f"""
        INSERT DATA {{
            <{duration_iri}> <{RDF_TYPE}> <{rdf_type}> ; 
                             <{TIME_UNIT_TYPE}> <{unit_iri}> ;
                             <{TIME_NUMERICDURATION}> "{value}"^^<{XSD_DECIMAL}> .
        }}
        """

        return query
    

    def instantiate_time_instant_query(self, instance_iri: str, unix_time_iri: int,
                                       instance_type:str) -> str:
        # Create SPARQL update to instantiate new time:Instant
        time_pos_iri = KB + 'TimePosition_' + str(uuid.uuid4())

        query = f"""
        INSERT DATA {{
            <{instance_iri}> <{RDF_TYPE}> <{instance_type}> ; 
                             <{TIME_INTIMEPOSITION}> <{time_pos_iri}> .
            <{time_pos_iri}> <{RDF_TYPE}> <{TIME_TIMEPOSITION}> ;
                             <{TIME_NUMERICPOSITION}> "{unix_time_iri}"^^<{XSD_DECIMAL}> ;
                             <{TIME_HASTRS}> <{UNIX_TIME}> .
        }}
        """

        return query
    

    def update_time_instant_query(self, instance_iri: str, unix_time_iri: int) -> str:
        # Create SPARQL update to update time stamp of instantiate time:Instant

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
