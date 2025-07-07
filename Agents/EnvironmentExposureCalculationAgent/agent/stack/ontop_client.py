import json
from agent.exceptions import StackException
from twa import agentlogging
from twa.kg_operations import PySparqlClient

class OntopClient():

#todo: need to double check how is ontop client used in java 
    def __init__(self, query_endpoint: str):
        self.logger = agentlogging.get_logger("dev")
        
        # Initialise OntopClient as RemoteStoreClient
        try:
            self.ontop_client = PySparqlClient(query_endpoint, None)
        except Exception as ex:
            self.logger.error("Unable to initialise OntopClient.")
            raise StackException("Unable to initialise OntopClient.") from ex
    

    def performQuery(self, query):
        """
            This function performs query to Ontop endpoint.
            Arguments:
                query - SPARQL Query string
        """
        try:
            response = self.ontop_client.perform_query(query)
        except Exception as ex:
            self.logger.error("SPARQL query not successful")
            raise StackException("SPARQL query not successful.") from ex
        return response
