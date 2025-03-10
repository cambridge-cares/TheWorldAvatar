import json
from agent.exceptions import StackException
from twa.jps_singletons import jps_gateway
from twa import agentlogging

class OntopClient():
    stackClients_view = jps_gateway.createModuleView()
    logger = agentlogging.get_logger("PostGISClient")

    def __init__(self, query_endpoint: str):
        # Initialise OntopClient as RemoteStoreClient
        try:
            self.ontop_client = self.jpsBaseLib_view.RemoteStoreClient(query_endpoint)
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
            response = self.ontop_client.execute(query)
        except Exception as ex:
            self.logger.error("SPARQL query not successful")
            raise StackException("SPARQL query not successful.") from ex
        return json.loads(response)
