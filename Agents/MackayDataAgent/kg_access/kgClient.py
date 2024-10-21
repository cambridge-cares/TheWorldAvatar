
# The purpose of this module is to provide functionality to execute
# KG queries and updates using the RemoteStoreClient from the JPS_BASE_LIB

import json
from .java_gateway_mc import jpsBaseLibView
import logging


class KGClient:

    def __init__(self, query_endpoint, update_endpoint, kg_user=None,
                 kg_password=None):
        logging.basicConfig(level=logging.DEBUG)
        self.jpsBaseLib_view = jpsBaseLibView().getView()

        try:
            if kg_user is not None:
                self.kg_client = self.jpsBaseLib_view.RemoteStoreClient(query_endpoint, update_endpoint, kg_user,
                                                                        kg_password)
            else:
                self.kg_client = self.jpsBaseLib_view.RemoteStoreClient(query_endpoint, update_endpoint)
        except Exception as ex:
            logging.error("Unable to initialise KG client.")

    def performQuery(self, query):
        """
            This function performs query to knowledge graph.
            Arguments:
                query - SPARQL Query string
        """
        try:
            response = self.kg_client.execute(query)
        except Exception as ex:
            logging.error("SPARQL query not successful.")
        return json.loads(response)
