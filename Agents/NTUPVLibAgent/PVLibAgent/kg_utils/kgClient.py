
# The purpose of this module is to provide functionality to execute
# KG queries and updates using the RemoteStoreClient from the JPS_BASE_LIB

import json

from PVLibAgent.error_handling.exceptions import KGException
from PVLibAgent.kg_utils.jpsSingletons import jpsBaseLibGW
import logging


class KGClient:

    def __init__(self, query_endpoint, update_endpoint, kg_user=None,
                 kg_password=None):
        logging.basicConfig(level=logging.DEBUG)
        # create a JVM module view and use it to import the required java classes
        self.jpsBaseLib_view = jpsBaseLibGW.createModuleView()
        jpsBaseLibGW.importPackages(self.jpsBaseLib_view, "uk.ac.cam.cares.jps.base.query.*")

        # TODO replace RemoteStoreClient with AccessAgent/StoreClient once its tested
        # StoreRouter = jpsBaseLib_view.StoreRouter
        # # Get StoreClientInterface Java object to sparqlEndPoint with
        # # query operations enabled (True) and update operations disabled (False)
        # StoreClient = StoreRouter.getStoreClient(sparqlEndPoint, True, False)

        try:
            if kg_user is not None:
                self.kg_client = self.jpsBaseLib_view.RemoteStoreClient(query_endpoint, update_endpoint, kg_user,
                                                                        kg_password)
            else:
                self.kg_client = self.jpsBaseLib_view.RemoteStoreClient(query_endpoint, update_endpoint)
        except Exception as ex:
            logging.error("Unable to initialise KG client.")
            raise KGException("Unable to initialise KG client.") from ex

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
            raise KGException("SPARQL query not successful.") from ex
        return json.loads(response)

    def performUpdate(self, update):
        """
            This function performs SPARQL Update to knowledge graph.
            Arguments:
                update - SPARQL Update string
        """
        try:
            self.kg_client.executeUpdate(update)
        except Exception as ex:
            logging.error("SPARQL update not successful.")
            raise KGException("SPARQL update not successful.") from ex
