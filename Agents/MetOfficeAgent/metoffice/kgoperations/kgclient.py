# The purpose of this module is to provide functionality to execute
# KG queries and updates using the StoreRouter from the JPB_BASE_LIB

from metoffice.kgoperations.javagateway import jpsBaseLibGW
import json


class KGClient:
    def __init__(self, query_endpoint, update_endpoint, kg_user=None, 
                 kg_password=None) -> None:

        # create a JVM module view and use it to import the required java classes
        self.jpsBaseLib_view = jpsBaseLibGW.createModuleView()
        jpsBaseLibGW.importPackages(self.jpsBaseLib_view,"uk.ac.cam.cares.jps.base.query.*")

        # TODO replace RemoteStoreClient with AccessAgent/StoreClient once its
        # tested and
        # StoreRouter = jpsBaseLib_view.StoreRouter
        # # Get StoreClientInterface Java object to sparqlEndPoint with
        # # query operations enabled (True) and update operations disabled (False)
        # StoreClient = StoreRouter.getStoreClient(sparqlEndPoint, True, False)

        if kg_user is not None:
            self.kg_client = self.jpsBaseLib_view.RemoteStoreClient(query_endpoint, update_endpoint, kg_user, kg_password)
        else:
            self.kg_client = self.jpsBaseLib_view.RemoteStoreClient(query_endpoint, update_endpoint)

    
    def performQuery(self, query):
        """
            This function performs query to knowledge graph.
            Arguments:
                query - SPARQL Query string
        """
        response = self.kg_client.execute(query)
        return json.loads(response)


    def performUpdate(self, update):
        """
            This function performs SPARQL Update to knowledge graph.
            Arguments:
                update - SPARQL Update string
        """
        self.kg_client.executeUpdate(update)
