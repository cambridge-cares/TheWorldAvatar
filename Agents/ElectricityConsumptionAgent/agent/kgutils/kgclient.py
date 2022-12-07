################################################
# Authors: Jieyang Xu (jx309@cam.ac.uk)        #    
# Date: 30/11 2022                             #
################################################

# The purpose of this module is to provide functionality to execute
# KG queries and updates using the RemoteStoreClient from the JPS_BASE_LIB

import json

import agentlogging
from agent.errorhandling.exceptions import KGException
from agent.kgutils.javagateway import jpsBaseLibGW

# Initialise logger
logger = agentlogging.get_logger("prod")

class KGClient:
    
    def __init__(self, query_endpoint, update_endpoint, kg_user=None, 
                 kg_password=None):

        # create a JVM module view and use it to import the required java classes
        self.jpsBaseLib_view = jpsBaseLibGW.createModuleView()
        jpsBaseLibGW.importPackages(self.jpsBaseLib_view,"uk.ac.cam.cares.jps.base.query.*")

        # TODO replace RemoteStoreClient with AccessAgent/StoreClient once its tested
        # StoreRouter = jpsBaseLib_view.StoreRouter
        # # Get StoreClientInterface Java object to sparqlEndPoint with
        # # query operations enabled (True) and update operations disabled (False)
        # StoreClient = StoreRouter.getStoreClient(sparqlEndPoint, True, False)

        print('query_endpoint:', query_endpoint)
        print('update_endpoint:', update_endpoint)
        try:
            if kg_user is not None:
                self.kg_client = self.jpsBaseLib_view.RemoteStoreClient(query_endpoint, update_endpoint, kg_user, kg_password)
                print("kg_user is not None")
            else:
                self.kg_client = self.jpsBaseLib_view.RemoteStoreClient(query_endpoint, update_endpoint)
                print("kg_user is None")
        except Exception as ex:
            print("kg_clinet cannot be created")
            logger.error("Unable to initialise KG client")
            raise KGException("Unable to initialise KG client.") from ex

    
    def performQuery(self, query):
        """
            This function performs query to knowledge graph.
            Arguments:
                query - SPARQL Query string
        """
        print('query1:', query)
        try:
            response = self.kg_client.execute(query)
        except Exception as ex:
            logger.error("SPARQL query not successful")
            raise KGException("SPARQL query not successful.") from ex
        return json.loads(response)


    def performUpdate(self, update):
        """
            This function performs SPARQL Update to knowledge graph.
            Arguments:
                update - SPARQL Update string
        """
        print("Update1:", update)
        try:
            self.kg_client.executeUpdate(update)
            print("Update query executed")
        except Exception as ex:
            print("Update query failed")
            logger.error("SPARQL update not successful")
            raise KGException("SPARQL update not successful.") from ex
