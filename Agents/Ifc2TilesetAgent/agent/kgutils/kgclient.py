"""
# Author: qhouyee #

This module provides functionality to execute KG query and update based on the TWA base library.
"""

# Third party imports
import json
from py4jps import agentlogging

# Self imports
from agent.exceptions.exceptions import KGException
from agent.kgutils.javagateway import jpsBaseLibGW

logger = agentlogging.get_logger('prod')


class KGClient:
    """
    A class to represent a Knowledge Graph Client.

    Attributes:
        query_endpoint: str
            SPARQL QUERY endpoint
        update_endpoint: str
            SPARQL UPDATE endpoint
        kg_user: str
            Username for the SPARQL endpoint
        kg_password: str
            Password for the SPARQL endpoint
    """

    def __init__(self, query_endpoint: str, update_endpoint: str, kg_user: str = None,
                 kg_password: str = None):
        # create a JVM module view and use it to import the required java classes
        self.twa_base_lib_view = jpsBaseLibGW.createModuleView()
        jpsBaseLibGW.importPackages(self.twa_base_lib_view, "uk.ac.cam.cares.jps.base.query.*")

        try:
            if kg_user is not None and kg_password is not None:
                self.kg_client = self.twa_base_lib_view.RemoteStoreClient(
                    query_endpoint, update_endpoint, kg_user, kg_password)
            else:
                self.kg_client = self.twa_base_lib_view.RemoteStoreClient(
                    query_endpoint, update_endpoint)
        except Exception as ex:
            logger.error("Unable to initialise KG client.")
            raise KGException("Unable to initialise KG client.") from ex

    def execute_query(self, query: str):
        """
        This function performs query to knowledge graph.
        Arguments:
            query - SPARQL Query string
        """
        try:
            response = self.kg_client.execute(query)
        except Exception as ex:
            logger.error("SPARQL query not successful.")
            raise KGException("SPARQL query not successful.") from ex
        return json.loads(response)

    def execute_update(self, update: str):
        """
        This function performs SPARQL Update to knowledge graph.
        Arguments:
            update - SPARQL Update string
        """
        try:
            self.kg_client.executeUpdate(update)
        except Exception as ex:
            logger.error("SPARQL update not successful." + str(ex))
            raise KGException("SPARQL update not successful.") from ex
