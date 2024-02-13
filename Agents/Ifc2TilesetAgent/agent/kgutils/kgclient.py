"""
# Author: qhouyee #

This module provides functionality to execute KG query and update based on the TWA base library.
"""

# Third party imports
import json
from typing import Optional

from py4jps import agentlogging

# Self imports
from agent.exceptions.exceptions import KGException
from agent.kgutils.javagateway import jpsBaseLibGW

logger = agentlogging.get_logger('prod')


class KGClient:
    """A class that represents a Knowledge Graph Client.

    Attributes:
        twa_base_lib_view: A JVM view object.
        kg_client: A remote KG client.
    """

    def __init__(self, query_endpoint: str, update_endpoint: str, kg_user: Optional[str] = None,
                 kg_password: Optional[str] = None):
        # create a JVM module view and use it to import the required java classes
        self.twa_base_lib_view = jpsBaseLibGW.createModuleView()
        jpsBaseLibGW.importPackages(self.twa_base_lib_view, "uk.ac.cam.cares.jps.base.query.*")

        try:
            if kg_user is not None and kg_password is not None:
                self.kg_client = self.twa_base_lib_view.RemoteStoreClient(query_endpoint, update_endpoint, kg_user,
                                                                          kg_password)
            else:
                self.kg_client = self.twa_base_lib_view.RemoteStoreClient(query_endpoint, update_endpoint)
        except Exception as ex:
            logger.error("Unable to initialise KG client.")
            raise KGException("Unable to initialise KG client.") from ex

    def execute_query(self, query: str):
        """Executes a query to the knowledge graph.
        """
        try:
            return json.loads(self.kg_client.execute(query))
        except Exception as ex:
            logger.error("SPARQL query not successful.")
            raise KGException("SPARQL query not successful.") from ex

    def execute_update(self, update: str):
        """Executes a SPARQL Update to the knowledge graph.
        """
        try:
            self.kg_client.executeUpdate(update)
        except Exception as ex:
            logger.error("SPARQL update not successful." + str(ex))
            raise KGException("SPARQL update not successful.") from ex
