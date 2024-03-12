################################################
# Authors: Jiying Chen (jc2341)        #    
# Date: 11/03 2024                             #
################################################

# This module is used to to provide functionality to execute KG queries and updates 
# using the RemoteStoreClient from the JPS_BASE_LIB

import json
from py4jps import agentlogging
from agent.errorhandling.exceptions import KGException
from agent.datainstantiation.jpsSingletons import jpsBaseLibGW

# Initialise logger
logger = agentlogging.get_logger("prod")

class KGClient:
    
    def __init__(self, query_endpoint, update_endpoint, kg_user=None, 
                 kg_password=None):

        # create a JVM module view and use it to import the required java classes
        self.jpsBaseLib_view = jpsBaseLibGW.createModuleView()
        jpsBaseLibGW.importPackages(self.jpsBaseLib_view,"uk.ac.cam.cares.jps.base.query.*")

        # replace RemoteStoreClient with AccessAgent/StoreClient once its tested
        # StoreRouter = jpsBaseLib_view.StoreRouter
        # # Get StoreClientInterface Java object to sparqlEndPoint with
        # # query operations enabled (True) and update operations disabled (False)
        # StoreClient = StoreRouter.getStoreClient(sparqlEndPoint, True, False)

        try:
            if kg_user is not None:
                self.kg_client = self.jpsBaseLib_view.RemoteStoreClient(query_endpoint, update_endpoint, kg_user, kg_password)
            else:
                self.kg_client = self.jpsBaseLib_view.RemoteStoreClient(query_endpoint, update_endpoint)
        except Exception as ex:
            logger.error("Unable to initialise KG client")
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
            logger.error("SPARQL query not successful")
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
            logger.error("SPARQL update not successful")
            raise KGException("SPARQL update not successful.") from ex
    
    def get_time_series_details(self, iri_to_forecast:str):
        query = f"""
            SELECT DISTINCT ?data_iri ?ts_iri ?fc_iri ?unit ?rdb_url ?time_format
            WHERE {{   
            VALUES ?iri {{ <{iri_to_forecast}> }} 
            ?iri <http://www.w3.org/2006/time#hasTime>*/<http://example.org/ts#hasTimeSeries> ?ts_iri .
            ?ts_iri ^<http://example.org/ts#hasTimeSeries> ?data_iri ;
                     <http://example.org/ts#hasRDB> ?rdb_url .
            OPTIONAL {{ ?data_iri <http://www.w3.org/2006/time#unitType> ?unit . }}
            OPTIONAL {{ ?ts_iri <http://example.org/ts#hasTimeUnit> ?time_format . }}
            OPTIONAL {{ ?iri <http://example.org/ts#hasForecast> ?fc_iri . }}
            }}
        """
        try:
            response = self.performQuery(query)
            if response and len(response) == 1:
                return response[0]  # Simplified for illustration; adapt as needed
            else:
                raise ValueError("No unique time series details found.")
        except Exception as ex:
            logger.error("Error getting time series details: " + str(ex))
            raise
