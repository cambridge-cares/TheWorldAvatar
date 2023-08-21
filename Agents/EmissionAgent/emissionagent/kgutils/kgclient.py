################################################
# Authors: Markus Hofmeister (mh807@cam.ac.uk) #    
# Date: 25 Jul 2023                            #
################################################

# The purpose of this module is to provide functionality to execute
# KG queries and updates using the PySparqlClient from the DerivationAgent

import uuid
from rdflib import URIRef, Literal, Graph

from py4jps import agentlogging

from pyderivationagent.kg_operations import PySparqlClient

from emissionagent.datamodel.iris import *
from emissionagent.kgutils.utils import *


# Initialise logger instance (ensure consistent logger level`)
logger = agentlogging.get_logger('prod')


class KGClient(PySparqlClient):
    
    #
    # SPARQL QUERIES
    #
    def get_associated_dataIRI(instance_iri:str, unit=None, forecast=False) -> str:
        """
        Retrieves the dataIRI (i.e., IRI with attached time series) associated
        with a given instance IRI (e.g., consumed gas amount IRI)

        Arguments:
            instance_iri {str} -- IRI of instance for which to retrieve dataIRI
            unit {str} -- target unit associated with dataIRI
                          If given, only dataIRIs with matching unit are returned
                          Otherwise, dataIRIs with any unit are returned
            forecast {bool} -- whether to retrieve dataIRI for actual (om:Measure)
                               or forecast data (default: actual data)
        Returns:
            dataIRI {str} -- IRI of associated dataIRI
        """
        pass


    #
    # SPARQL UPDATES
    # 
    def instantiate_emissions(self, emissions:list) -> Graph:
        """
        Takes a list of dictionaries with emissions data and creates 
        new emission instances
        NOTE: All values MUST be given in SI units (for Aermod to properly pick
              them up), i.e.,   temperature - K, 
                                density - kg/m3, 
                                mass flow rate - kg/s

        Arguments:
            emissions {list} -- emission data dictionaries with the following keys:
                                'pollutantID', 'temperature', 'density', 'massflow'
        Returns:
            graph {Graph} -- Graph of updated forecast instance
        """
        
        def _add_emission_instance(graph: Graph, emission:dict) -> Graph:
            # Add triples for single emission instance
            
            return graph


        # Create Graph of derivation output triples
        graph = Graph()

        # Add triples for each emission instance
        for e in emissions:
            graph = _add_emission_instance(graph, e)

        return graph
