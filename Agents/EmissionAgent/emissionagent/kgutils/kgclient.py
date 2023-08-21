################################################
# Authors: Markus Hofmeister (mh807@cam.ac.uk) #    
# Date: 21 Aug 2023                            #
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
    def get_associated_dataIRI(self, instance_iri:str, unit=None, forecast=False) -> tuple:
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

        # Constrain unit value if given
        unit_constrain = ''
        if unit:
            unit_constrain = f'VALUES ?unit {{ <{unit}> }} '

        # Specify relationship between instance and dataIRI
        if forecast:
            relationship = TS_HASFORECAST
        else:
            relationship = OM_HASVALUE

        query = f"""
            SELECT ?dataIRI ?unit
            WHERE {{
            {unit_constrain}
            <{instance_iri}> <{relationship}> ?dataIRI .
            ?dataIRI <{OM_HASUNIT}> ?unit .
            }}
        """
        query = remove_unnecessary_whitespace(query)
        res = self.performQuery(query)

        # Extract and return results
        if len(res) == 1:
            return get_unique_value(res, 'dataIRI'),  \
                   get_unique_value(res, 'unit')

        else:
            # Throw exception if no or multiple dataIRIs (with units) are found
            if len(res) == 0:
                msg = f'No "dataIRI" associated with given instance: {instance_iri}.'
            else:
                msg = f'Multiple "dataIRI"s associated with given instance: {instance_iri}.'
            logger.error(msg)
            raise ValueError(msg) 


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
