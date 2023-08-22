################################################
# Authors: Markus Hofmeister (mh807@cam.ac.uk) #    
# Date: 21 Aug 2023                            #
################################################

# The purpose of this module is to provide functionality to execute
# KG queries and updates using the PySparqlClient from the DerivationAgent

import uuid
from distutils.util import strtobool
from rdflib import URIRef, Literal, Graph
from rdflib.namespace import XSD

from py4jps import agentlogging

from pyderivationagent.kg_operations import PySparqlClient

from emissionagent.datamodel.iris import *


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
            unit {str} -- unit of associated dataIRI
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
        query = self.remove_unnecessary_whitespace(query)
        res = self.performQuery(query)

        # Extract and return results
        if len(res) == 1:
            return self.get_unique_value(res, 'dataIRI'),  \
                   self.get_unique_value(res, 'unit')

        else:
            # Throw exception if no or multiple dataIRIs (with units) are found
            if len(res) == 0:
                msg = f'No "dataIRI" associated with given instance: {instance_iri}.'
            else:
                msg = f'Multiple "dataIRI"s associated with given instance: {instance_iri}.'
            logger.error(msg)
            raise ValueError(msg)
        

    def get_emission_details(self, emission_iris:list=None) -> dict:
        """
        Retrieves details for (a given list of) emission instances 
        (i.e., pollutantID, temperature, density, and massflow)

        Arguments:
            emission_iris {list} -- list of emission IRIs for which to retrieve 
                                    details (optional); otherwise, details for all
                                    emission instances are retrieved
        Returns:
            dictionary with pollutantIDs as top-level keys and nested dicts as 
            values with keys: temperature, density, and massflow
        """
        
        # Constrain emission IRIs if given
        emission_values = ''
        if emission_iris:
            emission_values = f'VALUES ?emission_iri {{ <{"> <".join(emission_iris)}> }} '

        query = f"""
            SELECT ?emission_iri ?pollutantID ?temperature ?density ?massflow
            WHERE {{
            {emission_values}
            ?emission_iri <{RDF_TYPE}> <{OD_EMISSION}> ;
                          <{OD_HAS_POLLUTANT_ID}> ?pollutantID ;
                          <{OM_HAS_QUANTITY}> ?quantity . 
            OPTIONAL {{ ?quantity <{RDF_TYPE}> <{OM_TEMPERATURE}> ;
                                  <{OM_HASVALUE}>/<{OM_HAS_NUMERICAL_VALUE}> ?temperature }}
            OPTIONAL {{ ?quantity <{RDF_TYPE}> <{OM_DENSITY}> ;
                                  <{OM_HASVALUE}>/<{OM_HAS_NUMERICAL_VALUE}> ?density }}
            OPTIONAL {{ ?quantity <{RDF_TYPE}> <{OM_MASSFLOW}> ;
                                  <{OM_HASVALUE}>/<{OM_HAS_NUMERICAL_VALUE}> ?massflow }}
            }}
        """
        query = self.remove_unnecessary_whitespace(query)
        res = self.performQuery(query)

        # Structure results in dictionary with pollutantIDs as top-level keys
        # and temperature, density, and massflow as sub-keys
        res_consolidated = {}
        for e in emission_iris:
            vals = [r for r in res if r.get('emission_iri') == e]
            res_consolidated[self.get_unique_value(vals, 'pollutantID')] = \
            {
                'temperature': self.get_unique_value(vals, 'temperature', float),
                'density': self.get_unique_value(vals, 'density', float),
                'massflow': self.get_unique_value(vals, 'massflow', float)
            }
        
        return res_consolidated


    #
    # SPARQL UPDATES
    # 
    def instantiate_emissions(self, location_iri:str, emissions:list) -> Graph:
        """
        Takes a list of dictionaries with emissions data and creates 
        new emission instances
        NOTE: All values MUST be given in SI units (for Aermod to properly pick
              them up), i.e.,   temperature - K, 
                                density - kg/m3, 
                                mass flow rate - kg/s

        Arguments:
            location_iri {str} -- IRI of StaticPointSource where emissions are located
            emissions {list} -- emission data dictionaries with the following keys:
                                'pollutantID', 'temperature', 'density', 'massflow'
        Returns:
            graph {Graph} -- Graph of updated forecast instance
        """

        def _instantiate_om_quantity(g:Graph, emission_iri:str, quantity:str, 
                                     value=float) -> Graph:
            # Create new OM:Quantity and OM:Measure instances
            if quantity == 'temperature':
                quantity_iri = KB + 'Temperature_' + str(uuid.uuid4())
                quantity_type = OM_TEMPERATURE
                unit_iri = OM_KELVIN
            elif quantity == 'density':
                quantity_iri = KB + 'Density_' + str(uuid.uuid4())
                quantity_type = OM_DENSITY
                unit_iri = OM_KG_PER_M3
            elif quantity == 'massflow':
                quantity_iri = KB + 'MassFlow_' + str(uuid.uuid4())
                quantity_type = OM_MASSFLOW
                unit_iri = OM_KG_PER_S
            measure_iri = KB + 'Measure_' + str(uuid.uuid4())
            
            # Add triples to graph
            g.add((URIRef(emission_iri), URIRef(OM_HAS_QUANTITY), URIRef(quantity_iri)))
            g.add((URIRef(quantity_iri), URIRef(RDF_TYPE), URIRef(quantity_type)))
            g.add((URIRef(quantity_iri), URIRef(OM_HASVALUE), URIRef(measure_iri)))
            g.add((URIRef(measure_iri), URIRef(RDF_TYPE), URIRef(OM_MEASURE)))
            g.add((URIRef(measure_iri), URIRef(OM_HAS_NUMERICAL_VALUE), Literal(value, datatype=XSD.float)))
            g.add((URIRef(measure_iri), URIRef(OM_HASUNIT), URIRef(unit_iri)))

            return g
        
        def _add_emission_instance(g:Graph, location:str, emission:dict) -> Graph:
            # Create new emission instance
            emission_iri = KB + 'Emission_' + str(uuid.uuid4())
            # Add triples for single emission instance
            g.add((URIRef(location), URIRef(OD_EMITS), URIRef(emission_iri)))
            g.add((URIRef(emission_iri), URIRef(RDF_TYPE), URIRef(OD_EMISSION)))
            g.add((URIRef(emission_iri), URIRef(OD_HAS_POLLUTANT_ID),
                   URIRef(emission.pop('pollutantID'))))
            # Create OM:Quantity instances for temperature, density, and mass flow rate
            for k, v in emission.items():
                g = _instantiate_om_quantity(g, emission_iri=emission_iri,
                                             quantity=k, value=v)
            
            return graph


        # Create Graph of derivation output triples
        graph = Graph()

        # Add triples for each emission instance
        for e in emissions:
            graph = _add_emission_instance(graph, location_iri, e)

        return graph
    

    #
    # HELPER FUNCTIONS
    #
    def remove_unnecessary_whitespace(self, query: str) -> str:
        """
        Remove unnecessary whitespaces
        """
        query = ' '.join(query.split())
        return query


    def get_list_of_unique_values(self, res: list, key: str) -> list:
        """
        Unpacks a query result list (i.e., list of dicts) into a list of 
        unique values for the given dict key.
        """    
        res_list =  list(set([r.get(key) for r in res]))
        res_list = [v for v in res_list if v is not None]
        return res_list


    def get_unique_value(self, res: list, key: str, cast_to=None) -> str:
        """
        Unpacks a query result list (i.e., list of dicts) into unique 
        value for the given dict key (returns None if no unique value is found)

        Tries to cast result in case 'cast_to' datatype is provided
        """
        
        res_list =  self.get_list_of_unique_values(res, key)
        if len(res_list) == 1:
            # Try to cast retrieved value to target type (throws error if not possible)
            if cast_to and issubclass(cast_to, bool):
                res_list[0] = bool(strtobool(res_list[0]))
            elif cast_to and issubclass(cast_to, (int, float)):
                res_list[0] = cast_to(res_list[0])
            return res_list[0]
        else:
            if len(res_list) == 0:
                msg = f"No value found for key: {key}."
            else:
                msg = f"Multiple values found for key: {key}."
            logger.warning(msg)
            return None
