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

from dhoptimisation.datamodel.iris import *


# Initialise logger instance (ensure consistent logger level`)
logger = agentlogging.get_logger('prod')


class KGClient(PySparqlClient):
    
    #
    # SPARQL QUERIES
    #
    #TODO: Check whether that's still needed in the end
    def get_associated_dataIRI(self, instance_iri:str, unit=None, forecast=True) -> tuple:
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
        

    def get_input_forecast_details(self, forecast_iri:str):
        """
        Returns the tsIRI, rdb url and time format associated with the given 
        dataIRI, i.e., IRI of grid temperature or heat demand forecast

        Arguments:
            forecast_iri (str) -- IRI of heat demand or grid temperature forecast
                                  (i.e., inputs to the generation optimisation)
        Returns:
            ts (dict) -- dictionary with keys 'ts_iri', 'rdb_url' and 'time_format'
        """

        #TODO: Include unit of forecast, i.e. MW?
        query = f"""
            SELECT DISTINCT ?ts_iri ?fc_iri ?unit ?rdb_url ?time_format
            WHERE {{   
            <{forecast_iri}> <{TS_HASTIMESERIES}> ?ts_iri .
            ?ts_iri <{TS_HASRDB}> ?rdb_url .
            OPTIONAL {{ ?ts_iri <{TS_HASTIMEUNIT}> ?time_format . }}
            }}
        """
        query = self.remove_unnecessary_whitespace(query)
        res = self.performQuery(query)

        # Extract relevant information from unique query result
        if len(res) == 1:
            ts = {'ts_iri': self.get_unique_value(res, 'ts_iri'),
                  'rdb_url': self.get_unique_value(res, 'rdb_url'),
                  'time_format': self.get_unique_value(res, 'time_format'),
            }
            return ts
        
        else:
            # Throw exception if no or multiple time series are found
            if len(res) == 0:
                msg = f"No time series associated with data IRI: {forecast_iri}."
            else:
                msg = f"Multiple time series associated with data IRI: {forecast_iri}."
            logger.error(msg)
            raise ValueError(msg)
        

    def get_optimisation_outputs(self, forecast_iri:str):
        """
        Returns a list of all generation optimisation outputs associated with
        provided forecast IRI as input
        NOTE: Only considers ts:Forecast types as relevant outputs

        Arguments:
            forecast_iri (str) -- IRI of heat demand or grid temperature forecast
                                  (i.e., inputs to the generation optimisation)
        Returns:
            output_iris (list) -- list of instantiated optimisation derivation outputs
                                  (empty list if not exist)
        """

        #TODO: To be revisited whether that's the final way we want to do it,
        #      or rather query via OntoHeatNet instead of OntoDerivation
        query = f"""
            SELECT DISTINCT ?output_iris
            WHERE {{   
            <{forecast_iri}> ^<{ONTODERIVATION_ISDERIVEDFROM}> ?deriv_iri . 
            ?deriv_iri ^<{ONTODERIVATION_BELONGSTO}> ?output_iri . 
            ?output_iri <{RDF_TYPE}> <{TS_FORECAST}> . 
            }}
        """
        query = self.remove_unnecessary_whitespace(query)
        res = self.performQuery(query)

        # Extract relevant information from query result
        return self.get_list_of_unique_values(res, 'output_iri')
   

    def get_interval_details(self, intervalIRI:str):
        """
        Returns interval time bounds (inclusive) for given optimisation interval IRI.

        Returns:
            interval (dict) -- dictionary with keys 'start_unix' and 'end_unix'
        """

        def _get_instant_details(var_instant, var_timepos):
            query = f"""
                VALUES ?trs {{ <{UNIX_TIME}> }} 
                ?{var_instant} <{TIME_INTIMEPOSITION}>/<{TIME_HASTRS}> ?trs ;
                            <{TIME_INTIMEPOSITION}>/<{TIME_NUMERICPOSITION}> ?{var_timepos} . 
            """
            return query

        query = f"""
            SELECT DISTINCT ?start_unix ?end_unix
            WHERE {{
            <{intervalIRI}> <{TIME_HASBEGINNING}> ?start_iri ;
                            <{TIME_HASEND}> ?end_iri .
            {_get_instant_details('start_iri', 'start_unix')} 
            {_get_instant_details('end_iri', 'end_unix')}        
            }}
        """
        query = self.remove_unnecessary_whitespace(query)
        res = self.performQuery(query)

        # Extract relevant information from unique query result
        if len(res) == 1:
            interval = {'start_unix': self.get_unique_value(res, 'start_unix', int),
                        'end_unix': self.get_unique_value(res, 'end_unix', int),
            }
            # Check validity of retrieved interval
            if interval['start_unix'] >= interval['end_unix']:
                msg = "Interval start time is not before end time."
                logger.error(msg)
                raise ValueError(msg)

            return interval
        
        else:
            msg = "No unique interval details could be retrieved from KG."
            logger.error(msg)
            raise ValueError(msg)
        

    def get_heat_providers(self):
        """
        Query IRIs for heat providers, i.e., waste incineration plant 
        as well as heat boilers and gas turbine

        Returns:
            providers (dict) -- dictionary with keys 'efw_plant', 
                               'boilers' and 'gt'
        """

        query = f"""
            SELECT DISTINCT ?efw_plant ?boiler ?gt
            WHERE {{
            ?efw_plant <{RDF_TYPE}> <{OHN_INCINERATIONPLANT}> .
            ?boiler <{RDF_TYPE}> <{OHN_HEATBOILER}> .
            ?gt <{RDF_TYPE}> <{OHN_GASTURBINE}> .
            }}
        """
        query = self.remove_unnecessary_whitespace(query)
        res = self.performQuery(query)

        # Extract relevant information from unique query result
        providers = {'efw_plant': self.get_list_of_unique_values(res, 'efw_plant'),
                     'boilers': self.get_list_of_unique_values(res, 'boiler'),
                     'gt': self.get_list_of_unique_values(res, 'gt'),
        }
        return providers
    

    def get_efw_output_iris(self, efw_plant:str):
        """
        Query IRIs for waste incineration plant optimisation output concepts,
        i.e., provided heat amount and availability

        Arguments:
            efw_plant (str) -- IRI of waste incineration plant instance

        Returns:
            outputs (dict) -- dictionary with keys 'heat', 'availability'
        """

        query = f"""
            SELECT DISTINCT ?heat ?availability
            WHERE {{
            <{efw_plant}> <{OHN_HAS_PROVIDED_HEAT_AMOUNT}> ?heat ;
                          <{OHN_HAS_OPERATING_AVAILABILITY}> ?availability .
            ?heat <{RDF_TYPE}> <{OHN_PROVIDED_HEAT_AMOUNT}> .
            ?availability <{RDF_TYPE}> <{OHN_AVAILABILITY}> .
            }}
        """
        query = self.remove_unnecessary_whitespace(query)
        res = self.performQuery(query)

        # Extract relevant information from unique query result
        outputs = {'heat': self.get_unique_value(res, 'heat'),
                   'availability': self.get_unique_value(res, 'availability')
        }
        return outputs
    

    def get_heatgenerator_output_iris(self, heat_generator:str, gt:bool = False):
        """
        Query IRIs for heat generator (gas turbine, gas boiler) output concepts,
        i.e., consumed gas amount, generated heat amount, co-generated electricity,
        availability

        Arguments:
            heat_generator (str) -- IRI of heat boiler or gas trubine instance
            gt (bool) -- boolean flag whether heat generator is conventional 
                         gas boiler or CHP gas turbine

        Returns:
            outputs (dict) -- dictionary with keys 'gas', 'heat', 'electricity',
                              'availability', ...
        """

        query = f"""
            SELECT DISTINCT ?gas ?heat ?electricity ?availability
            WHERE {{
            <{heat_generator}> <{OHN_HAS_CONSUMED_GAS_AMOUNT}> ?gas ;
                               <{OHN_HAS_GENERATED_HEAT_AMOUNT}> ?heat ;
                               <{OHN_HAS_OPERATING_AVAILABILITY}> ?availability .
            ?gas <{RDF_TYPE}> <{OHN_CONSUMED_GAS_AMOUNT}> .
            ?heat <{RDF_TYPE}> <{OHN_GENERATED_HEAT_AMOUNT}> .
            ?availability <{RDF_TYPE}> <{OHN_AVAILABILITY}> .
            OPTIONAL {{
            <{heat_generator}> <{OHN_HAS_COGEN_ELECTRICITY_AMOUNT}> ?electricity . 
            ?electricity <{RDF_TYPE}> <{OHN_COGEN_ELECTRICITY_AMOUNT}> .
            }}
            }}
        """
        query = self.remove_unnecessary_whitespace(query)
        res = self.performQuery(query)

        # Extract relevant information from unique query result
        outputs = {'gas': self.get_unique_value(res, 'gas'),
                   'heat': self.get_unique_value(res, 'heat'),
                   'availability': self.get_unique_value(res, 'availability'),
                   'electricity': None
        }
        if gt:
            outputs['electricity'] = self.get_unique_value(res, 'electricity')
        return outputs


    #
    # SPARQL UPDATES
    # 
    def instantiate_new_outputs(self, g: Graph, outputs: dict):
        """
        Instantiate forecast relationships for all generation optimisation outputs
        NOTE: All optimisation outputs are instantiated as forecasts to 
              indicate "hypothetical" nature of values and to not interfere
              with actual historical measurements
        
        Returns:
            outputs (dict) -- dictionary with keys 'heat', 'gas', 'electricity',
                              'availability', ...
                              and associated time series IRIs as values
        """

        #TODO: add proper instantiation of units
        #      (incl. prior query) where relevant
        for output in outputs:
            # Suppres instantiation of non applicable instances (i.e., co-gen 
            # electricity for conventional heat boilers)
            if outputs[output] is not None:
                fc_iri = KB + 'Forecast_' + str(uuid.uuid4())
                g.add((URIRef(outputs[output]), URIRef(TS_HASFORECAST), URIRef(fc_iri)))
                g.add((URIRef(fc_iri), URIRef(RDF_TYPE), URIRef(TS_FORECAST)))
                g.add((URIRef(fc_iri), URIRef(OM_HASUNIT), URIRef(OM_MEGAWATTHOUR)))
                outputs[output] = fc_iri

        return g, outputs
    

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
