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


    #
    # SPARQL UPDATES
    # 
    
    

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
