################################################
# Authors: Markus Hofmeister (mh807@cam.ac.uk) #    
# Date: 25 Jul 2023                            #
################################################

# The purpose of this module is to provide functionality to execute
# KG queries and updates using the StoreRouter from the JPS_BASE_LIB

from py4jps import agentlogging

from pyderivationagent.kg_operations import PySparqlClient

from forecastingagent.datamodel.iris import *


# Initialise logger instance (ensure consistent logger level`)
logger = agentlogging.get_logger('prod')


class KGClient(PySparqlClient):
    
    #
    # SPARQL QUERIES
    #
    def get_time_series_details(self, dataIRI:str):
        """
        Returns the IRI, RDB URL ans time format of the time series instance 
        associated with the given data IRI.

        Returns:
            ts (dict) -- dictionary with keys 'ts_iri', 'rdb_url' and 'time_format'
        """
        query = f"""
            SELECT DISTINCT ?ts_iri ?rdb_url ?time_format
            WHERE {{   
            VALUES ?data_iri {{ <{dataIRI}> }} 
            ?data_iri <{TS_HASTIMESERIES}> ?ts_iri .
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
                  'time_format': self.get_unique_value(res, 'time_format')
            }
            return ts
        
        else:
            # Throw exception if no or multiple time series are found
            if len(res) == 0:
                msg = f"No time series associated with data IRI: {dataIRI}."
            else:
                msg = f"Multiple time series associated with data IRI: {dataIRI}."
            logger.error(msg)
            raise ValueError(msg)
        

    def get_fcmodel_details(self, fcmodelIRI:str):
        """
        Returns relevant forecasting model details for given forecasting model IRI.

        Returns:
            fcmodel (dict) -- dictionary with keys 'ts_iri', 'rdb_url' and 'time_format'
        """
        query = f"""
            SELECT ?fcmodel_iri ?label ?scale_data ?model_url ?chkpt_url ?covariate_iri
            WHERE {{   
            VALUES ?fcmodel_iri {{ {fcmodelIRI} }} 
            ?fcmodel_iri <{RDFS_LABEL}> ?label .
            OPTIONAL {{ ?fcmodel_iri <{TS_SCALE_DATA}> ?scale_data . }}
            OPTIONAL {{ ?fcmodel_iri <{TS_HAS_MODEL_URL}> ?model_url . }}
            OPTIONAL {{ ?fcmodel_iri <{TS_HAS_CHKPT_URL}> ?chkpt_url . }}
            OPTIONAL {{ ?fcmodel_iri <{TS_HASCOVARIATE}> ?covariate_iri . }}
            }}
        """
        query = self.remove_unnecessary_whitespace(query)
        res = self.performQuery(query)

        # Extract relevant information from unique query result
        if len(res) >= 1:
            fcmodel = {'fcmodel_iri': self.get_unique_value(res, 'fcmodel_iri'),
                       'label': self.get_unique_value(res, 'label'),
                       'scale_data': self.get_unique_value(res, 'scale_data'),
                       'model_url': self.get_unique_value(res, 'model_url'),
                       'chkpt_url': self.get_unique_value(res, 'chkpt_url'),
                       'covariate_iri': self.get_list_of_unique_values(res, 'covariate_iri')
            }
            return fcmodel
        
        else:
            msg = "No forecasting model could be retrieved."
            logger.error(msg)
            raise ValueError(msg)
        

        


    #
    # SPARQL UPDATES
    # 


    #
    # Helper functions
    #
    def remove_unnecessary_whitespace(self, query: str) -> str:
        # Remove unnecessary whitespaces
        query = ' '.join(query.split())
        return query
    
    def get_list_of_unique_values(self, res: list, key: str) -> list:
        # Unpacks a query result list (i.e., list of dicts) into a list of 
        # unique values for the given key.
        
        res_list =  list(set([r.get(key) for r in res]))
        res_list = [v for v in res_list if v is not None]
        return res_list
    
    def get_unique_value(self, res: list, key: str) -> str:
        # Unpacks a query result list (i.e., list of dicts) into unique 
        # value for the given key (returns None if no unique value is found)
        
        res_list =  self.get_list_of_unique_values(res, key)
        if len(res_list) == 1:
            return res_list[0]
        else:
            if len(res_list) == 0:
                msg = f"No unique value found for key: {key}."
            else:
                msg = f"Multiple unique values found for key: {key}."
            logger.warning(msg)
            return None
