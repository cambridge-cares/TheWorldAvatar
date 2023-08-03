################################################
# Authors: Markus Hofmeister (mh807@cam.ac.uk) #    
# Date: 25 Jul 2023                            #
################################################

# The purpose of this module is to provide functionality to execute
# KG queries and updates using the StoreRouter from the JPS_BASE_LIB

from distutils.util import strtobool

from py4jps import agentlogging

from pyderivationagent.kg_operations import PySparqlClient

from forecastingagent.datamodel.iris import *


# Initialise logger instance (ensure consistent logger level`)
logger = agentlogging.get_logger('prod')


class KGClient(PySparqlClient):
    
    #
    # SPARQL QUERIES
    #
    def get_time_series_details(self, iri_to_forecast:str):
        """
        Returns the dataIRI, tsIRI, RDB URL and time format of the time series 
        instance associated with the given instance IRI.
        NOTE: iri_to_forecast and dataIRI do not need to be equivalent, especially
              when OM representation for data is used, where a om:Measure concept 
              is used "in between":
              <iri_to_forecast> ts:hasForecast <...> ;
                                om:hasValue <dataIRI> .
              <dataIRI> a om:Measure ;
                        ts:hasTimeseries <tsIRI> .
              <tsIRI> ts:hasRDB <...> ;
                      ts:hasTimeUnit <...> .

        Arguments:
            iri_to_forecast (str) -- IRI of instance for which to create forecast
        Returns:
            ts (dict) -- dictionary with keys 'data_iri', 'ts_iri', 'fc_iri',
                         'rdb_url' and 'time_format'
        """
        query = f"""
            SELECT DISTINCT ?data_iri ?ts_iri ?fc_iri ?unit ?rdb_url ?time_format
            WHERE {{   
            VALUES ?iri {{ <{iri_to_forecast}> }} 
            ?iri <{OM_HASVALUE}>*/<{TS_HASTIMESERIES}> ?ts_iri .
            ?ts_iri ^<{TS_HASTIMESERIES}> ?data_iri ;
                     <{TS_HASRDB}> ?rdb_url .
            OPTIONAL {{ ?data_iri <{OM_HASUNIT}> ?unit . }}
            OPTIONAL {{ ?ts_iri <{TS_HASTIMEUNIT}> ?time_format . }}
            OPTIONAL {{ ?iri <{TS_HASFORECAST}> ?fc_iri . }}
            }}
        """
        query = self.remove_unnecessary_whitespace(query)
        res = self.performQuery(query)

        # Extract relevant information from unique query result
        if len(res) == 1:
            ts = {'data_iri': self.get_unique_value(res, 'data_iri'),
                  'ts_iri': self.get_unique_value(res, 'ts_iri'),
                  'fc_iri': self.get_unique_value(res, 'fc_iri'),
                  'unit': self.get_unique_value(res, 'unit'),
                  'rdb_url': self.get_unique_value(res, 'rdb_url'),
                  'time_format': self.get_unique_value(res, 'time_format')
            }
            return ts
        
        else:
            # Throw exception if no or multiple time series are found
            if len(res) == 0:
                msg = f"No time series associated with data IRI: {iri_to_forecast}."
            else:
                msg = f"Multiple time series associated with data IRI: {iri_to_forecast}."
            logger.error(msg)
            raise ValueError(msg)
        

    def get_fcmodel_details(self, fcmodelIRI:str):
        """
        Returns relevant forecasting model details for given forecasting model IRI.

        Returns:
            fcmodel (dict) -- dictionary with keys 'fcmodel_iri', 'label', 'scale_data',
                              'model_url', 'chkpt_url' and 'covariate_iris'
        """
        query = f"""
            SELECT DISTINCT ?fcmodel_iri ?label ?scale_data ?model_url ?chkpt_url ?covariate_iri
            WHERE {{   
            VALUES ?fcmodel_iri {{ <{fcmodelIRI}> }} 
            ?fcmodel_iri <{RDFS_LABEL}> ?label .
            OPTIONAL {{ ?fcmodel_iri <{TS_SCALE_DATA}> ?scale_data . }}
            OPTIONAL {{ ?fcmodel_iri <{TS_HAS_MODEL_URL}> ?model_url ;
                                     <{TS_HAS_CHKPT_URL}> ?chkpt_url . }}
            OPTIONAL {{ ?fcmodel_iri <{TS_HASCOVARIATE}> ?covariate_iri . }}
            }}
        """
        query = self.remove_unnecessary_whitespace(query)
        res = self.performQuery(query)

        # Extract relevant information from unique query result
        if len(res) >= 1:
            fcmodel = {'fcmodel_iri': self.get_unique_value(res, 'fcmodel_iri'),
                       'label': self.get_unique_value(res, 'label'),
                       'scale_data': self.get_unique_value(res, 'scale_data', bool),
                       'model_url': self.get_unique_value(res, 'model_url'),
                       'chkpt_url': self.get_unique_value(res, 'chkpt_url'),
                       #TODO: covariates should become dictionary with type and IRI
                       'covariate_iris': self.get_list_of_unique_values(res, 'covariate_iri')
            }
            return fcmodel
        
        else:
            msg = "No forecasting model details could be retrieved from KG."
            logger.error(msg)
            raise ValueError(msg)
    

    def get_duration_details(self, durationIRI:str):
        """
        Returns relevant duration details (i.e., time value and unit) for given 
        duration or frequency (i.e., a subclass of time:duration) IRI.

        Returns:
            duration (dict) -- dictionary with keys 'iri', 'unit', 'value', 
                               and 'resample_data' (only relevant for frequency)
        """
        query = f"""
            SELECT DISTINCT ?iri ?resample_data ?unit ?value
            WHERE {{   
            VALUES ?iri {{ <{durationIRI}> }} 
            ?iri <{TIME_UNIT_TYPE}> ?unit ;
                 <{TIME_NUMERICDURATION}> ?value .
            OPTIONAL {{ ?iri <{TS_RESAMPLE_DATA}> ?resample_data . }}
            }}
        """
        query = self.remove_unnecessary_whitespace(query)
        res = self.performQuery(query)

        # Extract relevant information from unique query result
        if len(res) == 1:
            duration = {'iri': self.get_unique_value(res, 'iri'),
                        'unit': self.get_unique_value(res, 'unit'),
                        'value': self.get_unique_value(res, 'value', float),
                        'resample_data': self.get_unique_value(res, 'resample_data', bool)
            }
            return duration
        
        else:
            msg = "No unique duration/frequency details could be retrieved from KG."
            logger.error(msg)
            raise ValueError(msg)
        

    def get_interval_details(self, intervalIRI:str):
        """
        Returns relevant time interval (i.e. forecast horizon) details for 
        given time interval IRI.

        Returns:
            interval (dict) -- dictionary with keys 'interval_iri', 'start_iri',
                               'end_iri', 'start_unix' and 'end_unix'
        """

        def _get_instant_details(var_instant, var_timepos):
            query = f"""
                VALUES ?trs {{ <{UNIX_TIME}> }} 
                ?{var_instant} <{TIME_INTIMEPOSITION}>/<{TIME_HASTRS}> ?trs ;
                            <{TIME_INTIMEPOSITION}>/<{TIME_NUMERICPOSITION}> ?{var_timepos} . 
            """
            return query

        query = f"""
            SELECT DISTINCT ?interval_iri ?start_iri ?end_iri ?start_unix ?end_unix
            WHERE {{
            VALUES ?interval_iri {{ <{intervalIRI}> }} 
            ?interval_iri <{TIME_HASBEGINNING}> ?start_iri ;
                          <{TIME_HASEND}> ?end_iri .
            {_get_instant_details('start_iri', 'start_unix')} 
            {_get_instant_details('end_iri', 'end_unix')}        
            }}
        """
        query = self.remove_unnecessary_whitespace(query)
        res = self.performQuery(query)

        # Extract relevant information from unique query result
        if len(res) == 1:
            interval = {'interval_iri': self.get_unique_value(res, 'interval_iri'),
                        'start_iri': self.get_unique_value(res, 'start_iri'),
                        'end_iri': self.get_unique_value(res, 'end_iri'),
                        'start_unix': self.get_unique_value(res, 'start_unix', int),
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
        

    # def get_associated_forecast_details(self, dataIRI:str):
    #     """
    #     Returns the IRI, RDB URL ans time format of the time series instance 
    #     associated with the given data IRI.

    #     Returns:
    #         ts (dict) -- dictionary with keys 'ts_iri', 'rdb_url' and 'time_format'
    #     """
    #     query = f"""
    #         SELECT DISTINCT ?ts_iri ?rdb_url ?time_format
    #         WHERE {{   
    #         VALUES ?data_iri {{ <{dataIRI}> }} 
    #         ?data_iri <{OM_HASVALUE}>*/<{TS_HASTIMESERIES}> ?ts_iri .
    #         ?ts_iri <{TS_HASRDB}> ?rdb_url .
    #         OPTIONAL {{ ?ts_iri <{TS_HASTIMEUNIT}> ?time_format . }}
    #         }}
    #     """
    #     query = self.remove_unnecessary_whitespace(query)
    #     res = self.performQuery(query)

    #     # Extract relevant information from unique query result
    #     if len(res) == 1:
    #         ts = {'ts_iri': self.get_unique_value(res, 'ts_iri'),
    #               'rdb_url': self.get_unique_value(res, 'rdb_url'),
    #               'time_format': self.get_unique_value(res, 'time_format')
    #         }
    #         return ts
        
    #     else:
    #         # Throw exception if no or multiple time series are found
    #         if len(res) == 0:
    #             msg = f"No time series associated with data IRI: {dataIRI}."
    #         else:
    #             msg = f"Multiple time series associated with data IRI: {dataIRI}."
    #         logger.error(msg)
    #         raise ValueError(msg)


    def get_dataIRI(self, tsIRI:str):
        """
        Returns dataIRI associated with given tsIRI.

        Returns:
            dataIRI {str} -- dataIRI associated with given tsIRI
        """
        query = f"""
            SELECT DISTINCT ?dataIRI
            WHERE {{   
            ?dataIRI <{TS_HASTIMESERIES}> <{tsIRI}> .
            }}
        """
        query = self.remove_unnecessary_whitespace(query)
        res = self.performQuery(query)

        # Return unique query result (otherwise exception is thrown)
        return self.get_unique_value(res, 'dataIRI')


    def get_all_tsIRIs(self):
        """
        Returns list of all instantiated time series IRIs.
        """
        query = f"""
            SELECT DISTINCT ?tsIRI
            WHERE {{   
            ?tsIRI <{RDF_TYPE}> <{TS_TIMESERIES}> .
            }}
        """
        query = self.remove_unnecessary_whitespace(query)
        res = self.performQuery(query)

        # Return unique query result (otherwise exception is thrown)
        return self.get_list_of_unique_values(res, 'tsIRI')


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
    
    def get_unique_value(self, res: list, key: str, cast_to=None) -> str:
        # Unpacks a query result list (i.e., list of dicts) into unique 
        # value for the given key (returns None if no unique value is found)
        
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
