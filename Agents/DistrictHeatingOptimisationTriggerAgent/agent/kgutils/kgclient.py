################################################
# Authors: Markus Hofmeister (mh807@cam.ac.uk) #    
# Date: 12 Jul 2022                            #
################################################

# The purpose of this module is to provide functionality to execute
# KG queries and updates using the StoreRouter from the JPS_BASE_LIB

import uuid
from pathlib import Path
from rdflib import Graph

from py4jps import agentlogging
from pyderivationagent.kg_operations import PySparqlClient

from agent.datamodel.iris import *

# Initialise logger instance (ensure consistent logger level`)
logger = agentlogging.get_logger('prod')


class KGClient(PySparqlClient):
    #
    # SPARQL QUERIES
    #
    def get_heat_demand_covariates(self):
        """
        Returns IRIs of ambient air temperature and public holiday time series
        (used as covariates for heat demand forecasting)
        NOTE: this assumes that there is only one instance of each type in the KG
              (which is the case when using the district heating instantiation agent;
               however, in case further data has been instantiated in the very same
               namespace, this assumption might not hold anymore)

        Returns:
            airtemp {str} -- dataIRI of ambient air temperature measure (time series)
            holiday {str} -- dataIRI of public holiday (time series)
        """

        # Exclude AirTemperature instances which are reported by weather stations,
        # which get added by weather agent required for dispersion simulation
        query = f"""
            SELECT DISTINCT ?measure ?holiday
            WHERE {{   
            ?airtemp <{RDF_TYPE}> <{EMS_AIRTEMPERATURE}> ; 
                     <{OM_HAS_VALUE}> ?measure . 
            FILTER NOT EXISTS {{ ?station <{EMS_REPORTS}> ?airtemp }} . 
            ?holiday <{RDF_TYPE}> <{OHN_ISPUBLICHOLIDAY}> .
            }}
        """
        query = self.remove_unnecessary_whitespace(query)
        res = self.performQuery(query)
        
        # Extract unique query results (otherwise exception is thrown)
        airtemp = self.get_unique_value(res, 'measure')
        holiday = self.get_unique_value(res, 'holiday')
        
        return airtemp, holiday
    

    def get_heat_demand(self):
        """
        Returns IRI of total heat demand to satisfy/forecast

        Returns:
            demand {str} -- IRI of heat demand
        """

        query = f"""
            SELECT DISTINCT ?demand
            WHERE {{   
            ?demand <{RDF_TYPE}> <{OHN_HEAT_DEMAND}> .
            }}
        """
        query = self.remove_unnecessary_whitespace(query)
        res = self.performQuery(query)
        
        # Extract unique query results (otherwise exception is thrown)
        return self.get_unique_value(res, 'demand')
    

    def get_grid_temperatures(self):
        """
        Returns IRIs of flow and return temperatures of municipal utility and EfW plant

        Returns:
            flow temperature of municipal utility {str}
            return temperature of municipal utility {str}
            flow temperature of EfW plant {str}
            return temperature of EfW plant {str}
        """

        query = f"""
            SELECT DISTINCT ?mu_temp_flow ?mu_temp_return ?efw_temp_flow ?efw_temp_return
            WHERE {{   
            ?efw_plant <{RDF_TYPE}> <{OHN_INCINERATIONPLANT}> ;
                       <{OHN_HAS_DOWNSTREAM_GRIDCONNECTION}> ?efw_downstream ;
                       <{OHN_HAS_UPSTREAM_GRIDCONNECTION}> ?efw_upstream .
            ?efw_downstream <{OHN_HAS_OBSERVABLE_PROPERTY}> ?efw_temp_flow .
            ?efw_temp_flow <{RDF_TYPE}> <{OM_TEMPERATURE}> .
            ?efw_upstream <{OHN_HAS_OBSERVABLE_PROPERTY}> ?efw_temp_return .
            ?efw_temp_return <{RDF_TYPE}> <{OM_TEMPERATURE}> .
            
            ?mu_plant <{RDF_TYPE}> <{OHN_MUNICIPAL_UTILITY}> ;
                      <{OHN_HAS_DOWNSTREAM_GRIDCONNECTION}> ?mu_downstream ;
                      <{OHN_HAS_UPSTREAM_GRIDCONNECTION}> ?mu_upstream .
            ?mu_downstream <{OHN_HAS_OBSERVABLE_PROPERTY}> ?mu_temp_flow .
            ?mu_temp_flow <{RDF_TYPE}> <{OM_TEMPERATURE}> .
            ?mu_upstream <{OHN_HAS_OBSERVABLE_PROPERTY}> ?mu_temp_return .
            ?mu_temp_return <{RDF_TYPE}> <{OM_TEMPERATURE}> . 
            }}
        """
        query = self.remove_unnecessary_whitespace(query)
        res = self.performQuery(query)
        
        # Extract unique query results (otherwise exception is thrown)
        mu_temp_flow = self.get_unique_value(res, 'mu_temp_flow')
        mu_temp_return = self.get_unique_value(res, 'mu_temp_return')
        efw_temp_flow = self.get_unique_value(res, 'efw_temp_flow')
        efw_temp_return = self.get_unique_value(res, 'efw_temp_return')
        
        return mu_temp_flow, mu_temp_return, efw_temp_flow, efw_temp_return
    

    def get_forecast_derivations(self):
        """
        Query already instantiated heat demand and grid temperature forecast 
        derivation IRIs

        Returns:
            derivation_iris {list} -- list of instantiated derivation IRIs
        """

        query = f"""
            SELECT DISTINCT ?derivation_iri
            WHERE {{
            ?derivation_iri <{ONTODERIVATION_ISDERIVEDFROM}> ?input .
            ?input <{RDF_TYPE}> ?input_type . 
            FILTER (?input_type = <{OHN_HEAT_DEMAND}> || 
                    ?input_type = <{OM_TEMPERATURE}> &&
                    EXISTS {{ ?input ^<{OHN_HAS_OBSERVABLE_PROPERTY}>/<{RDF_TYPE}> <{OHN_GRIDCONNECTION}> }} )
            }}
        """
        query = self.remove_unnecessary_whitespace(query)
        res = self.performQuery(query)
        
        # Extract list of unique query results
        return self.get_list_of_unique_values(res, 'derivation_iri')


    def get_downstream_derivation(self, derivation_iri:str):
        """
        Queries IRI of downstream derivation, i.e., derivation which uses
        output of current derivation as input

        Returns:
            downstream {str} -- IRI of downstream derivation
        """

        query = f"""
            SELECT DISTINCT ?downstream
            WHERE {{
            <{derivation_iri}> ^<{ONTODERIVATION_BELONGSTO}> ?output .
            ?output ^<{ONTODERIVATION_ISDERIVEDFROM}> ?downstream
            }}
        """
        query = self.remove_unnecessary_whitespace(query)
        res = self.performQuery(query)
        
        # Extract list of unique query results
        return self.get_list_of_unique_values(res, 'downstream')


    def get_pure_trigger_inputs(self, derivation_iris:list):
        """
        Queries IRIs of already instantiated time:Interval (incl. associated
        time:Instants) and disp:SimulationTime instances, which are associated
        with current chain of derivations

        Returns:
            sim_t {str} -- IRI of simulation time instance
            interval {str} -- IRI of forecast/optimisation interval instance
            start {str} -- IRI of interval start instance
            end {str} -- IRI of interval end instance
        """

        query = f"""
            SELECT DISTINCT ?sim_t ?interval ?start ?end
            WHERE {{
            VALUES ?derivation_iri {{ <{'> <'.join(derivation_iris)}> }}
            {{ ?sim_t <{RDF_TYPE}> <{OD_SIMULATION_TIME}> ;
                      ^<{ONTODERIVATION_ISDERIVEDFROM}> ?derivation_iri .
            }} UNION {{
               ?interval <{RDF_TYPE}> <{TIME_INTERVAL}> ;
                         ^<{ONTODERIVATION_ISDERIVEDFROM}> ?derivation_iri ;
                         <{TIME_HASBEGINNING}> ?start ;
                         <{TIME_HASEND}> ?end }}
            }}
        """
        query = self.remove_unnecessary_whitespace(query)
        res = self.performQuery(query)

        # Extract unique query results
        return self.get_unique_value(res, 'sim_t'), self.get_unique_value(res, 'interval'), \
               self.get_unique_value(res, 'start'), self.get_unique_value(res, 'end')
    

    def get_derivation_outputs(self, derivation_iris: list):
        """
        Returns all output instances of a given list of derivation instances

        Returns:
            outputs {dict} -- Dictionary of derivation outputs, with rdf types 
                              as keys and instances as list of IRI strings
        """
        query = f"""SELECT DISTINCT ?output ?output_type
            WHERE {{
                VALUES ?derivation_iri {{ <{'> <'.join(derivation_iris)}> }} .
                ?output <{ONTODERIVATION_BELONGSTO}> ?derivation_iri .
                ?output a ?output_type .
            }}"""
        query = self.remove_unnecessary_whitespace(query)
        response = self.performQuery(query)
        if len(response) == 0:
            return None
        else:
            # Derivation outputs (i.e. belongsTo)
            key = set([x['output_type'] for x in response])
            outputs = {k: set([x['output'] for x in response if x['output_type'] == k]) for k in key}
        
        return outputs


    #
    # SPARQL UPDATES
    #
    def instantiate_covariate_relationships(self, fcmodel_iri: str, covariates:list):
        # Create and execute SPARQL update to connect forecasting model with covariates

        update = "INSERT DATA {"
        for cov in covariates:
            update += f"<{fcmodel_iri}> <{TS_HASCOVARIATE}> <{cov}> . "
        update += "}"

        update = self.remove_unnecessary_whitespace(update)
        self.performUpdate(update)
    

    def instantiate_time_interval(self, interval_iri: str, beginning_iri: str, end_iri: str,
                                  unix_beginning: int, unix_end: int):
        # Instantiate new time interval instance
        update = self.instantiate_time_interval_query(interval_iri, beginning_iri, end_iri)
        update = self.remove_unnecessary_whitespace(update)
        self.performUpdate(update)
        # Instantiate corresponding time:Instant instances
        self.instantiate_time_instant(beginning_iri, unix_beginning)
        self.instantiate_time_instant(end_iri, unix_end)


    def instantiate_time_instant(self, instance_iri: str, unix_time_iri: int,
                                  instance_type:str = TIME_INSTANT):
        # Instantiate new time:Instant instance
        update = self.instantiate_time_instant_query(instance_iri, unix_time_iri,
                                                     instance_type)
        update = self.remove_unnecessary_whitespace(update)
        self.performUpdate(update)


    def instantiate_time_duration(self, duration_iri: str, unit_iri: str, value: float,
                                  rdf_type: str = TIME_DURATION):
        # Instantiate new time:Duration instance
        update = self.instantiate_time_duration_query(duration_iri, rdf_type,
                                                      unit_iri, value)
        update = self.remove_unnecessary_whitespace(update)
        self.performUpdate(update)


    def update_time_instant(self, instance_iri: str, unix_time_iri: int):
        # Update existing time:Instant instance
        update = self.update_time_instant_query(instance_iri, unix_time_iri)
        update = self.remove_unnecessary_whitespace(update)
        self.performUpdate(update)


    def instantiate_time_interval_query(self, interval_iri: str, beginning_iri: str, end_iri: str) -> str:
        # Create SPARQL update to instantiate new time interval
        query = f"""
        INSERT DATA {{
            <{interval_iri}> <{RDF_TYPE}> <{TIME_INTERVAL}> ; 
                             <{TIME_HASBEGINNING}> <{beginning_iri}> ;
                             <{TIME_HASEND}> <{end_iri}> .
        }}
        """

        return query
    

    def instantiate_time_duration_query(self, duration_iri: str, rdf_type: str,
                                        unit_iri: str, value: float) -> str:
        # Create SPARQL update to instantiate new time duration
        # rdf:type shall be either TIME_DURATION or TS_FREQUENCY
        query = f"""
        INSERT DATA {{
            <{duration_iri}> <{RDF_TYPE}> <{rdf_type}> ; 
                             <{TIME_UNIT_TYPE}> <{unit_iri}> ;
                             <{TIME_NUMERICDURATION}> "{value}"^^<{XSD_DECIMAL}> .
        }}
        """

        return query
    

    def instantiate_time_instant_query(self, instance_iri: str, unix_time_iri: int,
                                       instance_type:str) -> str:
        # Create SPARQL update to instantiate new time:Instant
        time_pos_iri = KB + 'TimePosition_' + str(uuid.uuid4())

        query = f"""
        INSERT DATA {{
            <{instance_iri}> <{RDF_TYPE}> <{instance_type}> ; 
                             <{TIME_INTIMEPOSITION}> <{time_pos_iri}> .
            <{time_pos_iri}> <{RDF_TYPE}> <{TIME_TIMEPOSITION}> ;
                             <{TIME_NUMERICPOSITION}> "{unix_time_iri}"^^<{XSD_DECIMAL}> ;
                             <{TIME_HASTRS}> <{UNIX_TIME}> .
        }}
        """

        return query
    

    def update_time_instant_query(self, instance_iri: str, unix_time_iri: int) -> str:
        # Create SPARQL update to update time stamp of instantiate time:Instant

        query = f"""
        DELETE {{
            ?time_pos <{TIME_NUMERICPOSITION}> ?old_unix . 
        }} INSERT {{
            ?time_pos <{TIME_NUMERICPOSITION}> "{unix_time_iri}"^^<{XSD_DECIMAL}> .
        }} WHERE {{
            <{instance_iri}> <{TIME_INTIMEPOSITION}> ?time_pos .
            ?time_pos <{TIME_NUMERICPOSITION}> ?old_unix .
        }}
        """

        return query
    

    def initialise_namespace(self, folder_path:str):
        """
        This function initialises the agent's KG namespace by uploading all 
        .ttl files in the given folder path, currently includes:
            - required forecasting model triples 
        """
        
        # Get all .ttl files in folder
        pathlist = Path(folder_path).glob('*.ttl')
        for path in pathlist:
            # Load data
            with open(path, 'r') as f:
                filedata = f.readlines()
            # Remove all comments and replace prefix placeholder
            triples = [s for s in filedata if not s.startswith("#")]
            triples = ' '.join(triples)
            triples = triples.replace('[prefix]', KB)            
            # Parse amended .ttl file and upload to KG
            g = Graph()
            g.parse(data=triples, format='turtle')
            self.uploadGraph(g)


    #
    # Helper functions
    #
    def remove_unnecessary_whitespace(self, query: str) -> str:
        # Remove unnecessary whitespaces
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


    def get_unique_value(self, res: list, key: str) -> str:
        """
        Unpacks a query result list (i.e., list of dicts) into unique 
        value for the given dict key (raise Exception if no/multiple values found)
        """
        res_list =  self.get_list_of_unique_values(res, key)
        if len(res_list) == 1:
            return res_list[0]
        else:
            if len(res_list) == 0:
                msg = f"No unique IRI found for key: {key}."
            else:
                msg = f"Multiple IRIs found for key: {key}."
            logger.error(msg)
            raise ValueError(msg)