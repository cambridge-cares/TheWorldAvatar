################################################
# Authors: Markus Hofmeister (mh807@cam.ac.uk) #    
# Date: 21 Aug 2023                            #
################################################

# The purpose of this module is to provide functionality to execute
# KG queries and updates using the PySparqlClient from the DerivationAgent

import uuid
from rdflib import URIRef, Graph
from distutils.util import strtobool

from pyderivationagent.kg_operations import PySparqlClient

from dhoptimisation.utils import *
from dhoptimisation.datamodel.iris import *


class KGClient(PySparqlClient):
    
    #
    # SPARQL QUERIES
    #
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
            unit_constrain = f"""
                VALUES ?unit {{ <{unit}> }}
                ?dataIRI <{OM_HASUNIT}> ?unit . """            
        else:
            unit_constrain = f"""
                OPTIONAL {{ ?dataIRI <{OM_HASUNIT}> ?unit }} """

        # Specify relationship between instance and dataIRI
        if forecast:
            relationship = TS_HASFORECAST
        else:
            relationship = OM_HASVALUE

        query = f"""
            SELECT ?dataIRI ?unit
            WHERE {{
            <{instance_iri}> <{relationship}> ?dataIRI .
            {unit_constrain}
            }}
        """
        query = self.remove_unnecessary_whitespace(query)
        res = self.performQuery(query)
        
        if len(res) == 0  and not forecast:
            # Allow for direct attachment of time series IRI, in case no
            # Measure is attached, i.e., for operating availability
            query = query.replace(f'<{OM_HASVALUE}>', f'<{OM_HASVALUE}>*')
            res = self.performQuery(query)

        # Extract and return results
        if len(res) == 1:
            return self.get_unique_value(res, 'dataIRI'),  \
                   self.get_unique_value(res, 'unit')

        else:
            # Log situation and throw default exception (to be caught) if no or
            # multiple dataIRIs (with units) are found
            if len(res) == 0:
                logger.info(f'No "dataIRI" associated with given instance: {instance_iri} via {relationship}.')
            else:
                logger.info(f'Multiple "dataIRI"s associated with given instance: {instance_iri} via {relationship}.')
            raise ValueError
            
            
    def get_rdftype(self, instance_iri:str) -> tuple:
        """
        Retrieves the rdf type of the given instance; if the instance is a 
        ts:Forecast, the type of the associated instance is retrieved

        Arguments:
            instance_iri {str} -- IRI of instance for which to retrieve type
        Returns:
            type {str} -- IRI of associated rdf type
        """

        query = f"""
            SELECT ?type
            WHERE {{
                {{ <{instance_iri}> <{RDF_TYPE}> ?type }}
                UNION
                {{ <{instance_iri}> ^<{TS_HASFORECAST}>/<{RDF_TYPE}> ?type }}
                FILTER (?type != <{TS_FORECAST}>)            
            }}
        """
        query = self.remove_unnecessary_whitespace(query)
        res = self.performQuery(query)

        # Extract and return results unique result
        if len(res) == 1:
            return self.get_unique_value(res, 'type')

        else:
            # Throw exception if no or multiple types are found
            if len(res) == 0:
                msg = f'No rdf type associated with given instance: {instance_iri}.'
            else:
                msg = f'Multiple rdf:types associated with given instance: {instance_iri}.'
            raise_error(ValueError, msg)
        

    def get_input_types_from_forecast_iris(self, forecast_iris:list):
        """
        Map forecast instances to corresponding input types, i.e., heat demand
        and flow/return grid temperatures at EfW and municipal utility location

        Arguments:
            forecast_iris (list) -- IRIs of heat demand and grid temperature forecasts
                                    (i.e., inputs to the generation optimisation)
        Returns:
            inputs (dict) -- dictionary with keys 'q_demand', 't_flow_efw', 't_return_efw',
                             't_flow_mu', and 't_return_mu' and mapped forecast IRIs as keys
        """

        query = f"""
            SELECT DISTINCT ?fc_iri1 ?fc_iri2 ?fc_iri3 ?fc_iri4 ?fc_iri5
            WHERE {{
            {{ ?q_demand <{RDF_TYPE}> <{OHN_HEAT_DEMAND}> ;
                         <{TS_HASFORECAST}> ?fc_iri1 .
               ?fc_iri1 <{OM_HASUNIT}> <{OM_MEGAWATTHOUR}> .
               FILTER (?fc_iri1 IN (<{'>, <'.join(forecast_iris)}>) )
            }} UNION {{
            ?efw_plant <{RDF_TYPE}> <{OHN_INCINERATIONPLANT}> ;
                       <{OHN_HAS_DOWNSTREAM_GRIDCONNECTION}> ?efw_downstream ;
                       <{OHN_HAS_UPSTREAM_GRIDCONNECTION}> ?efw_upstream .
            ?efw_downstream <{OHN_HAS_OBSERVABLE_PROPERTY}> ?t_flow_efw .
            ?t_flow_efw <{RDF_TYPE}> <{OM_TEMPERATURE}> ;
                        <{TS_HASFORECAST}> ?fc_iri2 .
            ?efw_upstream <{OHN_HAS_OBSERVABLE_PROPERTY}> ?t_return_efw .
            ?t_return_efw <{RDF_TYPE}> <{OM_TEMPERATURE}> ;
                          <{TS_HASFORECAST}> ?fc_iri3 .
            FILTER (?fc_iri2 IN (<{'>, <'.join(forecast_iris)}>) && 
                    ?fc_iri3 IN (<{'>, <'.join(forecast_iris)}>))
            }} UNION {{
            ?mu_plant  <{RDF_TYPE}> <{OHN_MUNICIPAL_UTILITY}> ;
                       <{OHN_HAS_DOWNSTREAM_GRIDCONNECTION}> ?mu_downstream ;
                       <{OHN_HAS_UPSTREAM_GRIDCONNECTION}> ?mu_upstream  .
            ?mu_downstream <{OHN_HAS_OBSERVABLE_PROPERTY}> ?t_flow_mu .
            ?t_flow_mu <{RDF_TYPE}> <{OM_TEMPERATURE}> ;
                       <{TS_HASFORECAST}> ?fc_iri4 .
            ?mu_upstream  <{OHN_HAS_OBSERVABLE_PROPERTY}> ?t_return_mu .
            ?t_return_mu <{RDF_TYPE}> <{OM_TEMPERATURE}> ;
                         <{TS_HASFORECAST}> ?fc_iri5 . 
            FILTER (?fc_iri4 IN (<{'>, <'.join(forecast_iris)}>) && 
                    ?fc_iri5 IN (<{'>, <'.join(forecast_iris)}>))
            }} }}
        """
        query = self.remove_unnecessary_whitespace(query)
        res = self.performQuery(query)

        # Extract relevant information from query result; returns None if unavailable
        inputs = {'q_demand': self.get_unique_value(res, 'fc_iri1'),
                  't_flow_efw': self.get_unique_value(res, 'fc_iri2'),
                  't_return_efw': self.get_unique_value(res, 'fc_iri3'),
                  't_flow_mu': self.get_unique_value(res, 'fc_iri4'),
                  't_return_mu': self.get_unique_value(res, 'fc_iri5')
        }
        return inputs

    
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

        query = f"""
            SELECT DISTINCT ?ts_iri ?unit ?rdb_url ?time_format
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
            raise_error(ValueError, msg)
        

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
            raise_error(ValueError, "No unique interval details could be retrieved from KG.")
    
    
    def get_existing_optimisation_outputs(self, forecast_iri:str, output_types:list):
        """
        Returns a dict of all generation optimisation outputs associated with
        provided forecast IRI as input
        NOTE: Provides output IRIs only for rdf types listed in 'output_types'

        Arguments:
            forecast_iri (str) -- IRI of heat demand or grid temperature forecast
                                  (i.e., inputs to the generation optimisation)
            output_types (list) -- list of IRIs specifying relevant optimisation 
                                   output types
        Returns:
            outputs (dict) -- dictionary with heat provider IRIs as keys and
                              associated optimisation output dict as values;
                              each provider contains an output dict with all
                              'output_types' as keys and corresponding IRIs as
                              values, with not applicable IRIs set to None
                              (empty dict if no outputs exist)
        """
        
        query = f"""
            SELECT DISTINCT ?source ?type ?output_iri
            WHERE {{
            VALUES ?type {{ <{'> <'.join(output_types)}> }}
            {{ ?output_iri ^<{OHN_HAS_PROVIDED_HEAT_AMOUNT}> ?source }}
            UNION
            {{ ?output_iri ^<{OHN_HAS_GENERATED_HEAT_AMOUNT}> ?source }}
            UNION
            {{ ?output_iri ^<{OHN_HAS_CONSUMED_GAS_AMOUNT}> ?source }}
            UNION
            {{ ?output_iri ^<{OHN_HAS_COGEN_ELECTRICITY_AMOUNT}> ?source }}
            UNION
            {{ ?output_iri ^<{OHN_HAS_OPERATING_AVAILABILITY}> ?source }}
            {{
                <{forecast_iri}> ^<{ONTODERIVATION_ISDERIVEDFROM}> ?deriv_iri . 
                ?deriv_iri ^<{ONTODERIVATION_BELONGSTO}> ?output_iri . 
                ?output_iri <{RDF_TYPE}> ?type .
            }}
            }}
        """
        
        query = self.remove_unnecessary_whitespace(query)
        res = self.performQuery(query)
        
        # Extract all heat providers
        sources = self.get_list_of_unique_values(res, 'source')
        outputs = {}
        # Add output dict {rdf type: iri} for all heat providers
        for s in sources:
            # Extract all relevant outputs for heat provider
            sub_res = [r for r in res if r['source'] == s]
            sub_res = {r['type']: r['output_iri'] for r in sub_res}
            # Ensure that not present outputs are set to None
            sub_res = {k: sub_res.get(k) for k in output_types}
            outputs.update({s: sub_res})
       
        return outputs


    def get_gas_properties(self):
        """
        Returns dictionary with all static gas properties as required for
        overall optimisation setup dict
        NOTE: Required units specified directly in query as optimisation
              algorithm requires values in corresponding units

        Returns:
            props (dict) -- dictionary with static gas properties
        """

        query = f"""
            SELECT DISTINCT ?gp1 ?gp2 ?gp3
            WHERE {{
            ?gas <{RDF_TYPE}> <{OHN_NATURAL_GAS}> ; 
                 <{OHN_HAS_HIGHER_CALORIFICVALUE}> ?ho ; 
                 <{OHN_HAS_LOWER_CALORIFICVALUE}> ?hu ; 
                 <{OHN_HAS_CO2_FACTOR}> ?co2 . 
            {self.get_numerical_value('ho', 'gp1', OM_KILOWATTHOUR_PER_M3)} 
            {self.get_numerical_value('hu', 'gp2', OM_KILOWATTHOUR_PER_M3)} 
            {self.get_numerical_value('co2', 'gp3', OM_TONNE_PER_MEGAWATTHOUR)} 
            }}
        """
        query = self.remove_unnecessary_whitespace(query)
        res = self.performQuery(query)

        # Extract relevant information from unique query result
        if len(res) == 1:
            props = {'gp1': self.get_unique_value(res, 'gp1', float),
                     'gp2': self.get_unique_value(res, 'gp2', float),
                     'gp3': self.get_unique_value(res, 'gp3', float),
            }
            return props
        else:
            raise_error(ValueError, 'Gas properties could not be retrieved unambiguously from KG.')
            
            
    def get_market_prices(self):
        """
        Returns dictionary with all market price instances as required for
        overall optimisation setup dict
        NOTE: For time series data, instance IRIs are included as 
              "placeholers" for which to retrieve ts data subsequently
        Returns:
            props (dict) -- dictionary with market prices
        """

        query = f"""
            SELECT DISTINCT ?mp1 ?mp2 ?mp3 ?mp4 ?mp5 ?mp6
            WHERE {{            
            ?mp1 <{RDF_TYPE}> <{OHN_GAS_UNIT_COST}> ;
                 ^<{OHN_HAS_UNIT_PRICE}>/^<{ONTOCHEMPLANT_HASFUELTYPE}>/<{RDF_TYPE}> <{OHN_HEATBOILER}> .
            ?mp2 <{RDF_TYPE}> <{OHN_GAS_UNIT_COST}> ;
                 ^<{OHN_HAS_UNIT_PRICE}>/^<{ONTOCHEMPLANT_HASFUELTYPE}>/<{RDF_TYPE}> <{OHN_GASTURBINE}> .            
            ?mp3 <{RDF_TYPE}> <{OHN_ELECTRICITY_SPOT_PRICE}> .
            ?mp4 <{RDF_TYPE}> <{OHN_CO2_CERTIFICATE_PRICE}> .
            ?mp5 <{RDF_TYPE}> <{OHN_CHP_BONUS}> .
            ?mp6 <{RDF_TYPE}> <{OHN_GRID_CHARGES}> .
            }}
        """
        query = self.remove_unnecessary_whitespace(query)
        res = self.performQuery(query)

        # Extract relevant information from unique query result
        if len(res) == 1:
            props = {'mp1': self.get_unique_value(res, 'mp1', str),
                     'mp2': self.get_unique_value(res, 'mp2', str),
                     'mp3': self.get_unique_value(res, 'mp3', str),
                     'mp4': self.get_unique_value(res, 'mp4', str),
                     'mp5': self.get_unique_value(res, 'mp5', str),
                     'mp6': self.get_unique_value(res, 'mp6', str),
            }
            return props
        else:
            raise_error(ValueError, 'Market prices could not be retrieved unambiguously from KG.')
            
            
    def get_dh_grid_details(self):
        """
        Returns dictionary with static district heating grid properties
        required for overall optimisation setup dict

        Returns:
            props (dict) -- dictionary with heating grid details
        """

        query = f"""
            SELECT DISTINCT ?dh1
            WHERE {{
                ?grid <{RDF_TYPE}> <{OHN_HEATINGNETWORK}> ;
                      <{RDFS_LABEL}> ?dh1 .
            }}
        """
        query = self.remove_unnecessary_whitespace(query)
        res = self.performQuery(query)

        # Extract relevant information from unique query result
        if len(res) == 1:
            props = {'dh1': self.get_unique_value(res, 'dh1', str)
            }
            return props
        else:
            raise_error(ValueError, 'Heating grid details could not be retrieved unambiguously from KG.')
            
            
    def get_dh_grid_supplier_details(self, grid_name: str, supplier_type: str):
        """
        Returns dictionary with static properties for given supplier type
        as required for overall optimisation setup dict
        NOTE: For time series data, instance IRIs are included as 
              "placeholers" for which to retrieve ts data subsequently
              
        Arguments:
            grid_name (str) -- label of target heating grid
            supplier_type (str) -- rdf type of heat supplier, i.e.,
                                   MunicipalUtility or IncinerationPlant
        Returns:
            props (dict) -- dictionary with heat supplier details
        """

        query = f"""
            SELECT DISTINCT  ?dh3 ?dh4 ?dh5
            WHERE {{
                ?supplier <{RDF_TYPE}> <{supplier_type}> ;
                          <{OHN_PROVIDES_HEAT_TO}>/<{RDFS_LABEL}> \"{grid_name}\" ;
                          <{RDFS_LABEL}> ?dh3 ;
                          <{OHN_HAS_UPSTREAM_GRIDCONNECTION}>/<{OHN_HAS_OBSERVABLE_PROPERTY}> ?dh5 .
                ?dh5 <{RDF_TYPE}> <{OM_PRESSURE}> .
                OPTIONAL {{
                    ?supplier <{OHN_OPERATES}>/<{OHN_HAS_MIN_FLOWRATE}> ?flow_min .
                    {self.get_numerical_value('flow_min', 'dh4', OM_M3_PER_HOUR)} 
                }}
            }}
        """
        query = self.remove_unnecessary_whitespace(query)
        res = self.performQuery(query)

        # Extract relevant information from unique query result
        if len(res) == 1:
            props = {'dh3': self.get_unique_value(res, 'dh3', str),
                     'dh4': self.get_unique_value(res, 'dh4', float),
                     'dh5': self.get_unique_value(res, 'dh5', str)
            }
            return props
        else:
            raise_error(ValueError, 'Grid supplier details could not be retrieved unambiguously from KG.')
            
            
    def get_municipal_utility_details(self):
        """
        Returns dictionary with static municipal utility details required
        for overall optimisation setup dict

        Returns:
            props (dict) -- dictionary with municipal utility details
        """

        query = f"""
            SELECT DISTINCT ?mu1
            WHERE {{
                ?mu <{RDF_TYPE}> <{OHN_MUNICIPAL_UTILITY}> ;
                      <{RDFS_LABEL}> ?mu1 .
            }}
        """
        query = self.remove_unnecessary_whitespace(query)
        res = self.performQuery(query)

        # Extract relevant information from unique query result
        if len(res) == 1:
            props = {'mu1': self.get_unique_value(res, 'mu1', str)
            }
            return props
        else:
            raise_error(ValueError, 'Municipal utility details could not be retrieved unambiguously from KG.')


    def get_heat_providers(self):
        """
        Query IRIs for heat providers, i.e., waste incineration plant 
        as well as heat boilers and gas turbine

        Returns:
            providers (dict) -- dictionary with keys 'efw_plant', 'boilers' 
                                and 'gt' and lists of IRIs as values
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
    
    
    def get_sourcing_contract_properties(self, provider_iri:str):
        """
        Returns dictionary with all static heat sourcing contract properties
        as required for overall optimisation setup dict
        NOTE: For time series data, instance IRIs are included as 
              "placeholers" for which to retrieve ts data subsequently
        NOTE: Assumes that annual sourcing limits (min/max) align
              with tiered unit price thresholds (which is the case for SWPS)
        NOTE: Required units specified directly in query as optimisation
              algorithm requires values in corresponding units

        Arguments:
            provider_iri (str) -- IRI of heat provider, i.e., party fulfilling
                                  the heat provision contract
        Returns:
            props (dict) -- dictionary with heat sourcing contract properties
        """

        query = f"""
            SELECT DISTINCT ?sc1 ?sc4 ?sc5 ?sc6 ?sc7 ?sc8 ?sc9
                            ?tiered_price
            WHERE {{
            <{provider_iri}> ^<{OHN_IS_FULFILLED_BY}> ?contract ;
                      <{RDFS_LABEL}> ?sc5 ;
                      <{OHN_HAS_OPERATING_AVAILABILITY}> ?sc6 ;
                      <{OHN_HAS_MIN_HOURLY_SUPPLY}> ?sc7 ;
                      <{OHN_HAS_MAX_HOURLY_SUPPLY}> ?sc8 ;                   
                      <{OHN_HAS_PROVIDED_HEAT_AMOUNT}> ?sc9 .            
            ?contract <{RDFS_LABEL}> ?sc1 ;
                      <{OHN_HAS_CURRENT_UNIT_PRICE}> ?p ;
                      <{OHN_HAS_TIERED_UNIT_PRICE}> ?tiered_price .
            {self.get_numerical_value('p', 'sc4', OM_EURO_PER_MEGAWATTHOUR)}
            }}
        """
        query = self.remove_unnecessary_whitespace(query)
        res = self.performQuery(query)

        # Extract relevant information from unique query result
        if len(res) == 1:
            props = {'sc1': self.get_unique_value(res, 'sc1', str),
                     'sc4': self.get_unique_value(res, 'sc4', float),
                     'sc5': self.get_unique_value(res, 'sc5', str),
                     'tiered_price': self.get_unique_value(res, 'tiered_price', str),
                     # Add IRIs associated with actual dynamic ts data
                     'sc6': self.get_unique_value(res, 'sc6', str),
                     'sc7': self.get_unique_value(res, 'sc7', str),
                     'sc8': self.get_unique_value(res, 'sc8', str),
                     'sc9': self.get_unique_value(res, 'sc9', str)
            }
            return props
        else:
            raise_error(ValueError, 'Sourcing contract data could not be retrieved unambiguously from KG.')
            
            
    def get_tiered_unit_prices(self, tiered_unit_price_iri:str):
        """
        Returns dictionary with tiered unit price details, i.e.,
        lists of annual energy caps and associated price bands
        NOTE: Required units specified directly in query as optimisation
              algorithm requires values in corresponding units

        Arguments:
            tiered_unit_price_iri (str) -- IRI of tiered unit price
                                    instance with associated tiers
        Returns:
            (dict) -- dictionary with tiered unit price thresholds
        """

        query = f"""
            SELECT DISTINCT ?p ?q
            WHERE {{
            <{tiered_unit_price_iri}> <{OHN_HAS_TIER}> ?tier .
            ?tier <{OHN_HAS_CUMULATIVE_ENERGYCAP}> ?tier_q ;
                  <{OHN_HAS_UNIT_PRICE}> ?tier_p .
            {self.get_numerical_value('tier_q', 'q', OM_MEGAWATTHOUR)} 
            {self.get_numerical_value('tier_p', 'p', OM_EURO_PER_MEGAWATTHOUR)} 
            }}
            ORDER BY ASC(?q)
        """
        query = self.remove_unnecessary_whitespace(query)
        res = self.performQuery(query)

        # Create combined list from query results
        comb = list(zip(self.get_list_of_values(res, 'q', float),
                        self.get_list_of_values(res, 'p', float)))
        # Sort collectively by ascending heat cap (q) - should not be necessary
        # due to sorting in SPARQL query, but just to be sure
        comb.sort()
        q, p = zip(*comb)
        
        return {'sc2': list(q), 'sc3': list(p)}
    
    
    def get_price_tier_iri(self, provider_iri:str, annual_amount:float, 
                           unit=OM_MEGAWATTHOUR):
        """
        Returns IRI of price tier applicable to the total amount of heat
        sourced (in that given year)

        Arguments:
            provider_iri (str) -- IRI of heat provider, i.e., party fulfilling
                                  the heat provision contract
            annual_amount (float) -- cumulative value of annual heat amount sourced
            
        Returns:
            IRI of price tier corresponding to total amount of heat sourced
        """

        # Constrain unit value if given
        unit_constrain = ''
        if unit:
            unit_constrain = f"""
                VALUES ?unit {{ <{unit}> }}
                ?measure <{OM_HASUNIT}> ?unit . """            

        query = f"""
            SELECT DISTINCT ?tier
            WHERE {{
            {unit_constrain}
            <{provider_iri}> ^<{OHN_IS_FULFILLED_BY}> ?contract .
            ?contract <{OHN_HAS_TIERED_UNIT_PRICE}>/<{OHN_HAS_TIER}> ?tier .
            ?tier <{OHN_HAS_CUMULATIVE_ENERGYCAP}>/<{OM_HASVALUE}> ?measure .
            ?measure <{OM_HAS_NUMERICAL_VALUE}> ?amount .
            FILTER (?amount > \"{annual_amount}\"^^<{XSD_FLOAT}> )
            }}
            ORDER BY ?amount
            LIMIT 1
        """
        query = self.remove_unnecessary_whitespace(query)
        res = self.performQuery(query)
        
        return self.get_unique_value(res, 'tier', str)
            
            
    def get_heat_boiler_properties(self, boiler_iri:str):
        """
        Returns dictionary with all static heat boiler properties
        as required for overall optimisation setup dict
        NOTE: For time series data, instance IRIs are included as 
              "placeholers" for which to retrieve ts data subsequently
        NOTE: Required units specified directly in query as optimisation
              algorithm requires values in corresponding units

        Arguments:
            boiler_iri (str) -- IRI of conventional gas boiler
        Returns:
            props (dict) -- dictionary with boiler properties
        """

        query = f"""
            SELECT DISTINCT ?hb1 ?hb2 ?hb7 ?hb8 ?hb9 ?hb10
            WHERE {{
            <{boiler_iri}> <{RDFS_LABEL}> ?hb1 ;
                      <{OHN_HAS_RATED_THERMAL_POWER}> ?power_q ;
                      <{OHN_HAS_OPERATING_AVAILABILITY}> ?hb8 ;
                      <{OHN_HAS_GENERATED_HEAT_AMOUNT}> ?hb10 ;
                      <{OHN_APPLICABLE_OPEX_COMPONENT}> ?comp .
            {self.get_numerical_value('power_q', 'hb2', OM_MEGAWATT)}
            {{
                ?comp <{RDF_TYPE}> <{OHN_DEMAND_DRIVEN_WEAR_COST}>
                BIND(?comp AS ?hb7)
            }}
            UNION
            {{
                ?comp <{RDF_TYPE}> <{OHN_HOURLY_LABOUR_COST}>
                BIND(?comp AS ?hb9)
            }}
            }}
        """
        query = self.remove_unnecessary_whitespace(query)
        res = self.performQuery(query)

        # Extract distinct information from query result (2 entries due to union)
        if len(res) == 2:
            props = {'hb1': self.get_unique_value(res, 'hb1', str),
                     'hb2': self.get_unique_value(res, 'hb2', float),
                     # Add IRIs associated with actual dynamic ts data
                     'hb7': self.get_unique_value(res, 'hb7', str),
                     'hb8': self.get_unique_value(res, 'hb8', str),
                     'hb9': self.get_unique_value(res, 'hb9', str),
                     'hb10': self.get_unique_value(res, 'hb10', str)
            }
            return props
        else:
            raise_error(ValueError, 'Heat boiler data could not be retrieved unambiguously from KG.')
            
            
    def get_gas_turbine_properties(self, gt_iri:str):
        """
        Returns dictionary with all static gas turbine properties
        as required for overall optimisation setup dict
        NOTE: For time series data, instance IRIs are included as 
              "placeholers" for which to retrieve ts data subsequently
        NOTE: Required units specified directly in query as optimisation
              algorithm requires values in corresponding units

        Arguments:
            gt_iri (str) -- IRI of gas turbine
        Returns:
            props (dict) -- dictionary with gas turbine properties
        """

        query = f"""
            SELECT DISTINCT ?gt1 ?gt2 ?gt3 ?gt4 ?gt8 ?gt9 ?gt10 ?gt11 ?gt14
            WHERE {{
            <{gt_iri}> <{RDFS_LABEL}> ?gt1 ;
                      <{OHN_HAS_RATED_ELECTRICAL_POWER}> ?power_el ;
                      <{OHN_HAS_RATED_THERMAL_POWER}> ?power_q ;
                      <{OHN_HAS_MIN_THERMAL_LOAD}> ?q_min ;
                      <{OHN_HAS_MIN_IDLE_TIME}> ?t_min ;
                      <{OHN_HAS_OPERATING_AVAILABILITY}> ?gt10 ;
                      <{OHN_HAS_GENERATED_HEAT_AMOUNT}> ?gt14 ;
                      <{OHN_APPLICABLE_OPEX_COMPONENT}> ?comp .
            {self.get_numerical_value('power_el', 'gt2', OM_MEGAWATT)}
            {self.get_numerical_value('power_q', 'gt3', OM_MEGAWATT)}
            {self.get_numerical_value('q_min', 'gt4', OM_MEGAWATT)}
            {self.get_numerical_value('t_min', 'gt8', OM_HOUR)}
            {{
                ?comp <{RDF_TYPE}> <{OHN_HOURLY_WEAR_COST}>
                BIND(?comp AS ?gt9)
            }}
            UNION
            {{
                ?comp <{RDF_TYPE}> <{OHN_HOURLY_LABOUR_COST}>
                BIND(?comp AS ?gt11)
            }}
            }}
        """
        query = self.remove_unnecessary_whitespace(query)
        res = self.performQuery(query)

        # Extract distinct information from query result (2 entries due to union)
        if len(res) == 2:
            props = {'gt1': self.get_unique_value(res, 'gt1', str),
                     'gt2': self.get_unique_value(res, 'gt2', float),
                     'gt3': self.get_unique_value(res, 'gt3', float),
                     'gt4': self.get_unique_value(res, 'gt4', float),
                     'gt8': self.get_unique_value(res, 'gt8', int),
                     # Add IRIs associated with actual dynamic ts data
                     'gt9': self.get_unique_value(res, 'gt9', str),
                     'gt10': self.get_unique_value(res, 'gt10', str),
                     'gt11': self.get_unique_value(res, 'gt11', str),
                     'gt14': self.get_unique_value(res, 'gt14', str)
            }
            return props
        else:
            raise_error(ValueError, 'Gas turbine data could not be retrieved unambiguously from KG.')
    

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
        outputs = {OHN_PROVIDED_HEAT_AMOUNT: self.get_unique_value(res, 'heat'),
                   OHN_AVAILABILITY: self.get_unique_value(res, 'availability')
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
        outputs = {OHN_CONSUMED_GAS_AMOUNT: self.get_unique_value(res, 'gas'),
                   OHN_GENERATED_HEAT_AMOUNT: self.get_unique_value(res, 'heat'),
                   OHN_AVAILABILITY: self.get_unique_value(res, 'availability'),
                   OHN_COGEN_ELECTRICITY_AMOUNT: None
        }
        if gt:
            outputs[OHN_COGEN_ELECTRICITY_AMOUNT] = self.get_unique_value(res, 'electricity')
        return outputs
    
    
    def get_historic_generation_datairi(self, heat_generator:str, unit=OM_MEGAWATTHOUR):
        """
        Query dataIRI for historical heat generation for given heat generator
        (gas turbine, gas boiler) 

        Arguments:
            heat_generator (str) -- IRI of heat boiler or gas trubine instance

        Returns:
            dataIRI of historical heat generation
        """

        query = f"""
            SELECT DISTINCT ?dataIRI_hist
            WHERE {{
            ?q <{OM_HASVALUE}> ?dataIRI_hist .
            ?dataIRI_hist <{OM_HASUNIT}> <{unit}> .
            {{ <{heat_generator}> <{OHN_HAS_PROVIDED_HEAT_AMOUNT}> ?q }}
            UNION
            {{ <{heat_generator}> <{OHN_HAS_GENERATED_HEAT_AMOUNT}> ?q }}
            }}
        """
        query = self.remove_unnecessary_whitespace(query)
        res = self.performQuery(query)

        # Extract relevant information from unique query result
        return self.get_unique_value(res, 'dataIRI_hist')
    
    
    def get_historic_qdemand_datairi(self, unit=OM_MEGAWATTHOUR):
        """
        Query dataIRI for historical heat demand to satisfy

        Arguments:
            heat_generator (str) -- IRI of heat boiler or gas trubine instance

        Returns:
            dataIRI of historical heat demand measure
        """

        query = f"""
            SELECT DISTINCT ?dataIRI_hist
            WHERE {{
            ?q <{RDF_TYPE}> <{OHN_HEAT_DEMAND}> ;
               <{OM_HASVALUE}> ?dataIRI_hist .
            ?dataIRI_hist <{OM_HASUNIT}> <{unit}> .
            }}
        """
        query = self.remove_unnecessary_whitespace(query)
        res = self.performQuery(query)

        # Extract relevant information from unique query result
        return self.get_unique_value(res, 'dataIRI_hist')
    
    
    def get_total_generation_cost(self):
        """
        Query dataIRIs for both historical and optimised forecast cost instance
        
        Returns:
            cost {dict} -- dictionary with municipal utility IRI as well as 
                           dataIRIs for cost time series
        """

        query = f"""
            SELECT DISTINCT ?mu ?hist_data_iri ?fc_data_iri
            WHERE {{
            ?mu <{RDF_TYPE}> <{OHN_MUNICIPAL_UTILITY}> .
            OPTIONAL {{
                ?mu <{ONTOCAPE_HASCOST}> ?cost .
                ?cost <{OM_HASVALUE}> ?hist_data_iri ;
                      <{TS_HASFORECAST}> ?fc_data_iri
                }}
            }}
        """
        query = self.remove_unnecessary_whitespace(query)
        res = self.performQuery(query)

        # Extract relevant information from unique query result
        cost = {'mu': self.get_unique_value(res, 'mu'),
                'hist_data_iri': self.get_unique_value(res, 'hist_data_iri'),
                'fc_data_iri': self.get_unique_value(res, 'fc_data_iri')
        }
        return cost


    #
    # SPARQL UPDATES
    # 
    def instantiate_new_outputs(self, g: Graph, outputs: dict):
        """
        Instantiate forecast relationships for all generation optimisation outputs,
        i.e., sourced/generated heat, generated electricity, consumed gas, and
        generator availability
        NOTE: All optimisation outputs are instantiated as forecasts to 
              indicate "hypothetical" nature of values and to not interfere
              with actual historical measurements
        
        Arguments:
            outputs (dict) -- dictionary with rdf types as keys and associated 
                              instance IRIs as values            
        Returns:
            outputs (dict) -- dictionary with rdf types as keys and associated
                              Forecast IRIs as values (i.e., dataIRIs of time series)
        """

        for output in outputs:
            # Suppres instantiation of non applicable instances (i.e., co-gen 
            # electricity for conventional heat boilers)
            if outputs[output] is not None:
                fc_iri = KB + 'Forecast_' + str(uuid.uuid4())
                # Ensure inclusion of top output instance rdf types
                g.add((URIRef(outputs[output]), URIRef(RDF_TYPE), URIRef(output)))
                g.add((URIRef(outputs[output]), URIRef(TS_HASFORECAST), URIRef(fc_iri)))
                g.add((URIRef(fc_iri), URIRef(RDF_TYPE), URIRef(TS_FORECAST)))
                outputs[output] = fc_iri
                # Instantiate output units, except for Availability (true/false)
                if output != OHN_AVAILABILITY:
                    g.add((URIRef(fc_iri), URIRef(OM_HASUNIT), URIRef(OM_MEGAWATTHOUR)))

        return g, outputs
    
    
    def update_current_heat_unit_price(self, provider_iri:str, price_tier_iri:str):
        """
        Update connection between sourcing contract and current heat unit price 
        for potentially updated price tier

        Arguments:
            provider_iri (str) -- IRI of heat provider, i.e., party fulfilling
                                  the heat provision contract
            price_tier_iri (iri) -- IRI of (potentially new) price tier
        """
        
        update = f"""
            DELETE {{
                ?contract <{OHN_HAS_CURRENT_UNIT_PRICE}> ?price_old .
            }} INSERT {{
                ?contract <{OHN_HAS_CURRENT_UNIT_PRICE}> ?price_new .
            }} WHERE {{
                <{provider_iri}> ^<{OHN_IS_FULFILLED_BY}> ?contract .
                ?contract <{OHN_HAS_CURRENT_UNIT_PRICE}> ?price_old .
                <{price_tier_iri}> <{OHN_HAS_UNIT_PRICE}> ?price_new .
            }}
        """

        update = self.remove_unnecessary_whitespace(update)
        self.performUpdate(update)
        
        
    def instantiate_generation_cost(self, municipal_utility:str):
        """
        Instantiate relationship for total heat generation cost at municipal
        utility level and add both om:Mease and ts:Forecast for both actual
        historical and optimised forecast cost data

        Arguments:
            municipal_utility (iri) -- IRI of municipal utility
        
        Returns:
            dict with dataIRIs for cost time series (historical and optimised forecast)
        """
        
        cost_iri = KB + 'CostInTimeInterval_' + str(uuid.uuid4())
        measure_iri = KB + 'Measure_' + str(uuid.uuid4())
        fc_iri = KB + 'Forecast_' + str(uuid.uuid4())
        
        update = f"""
            INSERT DATA {{
                <{municipal_utility}> <{ONTOCAPE_HASCOST}> <{cost_iri}> .
                <{cost_iri}> <{RDF_TYPE}> <{OHN_COST_IN_TIME_INTERVAL}> .
                <{cost_iri}> <{OM_HASVALUE}> <{measure_iri}> .
                <{measure_iri}> <{RDF_TYPE}> <{OM_MEASURE}> .
                <{measure_iri}> <{OM_HASUNIT}> <{OM_EURO}> .
                <{cost_iri}> <{TS_HASFORECAST}> <{fc_iri}> .
                <{fc_iri}> <{RDF_TYPE}> <{TS_FORECAST}> .
                <{fc_iri}> <{OM_HASUNIT}> <{OM_EURO}> .
            }}
        """

        update = self.remove_unnecessary_whitespace(update)
        self.performUpdate(update)
        
        return {'hist_data_iri' : measure_iri, 'fc_data_iri': fc_iri }
    

    #
    # HELPER FUNCTIONS
    #
    def get_numerical_value(self, var_input, var_output, unit_iri):
        """
        Returns sub-query to retrieve numerical value for specific unit of
        a quantity IRI given as 'var_input'
        """
        var = str(uuid.uuid4()).replace('-', '')
        query = f"""
            ?{var_input} <{OM_HASVALUE}> ?{var} .
            ?{var} <{OM_HAS_NUMERICAL_VALUE}> ?{var_output} ;
                   <{OM_HASUNIT}> <{unit_iri}> .
        """
        return query
        
        
    def remove_unnecessary_whitespace(self, query: str) -> str:
        """
        Remove unnecessary whitespaces
        """
        query = ' '.join(query.split())
        return query


    def get_list_of_unique_values(self, res: list, key: str, cast_to=None) -> list:
        """
        Unpacks a query result list (i.e., list of dicts) into a list of 
        unique values for the given dict key.
        
        Tries to cast results to int/float in case 'cast_to' datatype is provided
        """    
        res_list =  list(set([r.get(key) for r in res]))
        res_list = [v for v in res_list if v is not None]
        if cast_to:
            res_list = [cast_to(r) for r in res_list]
        return res_list
    
    
    def get_list_of_values(self, res: list, key: str, cast_to=None) -> list:
        """
        Unpacks a query result list (i.e., list of dicts) into a list of 
        (potentially duplicate) values for the given dict key.
        
        Tries to cast results to int/float in case 'cast_to' datatype is provided
        """    
        res_list =  list([r.get(key) for r in res])
        res_list = [v for v in res_list if v is not None]
        if cast_to:
            res_list = [cast_to(r) for r in res_list]
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
            elif cast_to and issubclass(cast_to, float):
                res_list[0] = cast_to(res_list[0])
            elif cast_to and issubclass(cast_to, int):
                res_list[0] = cast_to(float(res_list[0]))
            return res_list[0]
        else:
            if len(res_list) == 0:
                msg = f"No value found for key: {key}."
            else:
                msg = f"Multiple values found for key: {key}."
            logger.info(msg)
            return None
