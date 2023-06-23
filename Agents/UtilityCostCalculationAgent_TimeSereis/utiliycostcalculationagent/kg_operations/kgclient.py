################################################
# Authors: Jieyang Xu (jx309@cam.ac.uk)        #    
# Date: 17 May 2023                            #
################################################

# The purpose of this module is to provide functionality to execute KG queries
# and updates using the PySparqlClient module from pyderivationagent

import uuid
import datetime as dt
from rdflib import URIRef, Literal

from py4jps import agentlogging
from pyderivationagent.kg_operations import PySparqlClient
from utiliycostcalculationagent.utils.stack_configs import ELECTRICITY_UNIT_COST, GAS_UNIT_COST, YEAR

from utiliycostcalculationagent.datamodel.iris import *
from utiliycostcalculationagent.errorhandling.exceptions import *

# Initialise logger instance (ensure consistent logger level with `entrypoint.py`)
logger = agentlogging.get_logger('prod')


class KGClient(PySparqlClient):
    #
    # SPARQL QUERIES
    #
    
    def get_country_iri(self):
        # check if there is country exist
        query_string = f'''
        SELECT ?country
        {{
        ?country <{RDF_TYPE}> <{ONTOCAPE_COUNTRY}>.
        }}
        '''
        res = self.performQuery(query_string)

        if not res:
            logger.error("No existed country iri, please go to run 'upper_level_ontology_update.py'")
            raise InvalidInput("No existed country iri, please go to run 'upper_level_ontology_update.py'")
        else: 
            res = res[0]
            country_iri = str(res["country"])
            logger.info(f'country iri: {country_iri} has been identified')
        
        return country_iri

    # ----------- Verify unit rate iri --------------- #
    def get_unit_rate_iri(self, country_iri):
        query_string = f"""
        SELECT ?unit_rate_iri ?elec_unit_rate_iri ?fuel_unit_rate_iri ?gas_unit_rate_iri
        WHERE {{
         <{country_iri}> <{ONTOHEATNETWORK_HASUNITRATE}> ?unit_rate_iri .
         ?unit_rate_iri <{RDF_TYPE}> <{ONTOHEATNETWORK_UNITRATE}> ;
                                <{OFP_VALIDFROM}> "2020-01-01T12:00:00.000Z"^^<{XSD_DATETIME}> ;
                                <{OFP_VALIDTO}> "2020-12-31T12:00:00.000Z"^^<{XSD_DATETIME}> .
         ?elec_unit_rate_iri <{IS_A}> ?unit_rate_iri;
                          <{RDF_TYPE}>  <{REGION_ELECTRICITYUNITCOST}>.
         ?fuel_unit_rate_iri <{IS_A}> ?unit_rate_iri;
                          <{RDF_TYPE}>  <{ONTOHEATNETWORK_FUELUNITCOST}>.
         ?gas_unit_rate_iri <{IS_A}> ?fuel_unit_rate_iri;
                          <{RDF_TYPE}>  <{ONTOHEATNETWORK_GASUNITCOST}>.
        }}
        """
        res = self.performQuery(query_string)

        if not res:
            unit_rate_iri = ONTOHEATNETWORK + "UnitRate_" + str(uuid.uuid4())
            logger.info(f'No existed unit_rate_iri, created {unit_rate_iri}')
            elec_unit_rate_iri = REGION + "ElectricityUnitCost_" + str(uuid.uuid4())
            logger.info(f'No existed elec_unit_rate_iri, created {elec_unit_rate_iri}')
            fuel_unit_rate_iri = ONTOHEATNETWORK + "FuelUnitCost_" + str(uuid.uuid4())
            logger.info(f'No existed fuel_unit_rate_iri, created {fuel_unit_rate_iri}')
            gas_unit_rate_iri = ONTOHEATNETWORK + "GasUnitCost_" + str(uuid.uuid4())
            logger.info(f'No existed gas_unit_rate_iri, created {gas_unit_rate_iri}')
        else: 
            res = res[0]
            if res["unit_rate_iri"] == "" or res["unit_rate_iri"] == None:
                unit_rate_iri = ONTOHEATNETWORK + "UnitRate_" + str(uuid.uuid4())
                logger.info(f'No existed unit_rate_iri, created {unit_rate_iri}')
            else:
                unit_rate_iri = str(res["unit_rate_iri"])
                logger.info(f'unit_rate_iri: {unit_rate_iri} will be used')

            if res["elec_unit_rate_iri"] == "" or res["elec_unit_rate_iri"] == None:
                elec_unit_rate_iri = REGION + "ElectricityUnitCost_" + str(uuid.uuid4())
                logger.info(f'No existed elec_unit_rate_iri, created {elec_unit_rate_iri}') 
            else:
                elec_unit_rate_iri = str(res["elec_unit_rate_iri"])
                logger.info(f'elec_unit_rate_iri: {elec_unit_rate_iri} will be used')

            if res["fuel_unit_rate_iri"] == "" or res["fuel_unit_rate_iri"] == None:
                fuel_unit_rate_iri = ONTOHEATNETWORK + "FuelUnitCost_" + str(uuid.uuid4())
                logger.info(f'No existed fuel_unit_rate_iri, created {fuel_unit_rate_iri}')
            else:
                fuel_unit_rate_iri = str(res["fuel_unit_rate_iri"])
                logger.info(f'fuel_unit_rate_iri: {fuel_unit_rate_iri} will be used')

            if res["gas_unit_rate_iri"] == "" or res["gas_unit_rate_iri"] == None:
                gas_unit_rate_iri = ONTOHEATNETWORK + "GasUnitCost_" + str(uuid.uuid4())
                logger.info(f'No existed gas_unit_rate_iri, created {gas_unit_rate_iri}')
            else:
                gas_unit_rate_iri = str(res["gas_unit_rate_iri"])
                logger.info(f'gas_unit_rate_iri: {gas_unit_rate_iri} will be used')
        
        return unit_rate_iri, elec_unit_rate_iri, fuel_unit_rate_iri, gas_unit_rate_iri
    
    def update_unit_rate_iri(self):

        country_iri = self.get_country_iri()
        unit_rate_iri, elec_unit_rate_iri, fuel_unit_rate_iri, gas_unit_rate_iri = self.get_unit_rate_iri(country_iri)
        
        query_string = f"""
        INSERT DATA {{
         <{country_iri}> <{ONTOHEATNETWORK_HASUNITRATE}> <{unit_rate_iri}>  .
        <{unit_rate_iri}>  <{RDF_TYPE}> <{ONTOHEATNETWORK_UNITRATE}> ;
                                <{OFP_VALIDFROM}> "2020-01-01T12:00:00.000Z"^^<{XSD_DATETIME}> ;
                                <{OFP_VALIDTO}> "2020-12-31T12:00:00.000Z"^^<{XSD_DATETIME}> .
         <{elec_unit_rate_iri}> <{IS_A}> <{unit_rate_iri}> ;
                          <{RDF_TYPE}>  <{REGION_ELECTRICITYUNITCOST}> ;
                          <{OM_HAS_NUMERICALVALUE}> "{ELECTRICITY_UNIT_COST}"^^<{XSD_FLOAT}> .
         <{fuel_unit_rate_iri}> <{IS_A}> <{unit_rate_iri}> ;
                          <{RDF_TYPE}>  <{ONTOHEATNETWORK_FUELUNITCOST}>.
         <{gas_unit_rate_iri}> <{IS_A}> ?fuel_unit_rate_iri;
                          <{RDF_TYPE}>  <{ONTOHEATNETWORK_GASUNITCOST}> ;
                          <{OM_HAS_NUMERICALVALUE}> "{GAS_UNIT_COST}"^^<{XSD_FLOAT}> .
        }}
        """
        res = self.performUpdate(query_string)
    
    # ----------- Get values based on iris ---------- #
    def get_unit_rate(self, unit_rate_iri):
        query_string = f"""
        SELECT ?electricity_unit_cost ?gas_unit_cost 
        WHERE {{
         ?country_iri <{ONTOHEATNETWORK_HASUNITRATE}> <{unit_rate_iri}>  .
        <{unit_rate_iri}>  <{RDF_TYPE}> <{ONTOHEATNETWORK_UNITRATE}> ;
                                <{OFP_VALIDFROM}> "{YEAR}-01-01T12:00:00.000Z"^^<{XSD_DATETIME}> ;
                                <{OFP_VALIDTO}> "{YEAR}-12-31T12:00:00.000Z"^^<{XSD_DATETIME}> .
         ?elec_unit_rate_iri  <{IS_A}> <{unit_rate_iri}> ;
                          <{RDF_TYPE}>  <{REGION_ELECTRICITYUNITCOST}> ;
                          <{OM_HAS_NUMERICALVALUE}> ?electricity_unit_cost .
         ?fuel_unit_rate_iri  <{IS_A}> <{unit_rate_iri}> ;
                          <{RDF_TYPE}>  <{ONTOHEATNETWORK_FUELUNITCOST}>.
         ?gas_unit_rate_iri  <{IS_A}> ?fuel_unit_rate_iri;
                          <{RDF_TYPE}>  <{ONTOHEATNETWORK_GASUNITCOST}> ;
                          <{OM_HAS_NUMERICALVALUE}> ?gas_unit_cost .
        }}
        """
        res = self.performQuery(query_string)
        if not res:
            logger.error("unit cost result can not be found -- go check if it exist")
            raise InvalidInput("unit cost result can not be found -- go check if it exist")
        else:
            res = res[0]
            try:
                electricity_unit_cost = float(res['electricity_unit_cost'])  
            except:
                logger.error("electricity_unit_cost result can not be retrieved -- go check if it exist")
                raise InvalidInput("electricity_unit_cost result can not be retrieved -- go check if it exist")
            try:
                gas_unit_cost = float(res['gas_unit_cost'])  
            except:
                logger.error("gas_unit_cost result can not be retrieved -- go check if it exist")
                raise InvalidInput("gas_unit_cost result can not be retrieved -- go check if it exist")

        return electricity_unit_cost, gas_unit_cost
    
    def get_resulted_gas_elec_consumption_iri(self, resulted_consumption_iri):
        query_string = f"""
        SELECT ?region ?elec_consumption_iri ?gas_consumption_iri
        WHERE {{
        ?region <{REGION_HAS_ENERGYCONSUMPTION_PROFILE}> <{resulted_consumption_iri}>.
        <{resulted_consumption_iri}>  <{RDF_TYPE}> <{REGION_RESULTED_ENERGYCONSUMPTION}> .
        ?elec_consumption_iri <{IS_A}> <{resulted_consumption_iri}> .
        ?elec_consumption_iri  <{RDF_TYPE}> <{REGION_RESULTED_ELECTRICITY_CONSUMPTION}> .
        ?gas_consumption_iri <{IS_A}> <{resulted_consumption_iri}> .
        ?gas_consumption_iri  <{RDF_TYPE}> <{REGION_RESULTED_GAS_CONSUMPTION}> . }}
        """
        query_string = self.remove_unnecessary_whitespace(query_string)
        res = self.performQuery(query_string)
        if not res:
            # In case some vars are missing (i.e. empty SPARQL result), return Nones
            res = dict(zip(['region', 'elec_consumption_iri', 'gas_consumption_iri'], (None,)*3))
        else:
            res = res[0]
            try:
                res['region'] = str(res['region'])
            except:
                res['region'] = None

            try:
                res['elec_consumption_iri'] = str(res['elec_consumption_iri'])
            except:
                res['elec_consumption_iri'] = None

            try:
                res['gas_consumption_iri'] = str(res['gas_consumption_iri'])
            except:
                res['gas_consumption_iri'] = None
        
        return res
    
    def generate_utility_cost_iri(self, region):
        
        query_string = f"""
        SELECT ?utility_cost_iri ?elec_cost_iri ?gas_cost_iri
        WHERE {{
        <{region}> <{ONTOCAPE_HASUTILITYCOST}> ?utility_cost_iri .
        ?utility_cost_iri  <{RDF_TYPE}> <{ONTOCAPE_UTILITYCOST}> .
        ?elec_cost_iri <{IS_A}> ?utility_cost_iri ;
                       <{RDF_TYPE}> <{ONTOCAPE_ELECTRICITYCOSTS}> .
        ?fuel_cost_iri <{IS_A}> ?utility_cost_iri  ;
                       <{RDF_TYPE}> <{ONTOCAPE_FUELCOSTS}> .
        ?gas_cost_iri <{IS_A}> ?fuel_cost_iri ;
                      <{RDF_TYPE}> <{REGION_GASCOSTS}> .
        }}
        """
        res = self.performQuery(query_string)

        if not res:
            utility_cost_iri = ONTOCAPE + "UtilityCosts_" + str(uuid.uuid4())
            logger.info(f'No existed utility_cost_iri, created {utility_cost_iri}')
            elec_cost_iri = ONTOCAPE + "ElectricityCosts_" + str(uuid.uuid4())
            logger.info(f'No existed elec_cost_iri, created {elec_cost_iri}')
            fuel_cost_iri = ONTOCAPE + "FuelCosts_" + str(uuid.uuid4())
            logger.info(f'No existed fuel_cost_iri, created {fuel_cost_iri}')
            gas_cost_iri = REGION + "GasCosts_" + str(uuid.uuid4())
            logger.info(f'No existed gas_cost_iri, created {gas_cost_iri}')
            deletion = False

        else: 
            res = res[0]
            utility_cost_iri = str(res["utility_cost_iri"])
            logger.info(f'utility_cost_iri: {utility_cost_iri} will be used')
            elec_cost_iri = str(res["elec_cost_iri"])
            logger.info(f'elec_cost_iri: {elec_cost_iri} will be used')
            fuel_cost_iri = str(res["fuel_cost_iri"])
            logger.info(f'fuel_cost_iri: {fuel_cost_iri} will be used')
            gas_cost_iri = str(res["gas_cost_iri"])
            logger.info(f'gas_cost_iri: {gas_cost_iri} will be used')
            deletion = True
    
        return utility_cost_iri, elec_cost_iri, fuel_cost_iri, gas_cost_iri, deletion

    def instantiate_utilitycosts(self, g, utility_cost_iri, elec_cost_iri, 
                                 fuel_cost_iri, gas_cost_iri, region):
        
        g.add((URIRef(region),URIRef(ONTOCAPE_HASUTILITYCOST),URIRef(utility_cost_iri)))
        g.add((URIRef(utility_cost_iri),URIRef(RDF_TYPE),URIRef(ONTOCAPE_UTILITYCOST)))
        g.add((URIRef(elec_cost_iri),URIRef(IS_A),URIRef(utility_cost_iri)))
        g.add((URIRef(elec_cost_iri),URIRef(RDF_TYPE),URIRef(ONTOCAPE_ELECTRICITYCOSTS)))
        g.add((URIRef(fuel_cost_iri),URIRef(IS_A),URIRef(utility_cost_iri)))
        g.add((URIRef(fuel_cost_iri),URIRef(RDF_TYPE),URIRef(ONTOCAPE_FUELCOSTS)))
        g.add((URIRef(gas_cost_iri),URIRef(IS_A),URIRef(fuel_cost_iri)))
        g.add((URIRef(gas_cost_iri),URIRef(RDF_TYPE),URIRef(REGION_GASCOSTS)))
    
    def remove_unnecessary_whitespace(self, query: str) -> str:
        # Remove unnecessary whitespaces
        query = ' '.join(query.split())

        return query

    def remove_triples_for_iri(self, *args):
        for dataIRI in args:
            query_string = f"""
            DELETE
            WHERE {{
            <{dataIRI}> ?p ?o .
            }}
            """
            query_string = self.remove_unnecessary_whitespace(query_string)
            self.performUpdate(query_string)

            query_string = f"""
            DELETE
            WHERE {{
            ?s ?p <{dataIRI}> .
            }}
            """
            query_string = self.remove_unnecessary_whitespace(query_string)
            self.performUpdate(query_string)

#ontop_url = QUERY_ENDPOINT.split('/blazegraph')[0] + '/ontop/ui/sparql'
# a = KGClient(QUERY_ENDPOINT, QUERY_ENDPOINT)
# res = a.get_birthday('http://example.org/birthday#Birthday_robot_1')
    
