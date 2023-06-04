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
    
    def get_resultedconsumption(self, resulted_consumption_iri):
        query_string = f"""
        SELECT ?region ?start ?end ?elec_resulted_consump ?gas_resulted_consump
        WHERE {{
        ?region <{REGION_HAS_ENERGYCONSUMPTION_PROFILE}> <{resulted_consumption_iri}>.
        <{resulted_consumption_iri}>  <{RDF_TYPE}> <{REGION_RESULTED_ENERGYCONSUMPTION}> ;
                 <{OFP_VALIDFROM}> ?start ;
                 <{OFP_VALIDTO}> ?end .
        ?elec_consumption_iri <{IS_A}> <{resulted_consumption_iri}> .
        ?elec_consumption_iri  <{RDF_TYPE}> <{REGION_RESULTED_ELECTRICITY_CONSUMPTION}> ;
                 <{OM_HAS_NUMERICALVALUE}> ?elec_resulted_consump .
                  
        ?gas_consumption_iri <{IS_A}> <{resulted_consumption_iri}> .
        ?gas_consumption_iri  <{RDF_TYPE}> <{REGION_RESULTED_GAS_CONSUMPTION}> ;
                 <{OM_HAS_NUMERICALVALUE}> ?gas_resulted_consump  . }}
        """
        query_string = self.remove_unnecessary_whitespace(query_string)
        res = self.performQuery(query_string)
        if not res:
            # In case some vars are missing (i.e. empty SPARQL result), return Nones
            res = dict(zip(['region', 'start', 'end', 'elec_resulted_consump', 'gas_resulted_consump'], (None,)*5))
        else:
            res = res[0]
            try:
                res['region'] = str(res['region'])
            except:
                res['region'] = None

            try:
                res['elec_resulted_consump'] = float(res['elec_resulted_consump'])
            except:
                res['elec_resulted_consump'] = None

            try:
                res['gas_resulted_consump'] = float(res['gas_resulted_consump'])
            except:
                res['gas_resulted_consump'] = None
        
        return res
    
    def generate_utility_cost_iri(self, region, start, end, gas_cost, elec_cost):
        
        query_string = f"""
        SELECT ?utility_cost_iri
        WHERE {{
        <{region}> <{ONTOCAPE_HASUTILITYCOST}> ?utility_cost_iri.
        ?utility_cost_iri  <{RDF_TYPE}> <{ONTOCAPE_UTILITYCOST}> ;
                 <{OFP_VALIDFROM}> "{start}"^^<{XSD_STRING}> ;
                 <{OFP_VALIDTO}> "{end}"^^<{XSD_STRING}> .
        }}
        """
        res = self.performQuery(query_string)

        if not res:
            utility_cost_iri = ONTOCAPE + "UtilityCosts_" + str(uuid.uuid4())
            logger.info(f'No existed utility_cost_iri, created {utility_cost_iri}')
        else: 
            res = res[0]
            utility_cost_iri = str(res["utility_cost_iri"])
            logger.info(f'utility_cost_iri: {utility_cost_iri} will be used')

        query_string = f"""
        SELECT ?elec_cost_iri
        WHERE {{
        ?elec_cost_iri <{IS_A}> <{utility_cost_iri}> .
        ?elec_cost_iri  <{RDF_TYPE}> <{ONTOCAPE_ELECTRICITYCOSTS}> ;
                 <{OM_HAS_NUMERICALVALUE}> "{elec_cost}"^^<{XSD_FLOAT}> .
        }}
        """
        res = self.performQuery(query_string)

        if not res:
            elec_cost_iri = ONTOCAPE + "ElectricityCosts_" + str(uuid.uuid4())
            logger.info(f'No existed elec_cost_iri, created {elec_cost_iri}')
        else: 
            res = res[0]
            elec_cost_iri = str(res["elec_cost_iri"])
            logger.info(f'elec_cost_iri: {elec_cost_iri} will be used')

        query_string = f"""
        SELECT ?fuel_cost_iri
        WHERE {{
        ?fuel_cost_iri <{IS_A}> <{utility_cost_iri}> .
        ?fuel_cost_iri  <{RDF_TYPE}> <{ONTOCAPE_FUELCOSTS}> .
        }}
        """
        res = self.performQuery(query_string)

        if not res:
            fuel_cost_iri = ONTOCAPE + "FuelCosts_" + str(uuid.uuid4())
            logger.info(f'No existed fuel_cost_iri, created {fuel_cost_iri}')
        else: 
            res = res[0]
            fuel_cost_iri = str(res["fuel_cost_iri"])
            logger.info(f'fuel_cost_iri: {fuel_cost_iri} will be used')
    
        query_string = f"""
        SELECT ?gas_cost_iri
        WHERE {{
        ?gas_cost_iri <{IS_A}> <{fuel_cost_iri}> .
        ?gas_cost_iri  <{RDF_TYPE}> <{REGION_GASCOSTS}> ;
                 <{OM_HAS_NUMERICALVALUE}> "{gas_cost}"^^<{XSD_FLOAT}> .
        }}
        """
        res = self.performQuery(query_string)

        if not res:
            gas_cost_iri = REGION + "GasCosts_" + str(uuid.uuid4())
            logger.info(f'No existed gas_cost_iri, created {gas_cost_iri}')
        else: 
            res = res[0]
            gas_cost_iri = str(res["gas_cost_iri"])
            logger.info(f'gas_cost_iri: {gas_cost_iri} will be used')
    
        return utility_cost_iri, elec_cost_iri, fuel_cost_iri, gas_cost_iri

    def instantiate_utilitycosts(self, g, utility_cost_iri, elec_cost_iri, 
                                 fuel_cost_iri, gas_cost_iri, region, start, end, 
                                 gas_cost, elec_cost):
        
        g.add((URIRef(region),URIRef(ONTOCAPE_HASUTILITYCOST),URIRef(utility_cost_iri)))
        g.add((URIRef(utility_cost_iri),URIRef(RDF_TYPE),URIRef(ONTOCAPE_UTILITYCOST)))
        g.add((URIRef(utility_cost_iri),URIRef(OFP_VALIDFROM),Literal(start, datatype=XSD_STRING)))
        g.add((URIRef(utility_cost_iri),URIRef(OFP_VALIDTO),Literal(end, datatype=XSD_STRING)))
        g.add((URIRef(elec_cost_iri),URIRef(IS_A),URIRef(utility_cost_iri)))
        g.add((URIRef(elec_cost_iri),URIRef(RDF_TYPE),URIRef(ONTOCAPE_ELECTRICITYCOSTS)))
        g.add((URIRef(elec_cost_iri),URIRef(OM_HAS_NUMERICALVALUE),Literal(elec_cost, datatype=XSD_FLOAT)))
        g.add((URIRef(fuel_cost_iri),URIRef(IS_A),URIRef(utility_cost_iri)))
        g.add((URIRef(fuel_cost_iri),URIRef(RDF_TYPE),URIRef(ONTOCAPE_FUELCOSTS)))
        g.add((URIRef(gas_cost_iri),URIRef(IS_A),URIRef(fuel_cost_iri)))
        g.add((URIRef(gas_cost_iri),URIRef(RDF_TYPE),URIRef(REGION_GASCOSTS)))
        g.add((URIRef(gas_cost_iri),URIRef(OM_HAS_NUMERICALVALUE),Literal(gas_cost, datatype=XSD_FLOAT)))
    
    def remove_unnecessary_whitespace(self, query: str) -> str:
        # Remove unnecessary whitespaces
        query = ' '.join(query.split())

        return query

#ontop_url = QUERY_ENDPOINT.split('/blazegraph')[0] + '/ontop/ui/sparql'
# a = KGClient(QUERY_ENDPOINT, QUERY_ENDPOINT)
# res = a.get_birthday('http://example.org/birthday#Birthday_robot_1')
    
