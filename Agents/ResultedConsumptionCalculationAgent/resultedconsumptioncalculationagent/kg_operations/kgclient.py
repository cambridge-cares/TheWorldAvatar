################################################
# Authors: Jieyang Xu (jx309@cam.ac.uk)        #    
# Date: 17 May 2023                            #
################################################

# The purpose of this module is to provide functionality to execute KG queries
# and updates using the PySparqlClient module from pyderivationagent

import uuid
from rdflib import URIRef, Literal
import ast
from tqdm import tqdm

from py4jps import agentlogging
from pyderivationagent.kg_operations import PySparqlClient
from resultedconsumptioncalculationagent.utils.stack_configs import ONTOP_URL, COP_VAR, BOILER_EFFICIENCY, PROPORTION_OF_HEATING, UPTAKE, ELECTRICITY_CONSUMPTION_PROFILE, GAS_CONSUMPTION_PROFILE, YEAR

from resultedconsumptioncalculationagent.datamodel.iris import *
from resultedconsumptioncalculationagent.errorhandling.exceptions import *

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

    # ---------- Verify assumptions iris ------------ # 
    def get_assumption_iri(self, country_iri):
        query_string = f"""
        SELECT ?assumption_iri
        WHERE {{
        ?assumption_iri <{REGION_APPLICABLETO}> <{country_iri}> .
        }}
        """
        res = self.performQuery(query_string)

        if not res:
            assumption_iri = REGION + "Assumption_" + str(uuid.uuid4())
            logger.info(f'No existed assumption_iri, created {assumption_iri}')
        else: 
            res = res[0]
            assumption_iri = str(res["assumption_iri"])
            logger.info(f'assumption_iri: {assumption_iri} will be used')
        
        return assumption_iri
    
    def get_boiler_efficiency_iri(self, assumption_iri):
        query_string = f"""
        SELECT ?boiler_efficiency_iri
        WHERE {{
        ?boiler_efficiency_iri <{IS_A}> <{assumption_iri}> ;
                    <{RDF_TYPE}> <{REGION_BOILER_EFFICIENCY}> .
        }}
        """
        res = self.performQuery(query_string)

        if not res:
            boiler_efficiency_iri = REGION + "BoilerEfficiency_" + str(uuid.uuid4())
            logger.info(f'No existed boiler_efficiency_iri, created {boiler_efficiency_iri}')
        else: 
            res = res[0]
            boiler_efficiency_iri = str(res["boiler_efficiency_iri"])
            logger.info(f'boiler_efficiency_iri: {boiler_efficiency_iri} will be used')

            # Remove originial triples
            self.remove_triples_for_iri(boiler_efficiency_iri)
        
        return boiler_efficiency_iri
    
    def get_proportion_of_heating_iri(self, assumption_iri):
        query_string = f"""
        SELECT ?proportion_of_heating_iri
        WHERE {{
        ?proportion_of_heating_iri <{IS_A}> <{assumption_iri}> ;
                    <{RDF_TYPE}> <{REGION_PROPORTION_OF_HEATING}> .
        }}
        """
        res = self.performQuery(query_string)

        if not res:
            proportion_of_heating_iri = REGION + "ProportionofHeating_" + str(uuid.uuid4())
            logger.info(f'No existed proportion_of_heating_iri, created {proportion_of_heating_iri}')
        else: 
            res = res[0]
            proportion_of_heating_iri = str(res["proportion_of_heating_iri"])
            logger.info(f'proportion_of_heating_iri: {proportion_of_heating_iri} will be used')
        
            
            # Remove originial triples
            self.remove_triples_for_iri(proportion_of_heating_iri)
        return proportion_of_heating_iri
    
    def get_uptake_iri(self, assumption_iri):
        query_string = f"""
        SELECT ?uptake_iri
        WHERE {{
        ?uptake_iri <{IS_A}> <{assumption_iri}> ;
                    <{RDF_TYPE}> <{REGION_UPTAKE}> .
        }}
        """
        res = self.performQuery(query_string)

        if not res:
            uptake_iri = REGION + "HeatPumpUptake_" + str(uuid.uuid4())
            logger.info(f'No existed uptake_iri, created {uptake_iri}')
        else: 
            res = res[0]
            uptake_iri = str(res["uptake_iri"])
            logger.info(f'uptake_iri: {uptake_iri} will be used')
            
            # Remove originial triples
            self.remove_triples_for_iri(uptake_iri)
        
        return uptake_iri

    def update_assumptions(self):
        country_iri = self.get_country_iri()
        assumption_iri = self.get_assumption_iri(country_iri)
        boiler_efficiency_iri = self.get_boiler_efficiency_iri(assumption_iri)
        proportion_of_heating_iri = self.get_proportion_of_heating_iri(assumption_iri)
        uptake_iri = self.get_uptake_iri(assumption_iri)
        query_string = f"""
        INSERT DATA {{
        <{assumption_iri}> <{REGION_APPLICABLETO}> <{country_iri}> .
        <{boiler_efficiency_iri}> <{IS_A}> <{assumption_iri}> ;
                    <{RDF_TYPE}> <{REGION_BOILER_EFFICIENCY}> ;
                <{OM_HAS_NUMERICALVALUE}> "{BOILER_EFFICIENCY}"^^<{XSD_FLOAT}> .
        <{proportion_of_heating_iri}> <{IS_A}> <{assumption_iri}> ;
                    <{RDF_TYPE}> <{REGION_PROPORTION_OF_HEATING}> ;
            <{OM_HAS_NUMERICALVALUE}> "{PROPORTION_OF_HEATING}"^^<{XSD_FLOAT}> .
        <{uptake_iri}> <{IS_A}> <{assumption_iri}> ;
                    <{RDF_TYPE}> <{REGION_UPTAKE}> ;
            <{OM_HAS_NUMERICALVALUE}> "{UPTAKE}"^^<{XSD_FLOAT}> .
        }}
        """
        query_string = self.remove_unnecessary_whitespace(query_string)
        self.performUpdate(query_string)
    
    # ----------- Verify profiles iri --------------- #
    def get_consumption_profile_iri(self, country_iri):
        query_string = f"""
        SELECT ?consumption_profile_iri ?elec_profile_iri ?gas_profile_iri
        WHERE {{
         <{country_iri}> <{REGION_HAS_ENERGYCONSUMPTION_PROFILE}> ?consumption_profile_iri .
         ?consumption_profile_iri <{RDF_TYPE}> <{REGION_ENERGYCONSUMPTION_PROFILE}> ;
                                <{OFP_VALIDFROM}> "{YEAR}-01-01T12:00:00.000Z"^^<{XSD_DATETIME}> ;
                                <{OFP_VALIDTO}> "{YEAR}-12-31T12:00:00.000Z"^^<{XSD_DATETIME}> .
         ?elec_profile_iri <{IS_A}> ?consumption_profile_iri;
                          <{RDF_TYPE}>  <{REGION_ELECTRICITYCONSUMPTION_PROFILE}>.
         ?gas_profile_iri <{IS_A}> ?consumption_profile_iri;
                          <{RDF_TYPE}>  <{REGION_GASCONSUMPTION_PROFILE}>.
        }}
        """
        res = self.performQuery(query_string)

        if not res:
            consumption_profile_iri = REGION + "EnergyConsumptionProfile_" + str(uuid.uuid4())
            logger.info(f'No existed consumption_profile_iri, created {consumption_profile_iri}')
            elec_profile_iri = REGION + "ElectricityConsumptionProfile_" + str(uuid.uuid4())
            logger.info(f'No existed elec_profile_iri, created {elec_profile_iri}')
            gas_profile_iri = REGION + "GasConsumptionProfile_" + str(uuid.uuid4())
            logger.info(f'No existed gas_profile_iri, created {gas_profile_iri}')
        else: 
            res = res[0]
            if res["consumption_profile_iri"] == "" or res["consumption_profile_iri"] == None:
                consumption_profile_iri = REGION + "EnergyConsumptionProfile_" + str(uuid.uuid4())
                logger.info(f'No existed consumption_profile_iri, created {consumption_profile_iri}')
            else:
                consumption_profile_iri = str(res["consumption_profile_iri"])
                logger.info(f'consumption_profile_iri: {consumption_profile_iri} will be used')
                # Remove originial triples
                self.remove_triples_for_iri(consumption_profile_iri)

            if res["elec_profile_iri"] == "" or res["elec_profile_iri"] == None:
                elec_profile_iri = REGION + "ElectricityConsumptionProfile_" + str(uuid.uuid4())
                logger.info(f'No existed elec_profile_iri, created {elec_profile_iri}') 
            else:
                elec_profile_iri = str(res["elec_profile_iri"])
                logger.info(f'elec_profile_iri: {elec_profile_iri} will be used')
                # Remove originial triples
                self.remove_triples_for_iri(elec_profile_iri)

            if res["gas_profile_iri"] == "" or res["gas_profile_iri"] == None:
                gas_profile_iri = REGION + "GasConsumptionProfile_" + str(uuid.uuid4())
                logger.info(f'No existed gas_profile_iri, created {gas_profile_iri}')
            else:
                gas_profile_iri = str(res["gas_profile_iri"])
                logger.info(f'gas_profile_iri: {gas_profile_iri} will be used')
                # Remove originial triples
                self.remove_triples_for_iri(gas_profile_iri)
        
        return consumption_profile_iri, elec_profile_iri, gas_profile_iri
    
    def update_consumption_profile(self):

        country_iri = self.get_country_iri()
        consumption_profile_iri, elec_profile_iri, gas_profile_iri = self.get_consumption_profile_iri(country_iri)
        
        query_string = f"""
        INSERT DATA {{
         <{country_iri}> <{REGION_HAS_ENERGYCONSUMPTION_PROFILE}> <{consumption_profile_iri}> .
         <{consumption_profile_iri}> <{RDF_TYPE}> <{REGION_ENERGYCONSUMPTION_PROFILE}> ;
                                <{OFP_VALIDFROM}> "{YEAR}-01-01T12:00:00.000Z"^^<{XSD_DATETIME}> ;
                                <{OFP_VALIDTO}> "{YEAR}-12-31T12:00:00.000Z"^^<{XSD_DATETIME}> .
         <{elec_profile_iri}> <{IS_A}> <{consumption_profile_iri}>;
                          <{RDF_TYPE}>  <{REGION_ELECTRICITYCONSUMPTION_PROFILE}> ;
                        <{REGION_HASPROFILEVALUE}> "{ELECTRICITY_CONSUMPTION_PROFILE}"^^<{XSD_STRING}> .

         <{gas_profile_iri}> <{IS_A}> <{consumption_profile_iri}>;
                          <{RDF_TYPE}>  <{REGION_GASCONSUMPTION_PROFILE}> ;
                         <{REGION_HASPROFILEVALUE}> "{GAS_CONSUMPTION_PROFILE}"^^<{XSD_STRING}> .

        }}
        """
        res = self.performUpdate(query_string)

    # ----------- Get values based on iris ---------- #
    def get_consumption(self, comsumption_iri):
        ontop_url = ONTOP_URL
        service_expression = f'SERVICE <{ontop_url}> {{ '
        query_string = f"""
        SELECT ?comsumption
        WHERE {{
        {service_expression}
        <{comsumption_iri}> <{RDF_TYPE}> <{OM_MEASURE}> ;
        <{OM_HAS_NUMERICALVALUE}>  ?comsumption .
        }}}}
        """
        query_string = self.remove_unnecessary_whitespace(query_string)
        res = self.performQuery(query_string)
        if not res:
            logger.warning("Consumption result can not be found -- go check if you uploaded csv files/ or ontop.obda file missing?")
            print("Consumption result can not be found -- go check if you uploaded csv files/ or ontop.obda file missing?")
            comsumption = 0
        else:
            res = res[0]
            try:
                comsumption = float(res['comsumption'])
            except:
                comsumption = 0

        return comsumption

    def get_region(self, cop_iri):
        query_string = f"""
        SELECT ?region
        WHERE {{
        ?region <{REGION_HASCOP}> <{cop_iri}> .
        <{cop_iri}>   <{RDF_TYPE}> <{REGION_COP}> .}}
        """
        query_string = self.remove_unnecessary_whitespace(query_string)
        res = self.performQuery(query_string)
        if not res:
            # In case some vars are missing (i.e. empty SPARQL result), return Nones
            region = None
        else:
            res = res[0]
            try:
                region = str(res['region'])
            except:
                region = None

        return region
    
    def get_proportion_of_heating(self, proportion_of_heating_iri):
        query_string = f"""
        SELECT ?proportion_of_heating
        WHERE {{
        <{proportion_of_heating_iri}> <{IS_A}> ?assumption_iri ;
                    <{RDF_TYPE}> <{REGION_PROPORTION_OF_HEATING}> ;
                    <{OM_HAS_NUMERICALVALUE}>  ?proportion_of_heating .
        }}
        """
        res = self.performQuery(query_string)
        if not res:
            logger.error("proportion_of_heating result can not be found -- go check if it exist")
            raise InvalidInput("proportion_of_heating result can not be found -- go check if it exist")
        else:
            res = res[0]
            try:
                proportion_of_heating = float(res['proportion_of_heating'])
            except:
                proportion_of_heating = None

        return proportion_of_heating

    def get_uptake(self, uptake_iri):
        query_string = f"""
        SELECT ?uptake
        WHERE {{
        <{uptake_iri}> <{IS_A}> ?assumption_iri ;
                    <{RDF_TYPE}> <{REGION_UPTAKE}> ;
                    <{OM_HAS_NUMERICALVALUE}>  ?uptake .
        }}
        """
        res = self.performQuery(query_string)
        if not res:
            logger.error("uptake result can not be found -- go check if it exist")
            raise InvalidInput("uptake result can not be found -- go check if it exist")
        else:
            res = res[0]
            try:
                uptake = float(res['uptake'])
            except:
                uptake = None

        return uptake
    
    def get_boiler_efficiency(self, boiler_efficiency_iri):
        query_string = f"""
        SELECT ?boiler_efficiency
        WHERE {{
        <{boiler_efficiency_iri}> <{IS_A}> ?assumption_iri ;
                    <{RDF_TYPE}> <{REGION_BOILER_EFFICIENCY}> ;
                    <{OM_HAS_NUMERICALVALUE}>  ?boiler_efficiency .
        }}
        """
        res = self.performQuery(query_string)
        if not res:
            logger.error("boiler_efficiency result can not be found -- go check if it exist")
            raise InvalidInput("boiler_efficiency result can not be found -- go check if it exist")
        else:
            res = res[0]
            try:
                boiler_efficiency = float(res['boiler_efficiency'])
            except:
                boiler_efficiency = None

        return boiler_efficiency

    def get_consumption_profile(self, consumption_profile_iri):
        query_string = f"""
        SELECT ?elec_profile ?gas_profile
        WHERE {{
         ?country_iri <{REGION_HAS_ENERGYCONSUMPTION_PROFILE}> <{consumption_profile_iri}> .
         <{consumption_profile_iri}> <{RDF_TYPE}> <{REGION_ENERGYCONSUMPTION_PROFILE}> ;
                                <{OFP_VALIDFROM}> "{YEAR}-01-01T12:00:00.000Z"^^<{XSD_DATETIME}> ;
                                <{OFP_VALIDTO}> "{YEAR}-12-31T12:00:00.000Z"^^<{XSD_DATETIME}> .
         ?elec_profile_iri <{IS_A}> <{consumption_profile_iri}>;
                          <{RDF_TYPE}>  <{REGION_ELECTRICITYCONSUMPTION_PROFILE}> ;
                        <{REGION_HASPROFILEVALUE}> ?elec_profile .

         ?gas_profile_iri <{IS_A}> <{consumption_profile_iri}> ;
                          <{RDF_TYPE}>  <{REGION_GASCONSUMPTION_PROFILE}> ;
                         <{REGION_HASPROFILEVALUE}> ?gas_profile .
        }}
        """
        res = self.performQuery(query_string)
        if not res:
            logger.error("consumption_profile result can not be found -- go check if it exist")
            raise InvalidInput("consumption_profile result can not be found -- go check if it exist")
        else:
            res = res[0]
            try:
                elec_profile = ast.literal_eval(res['elec_profile'])  
            except:
                logger.error("elec_profile result can not be retrieved -- go check if it exist")
                raise InvalidInput("elec_profile result can not be retrieved -- go check if it exist")
            try:
                gas_profile = ast.literal_eval(res['gas_profile'])  
            except:
                logger.error("gas_profile result can not be retrieved -- go check if it exist")
                raise InvalidInput("gas_profile result can not be retrieved -- go check if it exist")

        return elec_profile, gas_profile
    
    def verify_resulted_consumption_iri(self, region):
        
        query_string = f"""
        SELECT ?consumption_iri ?elec_consumption_iri ?gas_consumption_iri
        WHERE {{
        <{region}> <{REGION_HAS_RESULTED_ENERGY_CONSUMPTION}> ?consumption_iri.
        ?consumption_iri  <{RDF_TYPE}> <{REGION_RESULTED_ENERGYCONSUMPTION}> .
        ?elec_consumption_iri <{IS_A}> ?consumption_iri .
        ?elec_consumption_iri  <{RDF_TYPE}> <{REGION_RESULTED_ELECTRICITY_CONSUMPTION}> .
        ?gas_consumption_iri <{IS_A}> ?consumption_iri .
        ?gas_consumption_iri  <{RDF_TYPE}> <{REGION_RESULTED_GAS_CONSUMPTION}> .
        }}
        """
        res = self.performQuery(query_string)

        if not res:
            consumption_iri = REGION + "ResultedEnergyConsumption_" + str(uuid.uuid4())
            logger.info(f'No existed consumption_iri, created {consumption_iri}')
            elec_consumption_iri = REGION + REGION_RESULTED_ELECTRICITY_CONSUMPTION +"_" + str(uuid.uuid4())
            logger.info(f'No existed elec_consumption_iri, created {elec_consumption_iri}')
            gas_consumption_iri = REGION + REGION_RESULTED_GAS_CONSUMPTION +"_" + str(uuid.uuid4())
            logger.info(f'No existed gas_consumption_iri, created {gas_consumption_iri}')
            deletion = False
        else: 
            res = res[0]
            consumption_iri = str(res["consumption_iri"])
            logger.info(f'consumption_iri: {consumption_iri} will be used')
            elec_consumption_iri = str(res["elec_consumption_iri"])
            logger.info(f'elec_consumption_iri: {elec_consumption_iri} will be used')
            gas_consumption_iri = str(res["gas_consumption_iri"])
            logger.info(f'gas_consumption_iri: {gas_consumption_iri} will be used')
            deletion = True
        

        return consumption_iri, elec_consumption_iri, gas_consumption_iri, deletion
    
    def instantiate_resulted_consumptions(self, g, consumption_iri, elec_consumption_iri, gas_consumption_iri, 
                                          region):
        g.add((URIRef(region),URIRef(REGION_HAS_RESULTED_ENERGY_CONSUMPTION),URIRef(consumption_iri)))
        g.add((URIRef(consumption_iri),URIRef(RDF_TYPE),URIRef(REGION_RESULTED_ENERGYCONSUMPTION)))
        g.add((URIRef(elec_consumption_iri),URIRef(IS_A),URIRef(consumption_iri)))
        g.add((URIRef(elec_consumption_iri),URIRef(RDF_TYPE),URIRef(REGION_RESULTED_ELECTRICITY_CONSUMPTION)))
        g.add((URIRef(gas_consumption_iri),URIRef(IS_A),URIRef(consumption_iri)))
        g.add((URIRef(gas_consumption_iri),URIRef(RDF_TYPE),URIRef(REGION_RESULTED_GAS_CONSUMPTION)))
        
        return g

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
