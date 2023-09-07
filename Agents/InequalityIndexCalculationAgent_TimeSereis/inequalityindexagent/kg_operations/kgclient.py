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
from inequalityindexagent.utils.stack_configs import MIN_FP, MAX_FP

from inequalityindexagent.datamodel.iris import *
from inequalityindexagent.errorhandling.exceptions import *

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
    
    # ----------- Verify min_max_fp iri --------------- #
    def get_min_max_fp_iri(self, assumption_iri):
        query_string = f"""
        SELECT ?min_fp_iri ?max_fp_iri
        WHERE {{
         ?min_fp_iri <{IS_A}> <{assumption_iri}> ;
                          <{RDF_TYPE}>  <{REGION_MIN_FP}>.
         ?max_fp_iri <{IS_A}> <{assumption_iri}>;
                          <{RDF_TYPE}>  <{REGION_MAX_FP}>.
        }}
        """
        res = self.performQuery(query_string)

        if not res:
            min_fp_iri = REGION + "MinimalFuelPoverty_" + str(uuid.uuid4())
            logger.info(f'No existed min_fp_iri, created {min_fp_iri}')
            max_fp_iri = REGION + "MaximalFuelPoverty_" + str(uuid.uuid4())
            logger.info(f'No existed max_fp_iri, created {max_fp_iri}')
        else: 
            res = res[0]
            if res["min_fp_iri"] == "" or res["min_fp_iri"] == None:
                min_fp_iri = REGION + "MinimalFuelPoverty_" + str(uuid.uuid4())
                logger.info(f'No existed min_fp_iri, created {min_fp_iri}')
            else:
                min_fp_iri = str(res["min_fp_iri"])
                logger.info(f'min_fp_iri: {min_fp_iri} will be used')

            if res["max_fp_iri"] == "" or res["max_fp_iri"] == None:
                max_fp_iri = REGION + "MaximalFuelPoverty_" + str(uuid.uuid4())
                logger.info(f'No existed max_fp_iri, created {max_fp_iri}')
            else:
                max_fp_iri = str(res["max_fp_iri"])
                logger.info(f'max_fp_iri: {max_fp_iri} will be used')

        return min_fp_iri, max_fp_iri
    
    def update_min_max_fp_iri(self):

        country_iri = self.get_country_iri()
        assumption_iri = self.get_assumption_iri(country_iri)
        min_fp_iri, max_fp_iri = self.get_min_max_fp_iri(assumption_iri)
        
        query_string = f"""
        INSERT DATA {{
         ?min_fp_iri <{IS_A}> <{assumption_iri}> ;
                          <{RDF_TYPE}>  <{REGION_MIN_FP}> ;
                          <{OM_HAS_NUMERICALVALUE}> "{MIN_FP}"^^<{XSD_FLOAT}> .
         ?max_fp_iri <{IS_A}> <{assumption_iri}>;
                          <{RDF_TYPE}>  <{REGION_MAX_FP}> ;
                          <{OM_HAS_NUMERICALVALUE}> "{MAX_FP}"^^<{XSD_FLOAT}> .
        }}
        """
        res = self.performUpdate(query_string)
    
    # ----------- Get values based on iris ---------- #
    def get_min_max_fp(self, min_fp_iri, max_fp_iri):
        query_string = f"""
        SELECT ?min_fp ?max_fp
        WHERE {{
         <{min_fp_iri}> <{IS_A}> ?assumption_iri ;
                          <{RDF_TYPE}>  <{REGION_MIN_FP}> ;
                          <{OM_HAS_NUMERICALVALUE}> ?min_fp .
         <{max_fp_iri}> <{IS_A}> ?assumption_iri ;
                          <{RDF_TYPE}>  <{REGION_MAX_FP}> ;
                          <{OM_HAS_NUMERICALVALUE}> ?max_fp .
        }}

        """
        res = self.performQuery(query_string)
        if not res:
            logger.error("min_max_fp result can not be found -- go check if it exist")
            raise InvalidInput("min_max_fp result can not be found -- go check if it exist")
        else:
            res = res[0]
            try:
                min_fp = float(res['min_fp'])  
            except:
                logger.error("min_fp result can not be retrieved -- go check if it exist")
                raise InvalidInput("min_fp result can not be retrieved -- go check if it exist")
            try:
                max_fp_iri = float(res['max_fp_iri'])  
            except:
                logger.error("max_fp_iri result can not be retrieved -- go check if it exist")
                raise InvalidInput("max_fp_iri result can not be retrieved -- go check if it exist")

        return min_fp_iri, max_fp_iri
    
    def get_utility_cost_iri(self, resulted_consumption_iri):
        query_string = f"""
        SELECT ?region ?elec_consumption_iri ?gas_consumption_iri
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
    
