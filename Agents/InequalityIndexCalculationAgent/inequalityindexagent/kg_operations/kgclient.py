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
from inequalityindexagent.utils.stack_configs import MIN_FP, MAX_FP, ONTOP_URL

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
    
    # -------------------- Get iris ----------------- #
    def get_elec_cost_gas_cost_iri(self, utility_cost_iri, status):
        query_string = f"""
        SELECT ?elec_cost_iri  ?gas_cost_iri
        WHERE {{
        ?region <{ONTOCAPE_HASUTILITYCOST}> <{utility_cost_iri}> .
        <{utility_cost_iri}> <{RDF_TYPE}> <{ONTOCAPE_UTILITYCOST}> ;
                            <{REGION_HASSTATUS}> {status} .
        ?elec_cost_iri <{IS_A}> <{utility_cost_iri}> ;
                       <{RDF_TYPE}> <{ONTOCAPE_ELECTRICITYCOSTS}> .
        ?fuel_cost_iri <{IS_A}> <{utility_cost_iri}>  ;
                       <{RDF_TYPE}> <{ONTOCAPE_FUELCOSTS}> .
        ?gas_cost_iri <{IS_A}> ?fuel_cost_iri ;
                      <{RDF_TYPE}> <{REGION_GASCOSTS}> .
        }}
        """
        query_string = self.remove_unnecessary_whitespace(query_string)
        res = self.performQuery(query_string)
        if not res:
            # In case some vars are missing (i.e. empty SPARQL result), return Nones
            logger.error("utility_cost_iri can not be found -- go check if it exist")
    
        else:
            res = res[0]
            if res["elec_cost_iri"] == "" or res["elec_cost_iri"] == None:
                logger.error(f'No existed elec_cost_iri')
            else:
                elec_cost_iri = str(res["elec_cost_iri"])

            if res["gas_cost_iri"] == "" or res["gas_cost_iri"] == None:
                logger.error(f'No existed gas_cost_iri')
            else:
                gas_cost_iri = str(res["gas_cost_iri"])
            
        return elec_cost_iri, gas_cost_iri

    def get_all_utility_cost_iris(self):

        query_string = f"""
        SELECT ?before_utility_cost_iri ?after_utility_cost_iri
        WHERE {{
        ?region <{ONTOCAPE_HASUTILITYCOST}> ?before_utility_cost_iri;
                <{ONTOCAPE_HASUTILITYCOST}> ?after_utility_cost_iri.

        ?before_utility_cost_iri <{RDF_TYPE}> <{ONTOCAPE_UTILITYCOST}> ;
                            <{REGION_HASSTATUS}> "Before" .
        ?after_utility_cost_iri <{RDF_TYPE}> <{ONTOCAPE_UTILITYCOST}> ;
                            <{REGION_HASSTATUS}> "After" .
        }}
        """
        query_string = self.remove_unnecessary_whitespace(query_string)
        res = self.performQuery(query_string)
        if not res:
            # In case some vars are missing (i.e. empty SPARQL result), return Nones
            logger.error("utility_cost_iri can not be found -- go check if it exist")
        
        else:
            before_utility_cost_iris_list = []
            after_utility_cost_iris_list = []
            for i in range(len(res)):
                before_utility_cost_iris_list.append(res[i]['before_utility_cost_iri'])
                after_utility_cost_iris_list.append(res[i]['after_utility_cost_iri'])
            
            return before_utility_cost_iris_list, after_utility_cost_iris_list

    def generate_inequality_index_iri(self, region):
        
        query_string = f"""
        SELECT ?inequalityindex_iri
        WHERE {{
        <{region}> <{REGION_HAS_INEQUALITY_INDEX}> ?inequalityindex_iri .
        ?utility_cost_iri  <{RDF_TYPE}> <{REGION_INEQUALITYINDEX}> .
        }}
        """
        res = self.performQuery(query_string)

        if not res:
            inequalityindex_iri = REGION + "InequalityIndex_" + str(uuid.uuid4())
            logger.info(f'No existed inequalityindex_iri, created {inequalityindex_iri}')

        else: 
            res = res[0]
            inequalityindex_iri = str(res["inequalityindex_iri"])
            logger.info(f'inequalityindex_iri: {inequalityindex_iri} will be used')
    
        return inequalityindex_iri

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
                max_fp = float(res['max_fp'])  
            except:
                logger.error("max_fp result can not be retrieved -- go check if it exist")
                raise InvalidInput("max_fp result can not be retrieved -- go check if it exist")

        return min_fp, max_fp
    
    def get_status_of_utility_cost(self, utility_cost_iri):
        query_string = f"""
        SELECT ?status 
        WHERE {{
        ?region <{ONTOCAPE_HASUTILITYCOST}> <{utility_cost_iri}> .
        <{utility_cost_iri}>  <{RDF_TYPE}> <{ONTOCAPE_UTILITYCOST}> ;
                            <{REGION_HASSTATUS}> ?status .
        }}
        """
        query_string = self.remove_unnecessary_whitespace(query_string)
        res = self.performQuery(query_string)
        if not res:
            # In case some vars are missing (i.e. empty SPARQL result), return Nones
            logger.error("utility_cost_iri can not be found -- go check if it exist")
        else:
            res = res[0]
            try:
                res['status'] = str(res['status'])
            except:
                logger.error("utility_cost_iri can not be found -- go check if it exist")

        return res[0]['status'] 
    
    def get_fuel_poverty_proportion(self, household_iri):
        ontop_url = ONTOP_URL
        service_expression = f'SERVICE <{ontop_url}> {{ '
        query_string = f"""
        PREFIX xsd: <http://www.w3.org/2001/XMLSchema#>
        SELECT DISTINCT ?s (xsd:float(?a)/xsd:float(?num) AS ?result) ?num 
        WHERE {{
        {service_expression} 
        ?s   <{RDF_TYPE}> <{ONS_DEF_STAT}>;  
            <{OFP_HASHOUSEHOLD}> <{household_iri}>.
        <{household_iri}> <{OFP_FUELPOOR_HOUSEHOLD}> ?a;
             <{OFP_HOUSHOLD}> ?num.
        """
        query_string = self.remove_unnecessary_whitespace(query_string)
        res = self.performQuery(query_string)
        if not res:
            logger.warning("Fuel Poverty result can not be found -- go check if you uploaded csv files/ or ontop.obda file missing?")
            print("Fuel Poverty result can not be found -- go check if you uploaded csv files/ or ontop.obda file missing?")
            fp = 0
        else:
            res = res[0]
            try:
                fp = float(res['result'])
            except:
                fp = 0
            try:
                region = float(res['s'])
            except:
                region = None

        return fp, region
        
    def instantiate_inequality_index(self, g, inequalityindex_iri, inequality_index, region):
        
        g.add((URIRef(region),URIRef(REGION_HAS_INEQUALITY_INDEX),URIRef(inequalityindex_iri)))
        g.add((URIRef(inequalityindex_iri),URIRef(RDF_TYPE),URIRef(REGION_INEQUALITYINDEX)))
        g.add((URIRef(inequalityindex_iri),URIRef(OM_HAS_NUMERICALVALUE),URIRef(inequality_index)))
    
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
    
