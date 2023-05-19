import uuid
from pyderivationagent.conf import config_derivation_agent
from pyderivationagent import PySparqlClient
from pyderivationagent import PyDerivationClient
from pyderivationagent.data_model import iris as pda_iris
import time
import pandas as pd
from tqdm import tqdm
import numpy as np

from copcalculationagent.datamodel.iris import *

# ---------------------------- Configs ------------------------------ #
agent_config = config_derivation_agent(env_file='./agent.env.example')
agentIRI = agent_config.ONTOAGENT_SERVICE_IRI
agentURL = agent_config.ONTOAGENT_OPERATION_HTTP_URL
QUERY_ENDPOINT = agent_config.SPARQL_QUERY_ENDPOINT
UPDATE_ENDPOINT = QUERY_ENDPOINT
# ----------------------------- Funcs ------------------------------- #
def check_country(sparql_client):
    # check if there is country exist
    query_string = f'''
    SELECT ?country
    {{
    ?country <{RDF_TYPE}> <{ONTOCAPE_COUNTRY}>.
    }}
    '''
    res = sparql_client.performQuery(query_string)

    if not res:
        country_iri = ONTOCAPE + "Country_" + str(uuid.uuid4())
        print(f'No existed country iri, created {country_iri}')
    else: 
        res = res[0]
        country_iri = str(res["country"])
        print(f'country iri: {country_iri} will be used')
    
    return country_iri

def update_country(sparql_client, country_iri):
     query_string = f'''
     INSERT DATA {{
     <{country_iri}> <{RDF_TYPE}> <{ONTOCAPE_COUNTRY}>.
     }}
     '''
     sparql_client.performUpdate(query_string)

def region_within_country_update_template(region, country_iri):
     
     triple = f'''
     <{region}> <{REGION_ISWITHIN}> <{country_iri}>.
     '''

     return triple

def initialize_assumptions(sparql_client):
     
    def get_country_iri(sparql_client):
        # check if there is country exist
        query_string = f'''
        SELECT ?country
        {{
        ?country <{RDF_TYPE}> <{ONTOCAPE_COUNTRY}>.
        }}
        '''
        res = sparql_client.performQuery(query_string)

        if not res:
            raise KeyError("No existed country iri, please go to run 'upper_level_ontology_update.py'")
        else: 
            res = res[0]
            country_iri = str(res["country"])
        
        return country_iri

    def get_assumption_iri(sparql_client, country_iri):
        query_string = f"""
        SELECT ?assumption_iri
        WHERE {{
        ?assumption_iri <{REGION_APPLICABLETO}> <{country_iri}> .
        }}
        """
        res = sparql_client.performQuery(query_string)

        if not res:
            assumption_iri = REGION + "Assumption_" + str(uuid.uuid4())
            print(f'No existed assumption_iri, created {assumption_iri}')
        else: 
            res = res[0]
            assumption_iri = str(res["assumption_iri"])
            print(f'assumption_iri: {assumption_iri} will be used')
        
        return assumption_iri
    
    def get_heatpumpefficiency_iri(sparql_client, assumption_iri):
        query_string = f"""
        SELECT ?heatpumpefficiency_iri
        WHERE {{
        ?heatpumpefficiency_iri <{IS_A}> <{assumption_iri}> ;
                    <{RDF_TYPE}> <{REGION_HEATPUMP_EFFICIENCY}> .
        }}
        """
        res = sparql_client.performQuery(query_string)

        if not res:
            heatpumpefficiency_iri = REGION + "HeatPumpEfficiency_" + str(uuid.uuid4())
            print(f'No existed heatpumpefficiency_iri, created {heatpumpefficiency_iri}')
        else: 
            res = res[0]
            heatpumpefficiency_iri = str(res["heatpumpefficiency_iri"])
            print(f'heatpumpefficiency_iri: {heatpumpefficiency_iri} will be used')
        
        return heatpumpefficiency_iri
    
    def get_hotsidetemperature_iri(sparql_client, assumption_iri):
        query_string = f"""
        SELECT ?hotsidetemperature_iri
        WHERE {{
        ?hotsidetemperature_iri <{IS_A}> <{assumption_iri}> ;
                    <{RDF_TYPE}> <{REGION_HOTSIDE_TEMPERATURE}> .
        }}
        """
        res = sparql_client.performQuery(query_string)

        if not res:
            hotsidetemperature_iri = REGION + "HotSideTemperature_" + str(uuid.uuid4())
            print(f'No existed hotsidetemperature_iri, created {hotsidetemperature_iri}')
        else: 
            res = res[0]
            hotsidetemperature_iri = str(res["hotsidetemperature_iri"])
            print(f'hotsidetemperature_iri: {hotsidetemperature_iri} will be used')
        
        return hotsidetemperature_iri
    
    # Assumptions for COP agent
    country_iri = get_country_iri(sparql_client)
    assumption_iri = get_assumption_iri(sparql_client,country_iri)
    heatpumpefficiency_iri = get_heatpumpefficiency_iri(sparql_client,assumption_iri)
    hotsidetemperature_iri = get_hotsidetemperature_iri(sparql_client,assumption_iri)
    query_string = f"""
    INSERT DATA {{
    <{assumption_iri}> <{REGION_APPLICABLETO}> <{country_iri}> .
    <{heatpumpefficiency_iri}> <{IS_A}> <{assumption_iri}> ;
                <{RDF_TYPE}> <{REGION_HEATPUMP_EFFICIENCY}> .
    <{hotsidetemperature_iri}> <{IS_A}> <{assumption_iri}> ;
                <{RDF_TYPE}> <{REGION_HOTSIDE_TEMPERATURE}> .
    }}
    """
    sparql_client.performUpdate(query_string)
     
# ----------------------------- Tasks ------------------------------- #

# A source file 'LSOA_codes_IRIs.csv' file is needed to get all the IRI for regions
data = pd.read_csv('LSOA_codes_IRIs.csv')

# Access the data in the DataFrame
LSOA_codes = data['LSOA code'].tolist()

# Create a PySparqlClient instance
sparql_client = PySparqlClient(query_endpoint=QUERY_ENDPOINT, update_endpoint=UPDATE_ENDPOINT)

country_iri = check_country(sparql_client)

update_country(sparql_client, country_iri)
# Split the queries into Batches
# Perform SPARQL update query in chunks to avoid heap size/memory issues
total = len(LSOA_codes)
n_compile = total / 10
remainder = total % 10
n_compile = int(n_compile)
len_query = np.zeros(n_compile + 2)
if remainder == 0:
    len_query = np.zeros(n_compile + 1)

for i in range(1, len(len_query) - 1):
        len_query[i] = len_query[i - 1] + 10
        len_query[-1] = len_query[-2] + remainder

for g in tqdm(range(len(len_query) - 1)):

    i = int(len_query[g])
    region = LSOA_codes[i]
    # Initialise update query
    query = f"INSERT DATA" + "{"
    region = LSOA_codes[i]
    triple = region_within_country_update_template(region, country_iri)
    middle_num = int(len_query[g + 1] - len_query[g]) - 2
    for j in range(middle_num):
        region = LSOA_codes[i + j + 1]
        triple += region_within_country_update_template(region, country_iri)

    region = LSOA_codes[int(len_query[g + 1]) - 1]
    triple += region_within_country_update_template(region, country_iri)
    query +=triple + "}"
    sparql_client.performUpdate(query)


