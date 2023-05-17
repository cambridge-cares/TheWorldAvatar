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


