from pyderivationagent.conf import config_derivation_agent
from pyderivationagent import PySparqlClient
from pyderivationagent import PyDerivationClient
from pyderivationagent.data_model import iris as pda_iris
import time
from utiliycostcalculationagent.datamodel.iris import *
from tqdm import tqdm


# ---------------------------- Configs ------------------------------ #
agent_config = config_derivation_agent(env_file='./agent.env.example')
agentIRI = agent_config.ONTOAGENT_SERVICE_IRI
agentURL = agent_config.ONTOAGENT_OPERATION_HTTP_URL
QUERY_ENDPOINT = agent_config.SPARQL_QUERY_ENDPOINT
UPDATE_ENDPOINT = QUERY_ENDPOINT

# iris
DERIVATION_INSTANCE_BASE_URL = 'https://www.example.com/kg/derivation/'
RDF = 'http://www.w3.org/1999/02/22-rdf-syntax-ns#'
RDF_TYPE =  RDF + 'type'

# ----------------------------- Funcs ------------------------------- #
def Synmarkup(
            derivation_client: PyDerivationClient,
            sparql_client: PySparqlClient,
            unit_rate_iri:str,
            resulted_consumption_iri:str,
            agentIRI : str
        ):
        derivation_iri = retrieve_derivation_iri(sparql_client, resulted_consumption_iri, agentIRI)
        if not derivation_iri :
            input_iris = [resulted_consumption_iri,unit_rate_iri]
            #print(input_iris)
            derivation = derivation_client.createSyncDerivationForNewInfoWithHttpUrl(
                agentIRI=agentIRI,
                agentURL=agentURL,
                inputsIRI=input_iris,
                derivationType=pda_iris.ONTODERIVATION_DERIVATION,
            )
        
        else:
            print(f'InputIRI: {resulted_consumption_iri} already have derivation IRI: {derivation_iri}, skipped for now')

def retrieve_derivation_iri(
          sparql_client: PySparqlClient,
          input_iri:str,
          agentIRI:str
):
        query = f"""
            SELECT DISTINCT ?s
            WHERE {{
                ?s <{pda_iris.ONTODERIVATION_ISDERIVEDFROM}> <{input_iri}>.
                ?s <{pda_iris.ONTODERIVATION_ISDERIVEDUSING}> <{agentIRI}>.
            }}"""
     
        query = ' '.join(query.split())
        response = sparql_client.performQuery(query)
        if len(response) == 0:
            return None
        else:
            syn_derivation_iri = response[0].get('s')
            return syn_derivation_iri

def retrieve_resulted_consumption_iri(sparql_client: PySparqlClient):
        
        query_string = f"""
        SELECT DISTINCT ?resulted_consumption_iri 
        WHERE {{
        ?region <{REGION_HAS_ENERGYCONSUMPTION_PROFILE}> ?resulted_consumption_iri .
        ?resulted_consumption_iri <{RDF_TYPE}> <{REGION_RESULTED_ENERGYCONSUMPTION}> .
        ?elec_consumption_iri <{IS_A}> ?resulted_consumption_iri .
        ?elec_consumption_iri  <{RDF_TYPE}> <{REGION_RESULTED_ELECTRICITY_CONSUMPTION}> .
        ?gas_consumption_iri <{IS_A}> ?resulted_consumption_iri .
        ?gas_consumption_iri  <{RDF_TYPE}> <{REGION_RESULTED_GAS_CONSUMPTION}> . }}
        """
        res = sparql_client.performQuery(query_string)
        
        if not res:
            raise IndexError('No resulted_consumption_iri found -- Are you sure you are using the correct namespace?')
        else:
            resulted_consumption_iri_list = []
            for i in range(len(res)):
                resulted_consumption_iri = res[i]["resulted_consumption_iri"]
                resulted_consumption_iri_list.append(resulted_consumption_iri)

            return resulted_consumption_iri_list

def retrieve_unit_rate_iri(sparql_client: PySparqlClient):
        
        query_string = f"""
        SELECT DISTINCT ?unit_rate_iri
        WHERE {{
         ?country_iri <{ONTOHEATNETWORK_HASUNITRATE}> ?unit_rate_iri .
         ?unit_rate_iri <{RDF_TYPE}> <{ONTOHEATNETWORK_UNITRATE}> ;
        }}
        """
        res = sparql_client.performQuery(query_string)
        if not res:
            raise IndexError('No unit_rate_iri found -- Are you sure you are using the correct namespace?')
        else:
            res = res[0]
            unit_rate_iri = res['unit_rate_iri']

            return unit_rate_iri

# ----------------------------- Tasks ------------------------------- #

# Create a PySparqlClient instance
sparql_client = PySparqlClient(
        query_endpoint=QUERY_ENDPOINT,
        update_endpoint=UPDATE_ENDPOINT,
    )

# retrieve resulted_consumption_iri
resulted_consumption_iri_list = retrieve_resulted_consumption_iri(sparql_client)
print(f"A total number of {len(resulted_consumption_iri_list)} will be marked, meaning there is {len(resulted_consumption_iri_list)} regions will be marked")
unit_rate_iri = retrieve_unit_rate_iri(sparql_client)

# Create a PyDerivationClient instance
derivation_client = PyDerivationClient(derivation_instance_base_url=DERIVATION_INSTANCE_BASE_URL,
                                        query_endpoint=QUERY_ENDPOINT,
                                        update_endpoint=UPDATE_ENDPOINT)

# Perform Syn markup
for i in tqdm(range(len(resulted_consumption_iri_list))):
    resulted_consumption_iri = resulted_consumption_iri_list[i]
    try:
        Synmarkup(
            derivation_client=derivation_client,
            sparql_client = sparql_client,
            unit_rate_iri = unit_rate_iri,
            resulted_consumption_iri = resulted_consumption_iri,
            agentIRI = agentIRI
        )
    except:
            derivation_iri = retrieve_derivation_iri(sparql_client, resulted_consumption_iri, agentIRI)
            if not derivation_iri :
                raise KeyError('something wrong, contact Jieyang to fix this')
            else:
                print(f'InputIRI: {resulted_consumption_iri} already have derivation IRI: {derivation_iri}, skipped for now')

# # Perform unified update
# for i in range(len(inputIRI)):
#     time.sleep(1)
#     bday_iri = inputIRI[i]
#     derivation_iri, entities = retrieve_derivation_iri(sparql_client,
#                                                  bday_iri,
#                                                  agentIRI)
#     # Perform unified update
#     derivation_client.unifiedUpdateDerivation(derivation_iri)