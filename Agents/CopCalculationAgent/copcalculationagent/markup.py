from pyderivationagent.conf import config_derivation_agent
from pyderivationagent import PySparqlClient
from pyderivationagent import PyDerivationClient
from pyderivationagent.data_model import iris as pda_iris
import time
from copcalculationagent.datamodel.iris import *
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
    sparql_client:PySparqlClient,
    region_iri: str,
    heatpumpefficiency_iri: str,
    hotsidetemperature_iri: str,
    agentIRI,
    agentURL
):
        derivation_iri = retrieve_derivation_iri(sparql_client, region_iri, agentIRI)
        #derivation_iri = None
        if not derivation_iri :
            input_iris = [region_iri, heatpumpefficiency_iri, hotsidetemperature_iri]
            derivation = derivation_client.createSyncDerivationForNewInfoWithHttpUrl(
                agentIRI=agentIRI,
                agentURL=agentURL,
                inputsIRI=input_iris,
                derivationType=pda_iris.ONTODERIVATION_DERIVATION,
            )
        
        else:
              print(f'InputIRI: {region_iri} already have derivation IRI: {derivation_iri}, skipped for now')

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
                ?entities <{pda_iris.ONTODERIVATION_BELONGSTO}> ?s.
            }}"""
     
        query = ' '.join(query.split())
        response = sparql_client.performQuery(query)
        if len(response) == 0:
            return None
        else:
            syn_derivation_iri = response[0].get('s')
            return syn_derivation_iri

def retrieve_region_iri(sparql_client: PySparqlClient):
        
        query_string = f"""
        SELECT DISTINCT ?region 
        WHERE {{?region <{CLIMB_HASMEASURE}>  ?temperature_iri.
                ?temperature_iri <{COMP_HAS_STARTUTC}> ?start;
                    <{COMP_HAS_ENDUTC}> ?end ;
                    <{CLIMB_HASVAR}> "{CLIMA_TAS}"^^<{XSD_STRING}> ;
                    <{OM_HAS_NUMERICALVALUE}> ?meantemperature.}}
        """

        res = sparql_client.performQuery(query_string)
        
        if not res:
            raise IndexError('No temperature_iri found -- Are you sure you are using the correct namespace?')
        else:
            region_iri_list = [d['region'] for d in res]

            return region_iri_list

def retrieve_heatpumpefficiency_iri(sparql_client: PySparqlClient):
        
        query_string = f"""
        SELECT ?heatpumpefficiency_iri
        WHERE {{
        ?heatpumpefficiency_iri <{IS_A}> ?assumption_iri ;
                    <{RDF_TYPE}> <{REGION_HEATPUMP_EFFICIENCY}> .
        }}
        """
        res = sparql_client.performQuery(query_string)
        if not res:
            raise IndexError('No heatpumpefficiency_iri found -- Are you sure you are using the correct namespace?')
        else:
            res = res[0]
            heatpumpefficiency_iri = res['heatpumpefficiency_iri']

            return heatpumpefficiency_iri
      
def retrieve_hotsidetemperature_iri(sparql_client: PySparqlClient):
        
        query_string = f"""
        SELECT ?hotsidetemperature_iri
        WHERE {{
        ?hotsidetemperature_iri <{IS_A}> ?assumption_iri ;
                    <{RDF_TYPE}> <{REGION_HOTSIDE_TEMPERATURE}> .
        }}
        """
        res = sparql_client.performQuery(query_string)
        if not res:
            raise IndexError('No hotsidetemperature_iri found -- Are you sure you are using the correct namespace?')
        else:
            res = res[0]
            hotsidetemperature_iri = res['hotsidetemperature_iri']

            return hotsidetemperature_iri
# ----------------------------- Tasks ------------------------------- #

# Create a PySparqlClient instance
sparql_client = PySparqlClient(
        query_endpoint=QUERY_ENDPOINT,
        update_endpoint=UPDATE_ENDPOINT,
    )

# retrieve temperature_iri
region_iri_list = retrieve_region_iri(sparql_client)
#region_iri_list = ["http://statistics.data.gov.uk/id/statistical-geography/Test_000002"]

print(f"A total number of {len(region_iri_list)*12} instances will be marked, meaning there is {len(region_iri_list)} regions will be marked")

heatpumpefficiency_iri = retrieve_heatpumpefficiency_iri(sparql_client)
hotsidetemperature_iri = retrieve_hotsidetemperature_iri(sparql_client)

# Create a PyDerivationClient instance
derivation_client = PyDerivationClient(derivation_instance_base_url=DERIVATION_INSTANCE_BASE_URL,
                                        query_endpoint=QUERY_ENDPOINT,
                                        update_endpoint=UPDATE_ENDPOINT)

# Perform Syn markup
# for i in tqdm(range(len(region_iri_list))):

#     region_iri = region_iri_list[i]
#     try:
#         Synmarkup(
#             derivation_client=derivation_client,
#             sparql_client = sparql_client,
#             region_iri=region_iri,
#             heatpumpefficiency_iri = heatpumpefficiency_iri,
#             hotsidetemperature_iri = hotsidetemperature_iri,
#             agentIRI = agentIRI,
#             agentURL = agentURL
#         )
#     except:
#          derivation_iri = retrieve_derivation_iri(sparql_client,region_iri, agentIRI)
#          if not derivation_iri :
#               raise KeyError('something wrong, contact Jieyang to fix this')
#          else:
            #   print(f'False: InputIRI: {region_iri} already have derivation IRI: {derivation_iri}, skipped for now')

# Update Timestamp
derivation_client.updateTimestamps([heatpumpefficiency_iri])

# Perform unified update
derivation_iri_list = []
for i in range(len(region_iri_list)):
    region_iri = region_iri_list[i]
    derivation_iri = retrieve_derivation_iri(sparql_client,
                                                region_iri,
                                                agentIRI)
    derivation_iri_list.append(derivation_iri)
#     # Perform unified update
derivation_client.updatePureSyncDerivations(derivation_iri_list)