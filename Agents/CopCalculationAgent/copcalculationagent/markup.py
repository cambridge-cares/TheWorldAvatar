from pyderivationagent.conf import config_derivation_agent
from pyderivationagent import PySparqlClient
from pyderivationagent import PyDerivationClient
from pyderivationagent.data_model import iris as pda_iris
import time
from copcalculationagent.datamodel.iris import *


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
    temperature_iri: str,
    heatpumpefficiency_iri: str,
    hotsidetemperature_iri: str,
    agentIRI,
    agentURL
):
        input_iris = [temperature_iri, heatpumpefficiency_iri, hotsidetemperature_iri]
        derivation = derivation_client.createSyncDerivationForNewInfoWithHttpUrl(
            agentIRI=agentIRI,
            agentURL=agentURL,
            inputsIRI=input_iris,
            derivationType=pda_iris.ONTODERIVATION_DERIVATION,
        )

def retrieve_derivation_iri(
          sparql_client: PySparqlClient,
          input_iri:str,
          agentIRI:str
):
        query = f"""
            SELECT DISTINCT ?s ?entities
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
            entities = [response[0].get('entities'),response[1].get('entities')]
            return syn_derivation_iri, entities

def retrieve_temperature_iri(sparql_client: PySparqlClient):
        
        query_string = f"""
        SELECT ?temperature_iri
        WHERE {{?region <{CLIMB_HASMEASURE}>  ?m.
                ?m <{COMP_HAS_STARTUTC}> ?start;
                    <{COMP_HAS_ENDUTC}> ?end ;
                    <{CLIMB_HASVAR}> "{CLIMA_TAS}"^^<{XSD_STRING}> .
                ?p <{OM_HAS_PHENO}> ?m.
                ?p <{OM_HAS_VALUE}> ?temperature_iri.
        ?temperature_iri <{OM_HAS_NUMERICALVALUE}> ?meantemperature.}}
        """

        res = sparql_client.performQuery(query_string)
        
        if not res:
            raise IndexError('No temperature_iri found -- Are you sure you are using the correct namespace?')
        else:
            temperature_iri_list = [d['temperature_iri'] for d in res]

            return temperature_iri_list

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
temperature_iri_list = retrieve_temperature_iri(sparql_client)
heatpumpefficiency_iri = retrieve_heatpumpefficiency_iri(sparql_client)
hotsidetemperature_iri = retrieve_hotsidetemperature_iri(sparql_client)

# Create a PyDerivationClient instance
derivation_client = PyDerivationClient(derivation_instance_base_url=DERIVATION_INSTANCE_BASE_URL,
                                        query_endpoint=QUERY_ENDPOINT,
                                        update_endpoint=UPDATE_ENDPOINT)

# Perform Syn markup
for i in range(len(temperature_iri_list)):
    time.sleep(1)
    temperature_iri = temperature_iri_list[i]
    Synmarkup(
        derivation_client=derivation_client,
        temperature_iri=temperature_iri,
        heatpumpefficiency_iri = heatpumpefficiency_iri,
        hotsidetemperature_iri = hotsidetemperature_iri,
        agentIRI = agentIRI,
        agentURL = agentURL
    )

# # Perform unified update
# for i in range(len(inputIRI)):
#     time.sleep(1)
#     bday_iri = inputIRI[i]
#     derivation_iri, entities = retrieve_derivation_iri(sparql_client,
#                                                  bday_iri,
#                                                  agentIRI)
#     # Perform unified update
#     derivation_client.unifiedUpdateDerivation(derivation_iri)