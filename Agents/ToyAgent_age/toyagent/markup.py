from pyderivationagent.conf import config_derivation_agent
from pyderivationagent import PySparqlClient
from pyderivationagent import PyDerivationClient
from pyderivationagent.data_model import iris as pda_iris
import time


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
EX = 'http://example.org/people#'
EX_BIRTHDAY = EX + 'Birthday'

# ----------------------------- Funcs ------------------------------- #
def Synmarkup(
    derivation_client: PyDerivationClient,
    bday_iri: str,
    agentIRI,
    agentURL
):
        derivation = derivation_client.createSyncDerivationForNewInfoWithHttpUrl(
            agentIRI=agentIRI,
            agentURL=agentURL,
            inputsIRI=[bday_iri],
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
        
def Asymarkup(
        derivation_client: PyDerivationClient,
        input_iri
):
        asyn_derivation_iri = derivation_client.createAsyncDerivationForNewInfo(
              agentIRI,
              [input_iri]
        )


# ----------------------------- Tasks ------------------------------- #

# Define the input
inputIRI = ['http://example.org/birthday#Birthday_R100001',
            'http://example.org/birthday#Birthday_R100002',
            'http://example.org/birthday#Birthday_R100003',
            'http://example.org/birthday#Birthday_R100004',
            'http://example.org/birthday#Birthday_R100005']

# Create a PySparqlClient instance
sparql_client = PySparqlClient(
        query_endpoint=QUERY_ENDPOINT,
        update_endpoint=UPDATE_ENDPOINT,
    )

# Back up the IRI in the Blazegraph
for i in range(len(inputIRI)):
    update_query = f'''
    INSERT DATA {{
    <{inputIRI[i]}> <{RDF_TYPE}> <{EX_BIRTHDAY}>.
    }}
    '''
    sparql_client.performUpdate(update_query)

# Create a PyDerivationClient instance
derivation_client = PyDerivationClient(derivation_instance_base_url=DERIVATION_INSTANCE_BASE_URL,
                                        query_endpoint=QUERY_ENDPOINT,
                                        update_endpoint=UPDATE_ENDPOINT)

# # Perform Syn markup
# for i in range(len(inputIRI)):
#     time.sleep(1)
#     bday_iri = inputIRI[i]
#     Synmarkup(
#         derivation_client=derivation_client,
#         bday_iri=bday_iri,
#         agentIRI = agentIRI,
#         agentURL = agentURL
#     )

# Perform Asy markup
for i in range(len(inputIRI)):
    time.sleep(1)
    bday_iri = inputIRI[i]
    derivation_iri, entities = retrieve_derivation_iri(sparql_client,
                                                 bday_iri,
                                                 agentIRI)
    # Asymarkup(derivation_client,
    #           bday_iri)
    derivation_client.unifiedUpdateDerivation(derivation_iri)