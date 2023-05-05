from toyagent.agent import AgeAgent
from pyderivationagent.conf import config_derivation_agent
from toyagent.utils.stack_configs import QUERY_ENDPOINT, UPDATE_ENDPOINT
from pyderivationagent import PySparqlClient
from pyderivationagent import PyDerivationClient
from pyderivationagent.data_model import iris as pda_iris

# ---------------------------- Configs ------------------------------ #
agent_config = config_derivation_agent(env_file='./agent.env.example')
agentIRI = agent_config.ONTOAGENT_SERVICE_IRI
agentURL = agent_config.ONTOAGENT_OPERATION_HTTP_URL
DERIVATION_INSTANCE_BASE_URL = 'https://www.example.com/kg/derivation/'
inputIRI = ['http://example.org/birthday#Birthday_robot_1',
            'http://example.org/birthday#Birthday_robot_2',
            'http://example.org/birthday#Birthday_robot_3',
            'http://example.org/birthday#Birthday_robot_4',
            'http://example.org/birthday#Birthday_robot_5'
            ]

# ----------------------------- Funcs ------------------------------- #
def markup(
    derivation_client: PyDerivationClient,
    bday_iri: str
):
        # Create sync derivation for new info to get property value estimation computed
        inputsIRI = [bday_iri]
        derivation = derivation_client.createSyncDerivationForNewInfoWithHttpUrl(
            agentIRI=agentIRI,
            agentURL=agentURL,
            inputsIRI=inputsIRI,
            derivationType=pda_iris.ONTODERIVATION_DERIVATION,
        )

# ----------------------------- Tasks ------------------------------- #
# Create a PySparqlClient instance
sparql_client = PySparqlClient( query_endpoint=QUERY_ENDPOINT,
                                update_endpoint=UPDATE_ENDPOINT)

# Create a PyDerivationClient instance
derivation_client = PyDerivationClient(derivation_instance_base_url=DERIVATION_INSTANCE_BASE_URL,
                                        query_endpoint=QUERY_ENDPOINT,
                                        update_endpoint=UPDATE_ENDPOINT)

for i in range(len(inputIRI)):
    bday_iri = inputIRI[i]
    markup(
        derivation_client=derivation_client,
        sparql_client=sparql_client,
        bday_iri=bday_iri
    )