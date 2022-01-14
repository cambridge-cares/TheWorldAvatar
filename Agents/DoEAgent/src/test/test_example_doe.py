from testcontainers.core.container import DockerContainer
import pytest
import time
from conf import *
from doe_agent import *


# fixture: spin up triple store
# test: query if the generated exp refers to experiment

# Hardcode the IRI to be used for the example, these should be identical with the ones specified in '/test/resources/doe.txt'
derivation_output = ['https://theworldavatar.com/kb/ontodoe/DoE_1/NewExperiment_1']
derivation_inputs = ['https://theworldavatar.com/kb/ontodoe/DoE_1/Strategy_1',
                    'https://theworldavatar.com/kb/ontodoe/DoE_1/Domain_1',
                    'https://theworldavatar.com/kb/ontodoe/DoE_1/SystemResponse_1',
                    'https://theworldavatar.com/kb/ontodoe/DoE_1/SystemResponse_2',
                    'https://theworldavatar.com/kb/ontodoe/DoE_1/HistoricalData_1']
design_of_experiment_iri = 'https://theworldavatar.com/kb/ontodoe/DoE_1/DoE_1'

@pytest.fixture()
def initialise_triple_store():
    """
        As the monitorDerivation() is set to be running periodically once the DoE agent is deployed, 
        this page should serve as the entry point for creating a working example once the developer 
        has uploaded the correct triples to the knowledge graph endpoints, 
        i.e. it creates the derivation instance based on the example data and execute asynchronous 
        derivation update automatically.

        Response:
            the created OntoDerivation:Derivation instance
    """
    # Define temporary Docker container based on empty Blazegraph image from CMCL registry
    blazegraph = DockerContainer('docker.cmclinnovations.com/blazegraph_for_tests:1.0.0')
    blazegraph.with_exposed_ports(9999)
    yield blazegraph

def test_example_doe():
    # Spin up temporary docker container
    with initialise_triple_store as container:
        # Wait some arbitrary time until container is reachable
        time.sleep(3)
        # Retrieve SPARQL endpoint
        endpoint = get_endpoint(container)

        # Create SparqlClient for testing
        sparql_client = SparqlClient(endpoint, endpoint)

        # Verify that knowledge base is empty
        res = sparql_client.getAmountOfTriples()
        assert res == 0
    
    # Initialise configuration
    config = DoEAgentConfig(str(Path(__file__).absolute().parent) + '/test_conf.json')
    
    # Initialise Flask app
    flask_app = Flask(__name__)
    # Initialise DoE agent with temporary docker container endpoint
    doe_agent = DoEAgent(flask_app, config.ONTOAGENT_SERVICE, config.PERIODIC_TIMESCALE, config.DERIVATION_INSTANCE_BASE_URL, endpoint)

    # # Initialise derivationClient with SPARQL Query and Update endpoints
    # storeClient = jpsBaseLib_view.RemoteStoreClient(SPARQL_QUERY_ENDPOINT, SPARQL_UPDATE_ENDPOINT)
    # derivationClient = jpsBaseLib_view.DerivationClient(storeClient, DERIVATION_INSTANCE_BASE_URL)
    
    # Upload all example triples to triple store
    folderpath = str(Path(__file__).absolute().parent) + '/resources/'
    for f in ['doe.ttl', 'Service__DoE.ttl', 'rxn_data.ttl']:
        with open(folderpath + f, 'r') as file:
            data = file.read()
            sparql_client.performUpdate(data)

    # Create derivation instance given above information, the timestamp of this derivation is 0
    derivation_iri = doe_agent.derivationClient.createAsynDerivation(derivation_output, config.ONTOAGENT_SERVICE, derivation_inputs)

    # Check if the derivation instance is created correctly
    assert sparql_client.checkInstanceClass(derivation_iri, ONTODERIVATION_DERIVATIONASYN)

    # Iterate over the list of inputs to add and update the timestamp
    for input in derivation_inputs:
        doe_agent.derivationClient.addTimeInstance(input)
        # Update timestamp is needed as the timestamp added using addTimeInstance() is 0
        doe_agent.derivationClient.updateTimestamp(input)

    # Update the asynchronous derivation, it will be marked as "PendingUpdate"
    # The actual update will be handled by monitorDerivation method periodically run by DoE agent
    doe_agent.derivationClient.updateDerivationAsyn(derivation_iri)

    currentTimestamp_derivation = 0
    while currentTimestamp_derivation == 0:
        time.sleep(20)
        currentTimestamp_derivation = 00

    new_exp_iri = sparql_client.getNewExperimentFromDoE(design_of_experiment_iri)

    # Check the new generated instance NewExperiment is different from the original one provided in the example
    assert new_exp_iri != derivation_output[0]

def get_endpoint(docker_container):
    # Retrieve SPARQL endpoint for temporary testcontainer
    # endpoint acts as both Query and Update endpoint
    endpoint = 'http://' + docker_container.get_container_host_ip().replace('localnpipe', 'localhost') + ':' \
               + docker_container.get_exposed_port(9999)
    # 'kb' is default namespace in Blazegraph
    endpoint += '/blazegraph/namespace/kb/sparql'
    return endpoint


# @pytest.fixture()
# def get_sample_gasflow_history():
#     # Create sample test data as returned by get_gasflow_history() from triple store and postgres database
#     # FORMAT: {terminal name: [terminal IRI, measurement IRI, Java time series object], ...}

#     ts_data = {}

#     # Create sample data of length n
#     n = 4
#     terminal_name = 'test_terminal'
#     terminal_iri = 'test_terminal_iri'
#     data_iri = 'test_data_iri'
#     values = [[1.0 for j in (range(n))]]
#     t = jpsBaseLibView.java.time.Instant.now()
#     times = []
#     for k in range(n):
#         times.append(t.plusSeconds(k))

#     # Construct time series object from sample data
#     ts = jpsBaseLibView.TimeSeries(times, [data_iri], values)
#     # Add to dictionary
#     ts_data[terminal_name] = [terminal_iri, data_iri, ts]

#     return ts_data

# def test_onSuccess(get_sample_gasflow_history):

#     # Retrieve converted data
#     converted_data = term_out.onSuccess(get_sample_gasflow_history)

#     # Remove trailing [] and split individual entries
#     conv = converted_data.strip('[]')
#     conv = conv.replace('}, {', '},  {')
#     conv = conv.split(',  ')

#     for c in range(len(conv)):

#         assert conv[c].startswith('{"s": "test_terminal_iri", "UTC": "')
#         assert conv[c].endswith('", "num_val": "1.0", "label": "test_terminal"}')

#         s = len('{"s": "test_terminal_iri", "UTC": "')
#         e = len('", "num_val": "1.0", "label": "test_terminal"}')
#         assert len(conv[c]) == s + e + len('2021-09-24T14:55:15.000Z')

#         date = conv[c][s : s+len('2021-09-24T14:55:15.000Z')]
#         regexp = re.compile(r'[0-9]{4}-[0-9]{2}-[0-9]{2}T[0-9]{2}:[0-9]{2}:[0-9]{2}.[0-9]{3}Z')
#         assert bool(regexp.search(date))

# def exampleEntryPoint():
#     """
#         As the monitorDerivation() is set to be running periodically once the DoE agent is deployed, 
#         this page should serve as the entry point for creating a working example once the developer 
#         has uploaded the correct triples to the knowledge graph endpoints, 
#         i.e. it creates the derivation instance based on the example data and execute asynchronous 
#         derivation update automatically.

#         Response:
#             the created OntoDerivation:Derivation instance
#     """
#     # Initialise derivationClient with SPARQL Query and Update endpoints
#     storeClient = jpsBaseLib_view.RemoteStoreClient(SPARQL_QUERY_ENDPOINT, SPARQL_UPDATE_ENDPOINT)
#     derivationClient = jpsBaseLib_view.DerivationClient(storeClient, 'https://www.example.com/triplestore/repository/')

#     clearAll = """DELETE {?s ?p ?o} \
#                WHERE {?s ?p ?o}
#                """

#     sparql_client = SparqlClient(
#             TRIPLE_STORE_UPLOAD_SERVER, TRIPLE_STORE_UPLOAD_REPOSITORY,
#             SPARQL_QUERY_ENDPOINT, SPARQL_UPDATE_ENDPOINT, KG_USERNAME, KG_PASSWORD
#         )
    
#     sparql_client.performUpdate(clearAll)

#     filepath = os.getcwd() + '/test/resources/' #'/Agents/DoEAgent/summit_agent/resources/'
#     for f in ['doe.ttl', 'Service__DoE.ttl', 'rxn_data.ttl']:
#         with open(filepath+f, 'r') as file:
#             data = file.read()
#             sparql_client.performUpdate(data)

#     # Hardcode the IRI to be used for the example
#     # Developers should upload the files containing these triples to the endpoints following the instructions in the README.md
#     derived = ['https://theworldavatar.com/kb/ontodoe/DoE_1/NewExperiment_1']
#     agentIRI = DOEAGENT_ONTOAGENT_SERVICE
#     inputs = ['https://theworldavatar.com/kb/ontodoe/DoE_1/Strategy_1', 
#     'https://theworldavatar.com/kb/ontodoe/DoE_1/Domain_1', 
#     'https://theworldavatar.com/kb/ontodoe/DoE_1/SystemResponse_1', 
#     'https://theworldavatar.com/kb/ontodoe/DoE_1/SystemResponse_2', 
#     'https://theworldavatar.com/kb/ontodoe/DoE_1/HistoricalData_1']

#     # Create derivation instance given above information, the timestamp of this derivation is 0
#     derivationIRI = derivationClient.createAsynDerivation(derived, agentIRI, inputs)
#     # logger.info(f'Initialised successfully, created derivation instance <{derivationIRI}>')
#     msg = f'Initialised successfully, created derivation instance: {derivationIRI}'

#     # Iterate over the list of inputs to add and update the timestamp
#     for input in inputs:
#         derivationClient.addTimeInstance(input)
#         # Update timestamp is needed as the timestamp added using addTimeInstance() is 0
#         derivationClient.updateTimestamp(input)

#     # Update the derivation asynchronous, it will only mark as "Requested"
#     # The actual update will be handled by monitorDerivation method periodically run by DoE agent
#     derivationClient.updateDerivationAsyn(derivationIRI)
#     return msg


# app.add_url_pattern('/example', 'example', exampleEntryPoint, methods=['GET'])





# doe_agent_config = DoEAgentConfig(str(Path(__file__).absolute().parent) + '/conf/doeagent_properties.json')

# app = DoEAgent(flask_app, doe_agent_config.ONTOAGENT_SERVICE, doe_agent_config.PERIODIC_TIMESCALE, doe_agent_config.DERIVATION_INSTANCE_BASE_URL, doe_agent_config.SPARQL_QUERY_ENDPOINT)
# app.add_url_pattern('/', 'root', default, methods=['GET'])
# app.add_url_pattern('/example', 'example', exampleEntryPoint, methods=['GET'])
