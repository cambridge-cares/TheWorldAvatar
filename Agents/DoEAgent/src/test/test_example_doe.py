from testcontainers.core.container import DockerContainer
import logging
import pytest
import time

from src.agent import *
from src.conf import *

logging.getLogger("py4j").setLevel(logging.INFO)

# Hardcode the IRI to be used for the example, these should be identical with the ones specified in '/test/resources/doe.txt'
derivation_output = ['https://www.example.com/triplestore/ontodoe/DoE_1/NewExperiment_1']
derivation_inputs = ['https://www.example.com/triplestore/ontodoe/DoE_1/Strategy_1',
                    'https://www.example.com/triplestore/ontodoe/DoE_1/Domain_1',
                    'https://www.example.com/triplestore/ontodoe/DoE_1/SystemResponse_1',
                    'https://www.example.com/triplestore/ontodoe/DoE_1/SystemResponse_2',
                    'https://www.example.com/triplestore/ontodoe/DoE_1/HistoricalData_1']
design_of_experiment_iri = 'https://www.example.com/triplestore/ontodoe/DoE_1/DoE_1'

@pytest.fixture()
def initialise_triple_store():
    # NOTE: requires access to the docker.cmclinnovations.com registry from the machine the test is run on.
    # For more information regarding the registry, see: https://github.com/cambridge-cares/TheWorldAvatar/wiki/Docker%3A-Image-registry
    blazegraph = DockerContainer('docker.cmclinnovations.com/blazegraph_for_tests:1.0.0')
    blazegraph.with_exposed_ports(9999) # the port is set as 9999 to match with the value set in the docker image
    yield blazegraph

@pytest.fixture()
def initialise_agent(initialise_triple_store):
    with initialise_triple_store as container:
        # Wait some arbitrary time until container is reachable
        time.sleep(3)

        # Retrieve SPARQL endpoint
        endpoint = get_endpoint(container)

        # Create SparqlClient for testing
        sparql_client = DoESparqlClient(endpoint, endpoint)

        # Initialise doe agent configuration
        config = DoEAgentConfig(str(Path(__file__).absolute().parent) + '/test_conf.json')

        # Initialise DoE agent with temporary docker container endpoint
        doe_agent = DoEAgent(config.ONTOAGENT_SERVICE, config.PERIODIC_TIMESCALE, config.DERIVATION_INSTANCE_BASE_URL, endpoint)

        yield sparql_client, doe_agent

        # Tear down scheduler of doe agent
        doe_agent.scheduler.shutdown()

        # Clear logger at the end of the test
        clear_loggers()

def test_example_doe(initialise_agent):
    sparql_client, doe_agent = initialise_agent

    # Verify that knowledge base is empty
    res = sparql_client.getAmountOfTriples()
    assert res == 0

    # Start the scheduler to monitor derivations
    doe_agent.start_monitoring_derivations()

    # Upload all example triples provided in the resources folder to triple store
    folderpath = str(Path(__file__).absolute().parent) + '/resources/'
    sparql_client.uploadOntology(folderpath+'doe.ttl')
    sparql_client.uploadOntology(folderpath+'Service__DoE.ttl')
    sparql_client.uploadOntology(folderpath+'rxn_data.ttl')

    # Create derivation instance given above information, the timestamp of this derivation is 0
    derivation_iri = doe_agent.derivationClient.createAsynDerivation(derivation_output, doe_agent.agentIRI, derivation_inputs)

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

    # Query timestamp of the derivation for every 20 seconds until it's updated
    currentTimestamp_derivation = 0
    query_timestamp = """SELECT ?time \
                        WHERE { <%s> <%s>/<%s>/<%s> ?time .}""" % (derivation_iri, TIME_HASTIME, TIME_INTIMEPOSITION, TIME_NUMERICPOSITION)
    while currentTimestamp_derivation == 0:
        time.sleep(20)
        currentTimestamp_derivation = sparql_client.performQuery(query_timestamp)[0]['time']

    # Wait some arbitrary time until the cleaning up is done by the derivation client
    time.sleep(20)

    # Query the iri of the new proposed NewExperiment
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

# method adopted from https://github.com/pytest-dev/pytest/issues/5502#issuecomment-647157873
def clear_loggers():
    """Remove handlers from all loggers"""
    import logging
    loggers = [logging.getLogger()] + list(logging.Logger.manager.loggerDict.values())
    for logger in loggers:
        handlers = getattr(logger, 'handlers', [])
        for handler in handlers:
            logger.removeHandler(handler)
