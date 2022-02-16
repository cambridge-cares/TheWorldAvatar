from testcontainers.core.container import DockerContainer
import pytest

from flask_apscheduler import APScheduler
from flask import Flask

from pathlib import Path
import time

from pyasyncagent.data_model import *

from tests.random_agent import *
from tests.sparql_client_for_test import PySparqlClientForTest

import logging
logging.getLogger("py4j").setLevel(logging.INFO)

# Configurations for agent
ONTOAGENT_SERVICE = 'http://www.asyncagent.com/resource/agents/Service__Random#Service' # should match one defined in Service__Random.ttl
DERIVATION_PERIODIC_TIMESCALE = 3
DERIVATION_INSTANCE_BASE_URL = 'http://www.asyncagent.com/triplestore/repository/'

# Predefined inputs and outputs, should match those defined in dummy_abox.ttl
DERIVATION_OUTPUT = ['https://www.example.com/triplestore/random/random_data_1/listofpoints']
DERIVATION_INPUTS = ['https://www.example.com/triplestore/random/random_data_1/numofpoints',
                    'https://www.example.com/triplestore/random/random_data_1/upperlimit',
                    'https://www.example.com/triplestore/random/random_data_1/lowerlimit']

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
        sparql_client = PySparqlClientForTest(endpoint, endpoint)

        # Initialise Async Agent with temporary docker container endpoint
        agent = RandomAgent(ONTOAGENT_SERVICE, DERIVATION_PERIODIC_TIMESCALE, DERIVATION_INSTANCE_BASE_URL, endpoint)

        yield sparql_client, agent

        # Tear down scheduler of doe agent
        agent.scheduler.shutdown()

        # Clear logger at the end of the test
        clear_loggers()

def test_async_agent(initialise_agent):
    sparql_client, agent = initialise_agent

    # Verify that knowledge base is empty
    res = sparql_client.getAmountOfTriples()
    assert res == 0

    # Start the scheduler to monitor derivations
    agent.start_monitoring_derivations()

    # Upload all example triples provided in the resources folder to triple store
    folderpath = str(Path(__file__).absolute().parent) + '/resources/'
    sparql_client.uploadOntology(folderpath+'dummy_tbox.ttl')
    sparql_client.uploadOntology(folderpath+'dummy_abox.ttl')
    sparql_client.uploadOntology(folderpath+'Service__Random.ttl')

    # Create derivation instance given above information, the timestamp of this derivation is 0
    derivation_iri = agent.derivationClient.createAsynDerivation(DERIVATION_OUTPUT, agent.agentIRI, DERIVATION_INPUTS)

    # Check if the derivation instance is created correctly
    assert sparql_client.checkInstanceClass(derivation_iri, ONTODERIVATION_DERIVATIONASYN)

    # Iterate over the list of inputs to add and update the timestamp
    for input in DERIVATION_INPUTS:
        agent.derivationClient.addTimeInstance(input)
        # Update timestamp is needed as the timestamp added using addTimeInstance() is 0
        agent.derivationClient.updateTimestamp(input)

    # Update the asynchronous derivation, it will be marked as "PendingUpdate"
    # The actual update will be handled by monitorDerivation method periodically run by DoE agent
    agent.derivationClient.updateDerivationAsyn(derivation_iri)

    # Query timestamp of the derivation for every 20 seconds until it's updated
    currentTimestamp_derivation = 0
    query_timestamp = """SELECT ?time \
                        WHERE { <%s> <%s>/<%s>/<%s> ?time .}""" % (derivation_iri, TIME_HASTIME, TIME_INTIMEPOSITION, TIME_NUMERICPOSITION)
    while currentTimestamp_derivation == 0:
        time.sleep(5)
        currentTimestamp_derivation = sparql_client.performQuery(query_timestamp)[0]['time']

    # Wait some arbitrary time until the cleaning up is done by the derivation client
    time.sleep(5)

    # Query the iri of the new proposed NewExperiment
    new_listofpoints_iri = sparql_client.getListOfPoints()

    # Check the new generated instance NewExperiment is different from the original one provided in the example
    assert new_listofpoints_iri != DERIVATION_OUTPUT[0]

    # Get the number of points
    numofpoints_iri = sparql_client.getNumOfPoints()
    numofpoints = sparql_client.getValue(numofpoints_iri)

    # Get the list of actual points generated
    pt_dict = sparql_client.getPointsInList(new_listofpoints_iri)

    # Check if the number of generated points is the same as the input
    assert len(pt_dict) == numofpoints

    # Get the max and min vlaue of the generated list of points
    all_pt_values = pt_dict.values()
    max_value = max(all_pt_values)
    min_value = min(all_pt_values)

    # Get the upper limit
    upperlimit_iri = sparql_client.getUpperLimit()
    upperlimit = sparql_client.getValue(upperlimit_iri)
    # Get the lower limit
    lowerlimit_iri = sparql_client.getLowerLimit()
    lowerlimit = sparql_client.getValue(lowerlimit_iri)

    # Check if the max and min generated values are within the upper and lower limits defined in inputs
    assert max_value <= upperlimit
    assert lowerlimit <= min_value

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
