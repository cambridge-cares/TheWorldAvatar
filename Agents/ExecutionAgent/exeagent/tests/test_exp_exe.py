from asyncio.log import logger
from testcontainers.core.container import DockerContainer
from rdflib import Graph
from enum import Enum
import logging
import pkgutil
import pytest
import time
import uuid
import os

from exeagent.agent import *
from exeagent.conf import *

logging.getLogger("py4j").setLevel(logging.INFO)

# TODO Hardcode the IRI to be used for the example, these should be identical with the ones specified in ''
class DerivationIO(Enum):
    NEW_RXN_EXP_1_IRI = 'https://www.example.com/triplestore/ontorxn/ReactionExperiment_1/ReactionVariation_fac53bb1-3ae0-4941-9f5b-38738b07ab70'
    NEW_RXN_EXP_2_IRI = 'https://www.example.com/triplestore/ontorxn/ReactionExperiment_1/ReactionVariation_3bd3166d-f782-4cdc-a6a8-75336afd71a8'
    NEW_RXN_EXP_3_IRI = 'https://www.example.com/triplestore/ontorxn/ReactionExperiment_1/ReactionVariation_c4b175d9-e53c-4d7e-b053-3a81f7ca0ddf'
    # TODO delete below placeholder IRIs as now we have the new information generation mode
    # # TODO it's only referring to the ReactorSettings at the moment, to be extended to support PumpSettings
    # PLACEHOLDER_1_IRI = 'https://www.example.com/triplestore/ontolab/ExpSetup_1/EquipmentSettings_1'
    # PLACEHOLDER_2_IRI = 'https://www.example.com/triplestore/ontolab/ExpSetup_2/EquipmentSettings_1'
    # PLACEHOLDER_3_IRI = 'https://www.example.com/triplestore/ontolab/ExpSetup_3/EquipmentSettings_1'

# The (scope="module") is added to make the initialisation only run once for the whole python module so it saves time
@pytest.fixture(scope="module")
def initialise_triple_store():
    # NOTE: requires access to the docker.cmclinnovations.com registry from the machine the test is run on.
    # For more information regarding the registry, see: https://github.com/cambridge-cares/TheWorldAvatar/wiki/Docker%3A-Image-registry
    blazegraph = DockerContainer('docker.cmclinnovations.com/blazegraph_for_tests:1.0.0')
    blazegraph.with_exposed_ports(9999) # the port is set as 9999 to match with the value set in the docker image
    yield blazegraph

# The (scope="module") is added to make the initialisation only run once for the whole python module so it saves time
@pytest.fixture(scope="module")
def initialise_agent(initialise_triple_store):
    with initialise_triple_store as container:
        # Wait some arbitrary time until container is reachable
        time.sleep(3)

        # Retrieve SPARQL endpoint
        endpoint = get_endpoint(container)

        # Create SparqlClient for testing
        sparql_client = ChemistryAndRobotsSparqlClient(endpoint, endpoint)

        # Initialise doe agent configuration
        config = ExeAgentConfig(str(Path(__file__).absolute().parent) + '/test_conf.json')

        # Initialise DoE agent with temporary docker container endpoint
        exe_agent = ExecutionAgent(config.ONTOAGENT_SERVICE, config.PERIODIC_TIMESCALE, config.DERIVATION_INSTANCE_BASE_URL, endpoint)

        # Start the scheduler to monitor derivations
        exe_agent.start_monitoring_derivations()

        # Upload triples
        for f in ['ontoagent/Service__Execution.ttl', 'sample_data/new_exp_data.ttl', 'sample_data/duplicate_ontorxn.ttl', 'sample_data/dummy_lab.ttl', 'sample_data/rxn_data.ttl']:
            data = pkgutil.get_data('chemistry_and_robots', 'resources/'+f).decode("utf-8")
            g = Graph().parse(data=data)
            filePath = f'{str(uuid.uuid4())}.ttl'
            g.serialize(filePath, format='ttl')
            sparql_client.uploadOntology(filePath)
            os.remove(filePath)

        # Iterate over the list of inputs to add and update the timestamp
        for input in DerivationIO:
            exe_agent.derivationClient.addTimeInstance(input.value)
            # Update timestamp is needed as the timestamp added using addTimeInstance() is 0
            exe_agent.derivationClient.updateTimestamp(input.value)

        yield sparql_client, exe_agent

        # Tear down scheduler of doe agent
        exe_agent.scheduler.shutdown()

        # Clear logger at the end of the test
        clear_loggers()

@pytest.mark.parametrize(
    # "derivation_input,derivation_output",
    # [
    #     ([DerivationIO.NEW_RXN_EXP_1_IRI.value], [DerivationIO.PLACEHOLDER_1_IRI.value]),
    #     # ([DerivationIO.NEW_RXN_EXP_2_IRI.value], [None]),
    #     # ([DerivationIO.NEW_RXN_EXP_3_IRI.value], [None]),
    # ],
    "derivation_input",
    [
        ([DerivationIO.NEW_RXN_EXP_1_IRI.value]),
        ([DerivationIO.NEW_RXN_EXP_2_IRI.value]),
        ([DerivationIO.NEW_RXN_EXP_3_IRI.value]),
    ],
)
def test_exp_exe(initialise_agent, derivation_input):
    sparql_client, exe_agent = initialise_agent

    # Create derivation instance given above information, the timestamp of this derivation is 0
    derivation_iri = exe_agent.derivationClient.createAsyncDerivationForNewInfo(exe_agent.agentIRI, derivation_input)

    # Check if the derivation instance is created correctly
    assert sparql_client.checkInstanceClass(derivation_iri, ONTODERIVATION_DERIVATIONASYN)

    # Query timestamp of the derivation for every 20 seconds until it's updated
    currentTimestamp_derivation = 0
    query_timestamp = """SELECT ?time \
                        WHERE { <%s> <%s>/<%s>/<%s> ?time .}""" % (derivation_iri, TIME_HASTIME, TIME_INTIMEPOSITION, TIME_NUMERICPOSITION)
    while currentTimestamp_derivation == 0:
        time.sleep(20)
        currentTimestamp_derivation = int(sparql_client.performQuery(query_timestamp)[0]['time'])

    # Wait some arbitrary time until the cleaning up is done by the derivation client
    time.sleep(20)

    response = sparql_client.performQuery("""SELECT ?output WHERE {?output <%s> <%s>}""" % (ONTODERIVATION_BELONGSTO, derivation_iri))
    logger.info(response)
    # Query the iri of the new proposed
    # equip_settings = sparql_client.get_equip_settings_of_rxn_exp(derivation_input[0])

    # Check the new generated instances of EquipmentSettings are different from the ones provided in the example as placeholder
    # assert len(equip_settings) == len(derivation_output)
    # assert len(set(equip_settings).difference(set(derivation_output))) != 0

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
