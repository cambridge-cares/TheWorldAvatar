from chemistry_and_robots.data_model.ontovapourtec import AutoSampler
from testcontainers.core.container import DockerContainer
from rdflib import Graph
from pathlib import Path
from enum import Enum
import logging
import pkgutil
import pytest
import time
import uuid
import os

logging.getLogger("py4j").setLevel(logging.INFO)
logger = logging.getLogger('test_sparql_client')

from chemistry_and_robots.kg_operations.sparql_client import ChemistryAndRobotsSparqlClient

class TargetIRIs(Enum):
    AUTOMATEDRXNPLATFORM_DUMMY_IRI = 'http://example.com/blazegraph/namespace/testlab/dummy_lab/AutomatedRxnPlatform_Dummy'
    VAPOURTECRS400_DUMMY_IRI = 'http://example.com/blazegraph/namespace/testlab/dummy_lab/VapourtecRS400_Dummy'
    HPLC_DUMMY_IRI = 'http://example.com/blazegraph/namespace/testlab/dummy_lab/HPLC_Dummy'
    BPR_DUMMY_IRI = 'http://example.com/blazegraph/namespace/testlab/dummy_lab/BPR_Dummy'
    VAPOURTECR4REACTOR_DUMMY_IRI = 'http://example.com/blazegraph/namespace/testlab/dummy_lab/VapourtecR4_Dummy'
    VAPOURTECR4REACTOR_ANOTHER_DUMMY_IRI = 'http://example.com/blazegraph/namespace/testlab/dummy_lab/VapourtecR4_Another_Dummy'
    VAPOURTECR2PUMP_1_DUMMY_IRI = 'http://example.com/blazegraph/namespace/testlab/dummy_lab/VapourtecR2_1_Dummy'
    VAPOURTECR2PUMP_2_DUMMY_IRI = 'http://example.com/blazegraph/namespace/testlab/dummy_lab/VapourtecR2_2_Dummy'
    VAPOURTECR2PUMP_3_DUMMY_IRI = 'http://example.com/blazegraph/namespace/testlab/dummy_lab/VapourtecR2_3_Dummy'
    VAPOURTECR2PUMP_4_DUMMY_IRI = 'http://example.com/blazegraph/namespace/testlab/dummy_lab/VapourtecR2_4_Dummy'
    AUTOSAMPLER_DUMMY_IRI = 'http://example.com/blazegraph/namespace/testlab/dummy_lab/AutoSampler_Dummy'
    EXAMPLE_RXN_EXP_1_IRI = 'https://www.example.com/triplestore/ontorxn/ReactionExperiment_1/RxnExp_1'
    EXAMPLE_RXN_EXP_2_IRI = 'https://www.example.com/triplestore/ontorxn/ReactionExperiment_2/RxnExp_1'
    EXAMPLE_RXN_EXP_3_IRI = 'https://www.example.com/triplestore/ontorxn/ReactionExperiment_3/RxnExp_1'
    EXAMPLE_RXN_EXP_4_IRI = 'https://www.example.com/triplestore/ontorxn/ReactionExperiment_4/RxnExp_1'
    EXAMPLE_RXN_EXP_5_IRI = 'https://www.example.com/triplestore/ontorxn/ReactionExperiment_5/RxnExp_1'
    NEW_RXN_EXP_1_IRI = 'https://www.example.com/triplestore/ontorxn/ReactionExperiment_1/ReactionVariation_342e55c0-23f7-4a08-8443-e958e3b0e2c9'
    NEW_RXN_EXP_2_IRI = 'https://www.example.com/triplestore/ontorxn/ReactionExperiment_1/ReactionVariation_d2f7b1f4-76e2-4401-bb68-29ade1a792ec'
    NEW_RXN_EXP_3_IRI = 'https://www.example.com/triplestore/ontorxn/ReactionExperiment_1/ReactionVariation_d46acf42-ec48-454b-b138-1f548ce1f4ad'
    LIST_RXN_EXP_1_INPUT_CHEMICAL_IRI = ['https://www.example.com/triplestore/ontorxn/ReactionExperiment_1/InputChemical_1',
    'https://www.example.com/triplestore/ontorxn/ReactionExperiment_1/InputChemical_2',
    'https://www.example.com/triplestore/ontorxn/ReactionExperiment_1/InputChemical_3']
    LIST_RXN_EXP_2_INPUT_CHEMICAL_IRI = ['https://www.example.com/triplestore/ontorxn/ReactionExperiment_2/InputChemical_1',
    'https://www.example.com/triplestore/ontorxn/ReactionExperiment_2/InputChemical_2',
    'https://www.example.com/triplestore/ontorxn/ReactionExperiment_2/InputChemical_3']
    LIST_RXN_EXP_3_INPUT_CHEMICAL_IRI = ['https://www.example.com/triplestore/ontorxn/ReactionExperiment_3/InputChemical_1',
    'https://www.example.com/triplestore/ontorxn/ReactionExperiment_3/InputChemical_2',
    'https://www.example.com/triplestore/ontorxn/ReactionExperiment_3/InputChemical_3']
    LIST_RXN_EXP_4_INPUT_CHEMICAL_IRI = ['https://www.example.com/triplestore/ontorxn/ReactionExperiment_4/InputChemical_1',
    'https://www.example.com/triplestore/ontorxn/ReactionExperiment_4/InputChemical_2',
    'https://www.example.com/triplestore/ontorxn/ReactionExperiment_4/InputChemical_3']
    LIST_RXN_EXP_5_INPUT_CHEMICAL_IRI = ['https://www.example.com/triplestore/ontorxn/ReactionExperiment_5/InputChemical_1',
    'https://www.example.com/triplestore/ontorxn/ReactionExperiment_5/InputChemical_2',
    'https://www.example.com/triplestore/ontorxn/ReactionExperiment_5/InputChemical_3']
    LIST_EXAMPLE_RXN_EXP = [EXAMPLE_RXN_EXP_1_IRI, EXAMPLE_RXN_EXP_2_IRI, EXAMPLE_RXN_EXP_3_IRI, EXAMPLE_RXN_EXP_4_IRI, EXAMPLE_RXN_EXP_5_IRI]
    LIST_RXN_EXP_ASSIGNEDTO_VAPR4_DUMMY = [EXAMPLE_RXN_EXP_1_IRI, EXAMPLE_RXN_EXP_2_IRI]
    LIST_VAPR4_DUMMY_CONDUCTED_RXN_EXP = [EXAMPLE_RXN_EXP_1_IRI, EXAMPLE_RXN_EXP_2_IRI]
    LIST_RXN_EXP_ASSIGNEDTO_VAPR4_ANOTHER_DUMMY = [EXAMPLE_RXN_EXP_3_IRI, EXAMPLE_RXN_EXP_4_IRI, EXAMPLE_RXN_EXP_5_IRI]
    LIST_VAPR4_ANOTHER_DUMMY_CONDUCTED_RXN_EXP = [EXAMPLE_RXN_EXP_3_IRI, EXAMPLE_RXN_EXP_4_IRI, EXAMPLE_RXN_EXP_5_IRI]
    LIST_DUMMY_R2PUMPS = [VAPOURTECR2PUMP_1_DUMMY_IRI, VAPOURTECR2PUMP_2_DUMMY_IRI, VAPOURTECR2PUMP_3_DUMMY_IRI, VAPOURTECR2PUMP_4_DUMMY_IRI]
    LIST_DUMMY_R4REACTORS = [VAPOURTECR4REACTOR_DUMMY_IRI, VAPOURTECR4REACTOR_ANOTHER_DUMMY_IRI]

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
def initialise_triples(initialise_triple_store):
    with initialise_triple_store as container:
        # Wait some arbitrary time until container is reachable
        time.sleep(3)

        # Retrieve SPARQL endpoint
        endpoint = get_endpoint(container)

        # Create SparqlClient for testing
        sparql_client = ChemistryAndRobotsSparqlClient(endpoint, endpoint)

        # Upload the example triples for testing
        pathlist = Path(str(Path(__file__).absolute().parent.parent)+'/chemistry_and_robots/resources/').glob('**/*.ttl')
        for path in pathlist:
            sparql_client.uploadOntology(str(path))

        yield sparql_client

        # Clear logger at the end of the test
        clear_loggers()

def test_amount_of_triples_none_zero(initialise_triples):
    sparql_client = initialise_triples
    assert sparql_client.getAmountOfTriples() != 0

def test_get_all_autosampler_with_fill(initialise_triples):
    sparql_client = initialise_triples
    response = sparql_client.get_all_autosampler_with_fill()
    assert len(response) == 1
    autosampler = response[0]
    assert isinstance(autosampler, AutoSampler)
    assert autosampler.instance_iri == TargetIRIs.AUTOSAMPLER_DUMMY_IRI.value

def test_get_r4_reactor_given_vapourtec_rs400(initialise_triples):
    sparql_client = initialise_triples
    response = sparql_client.get_r4_reactor_given_vapourtec_rs400(TargetIRIs.VAPOURTECRS400_DUMMY_IRI.value)
    assert len(response) == len(TargetIRIs.LIST_DUMMY_R4REACTORS.value)
    list_reactor_iri = [res.instance_iri for res in response]
    assert len(set(list_reactor_iri).difference(set(TargetIRIs.LIST_DUMMY_R4REACTORS.value))) == 0

def test_get_r2_pump_given_vapourtec_rs400(initialise_triples):
    sparql_client = initialise_triples
    response = sparql_client.get_r2_pump_given_vapourtec_rs400(TargetIRIs.VAPOURTECRS400_DUMMY_IRI.value)
    assert len(response) == len(TargetIRIs.LIST_DUMMY_R2PUMPS.value)
    list_pumps_iri = [res.instance_iri for res in response]
    assert len(set(list_pumps_iri).difference(set(TargetIRIs.LIST_DUMMY_R2PUMPS.value))) == 0

@pytest.mark.parametrize(
    "r4_reactor_iri,rxn_exp_conducted",
    [
        (TargetIRIs.VAPOURTECR4REACTOR_DUMMY_IRI.value, TargetIRIs.LIST_VAPR4_DUMMY_CONDUCTED_RXN_EXP.value),
        (TargetIRIs.VAPOURTECR4REACTOR_ANOTHER_DUMMY_IRI.value, TargetIRIs.LIST_RXN_EXP_ASSIGNEDTO_VAPR4_ANOTHER_DUMMY.value),
    ],
)
def test_get_rxn_exp_conducted_in_r4_reactor(initialise_triples, r4_reactor_iri, rxn_exp_conducted):
    sparql_client = initialise_triples
    response = sparql_client.get_rxn_exp_conducted_in_r4_reactor(r4_reactor_iri)
    assert len(response) == len(rxn_exp_conducted)
    assert len(set(response).difference(set(rxn_exp_conducted))) == 0

@pytest.mark.parametrize(
    "r4_reactor_iri,rxn_exp_assigned",
    [
        (TargetIRIs.VAPOURTECR4REACTOR_DUMMY_IRI.value, TargetIRIs.LIST_RXN_EXP_ASSIGNEDTO_VAPR4_DUMMY.value),
        (TargetIRIs.VAPOURTECR4REACTOR_ANOTHER_DUMMY_IRI.value, TargetIRIs.LIST_RXN_EXP_ASSIGNEDTO_VAPR4_ANOTHER_DUMMY.value),
    ],
)
def test_get_rxn_exp_assigned_to_r4_reactor(initialise_triples, r4_reactor_iri, rxn_exp_assigned):
    sparql_client = initialise_triples
    response = sparql_client.get_rxn_exp_assigned_to_r4_reactor(r4_reactor_iri)
    assert len(response) == len(rxn_exp_assigned)
    assert len(set(response).difference(set(rxn_exp_assigned))) == 0

@pytest.mark.parametrize(
    "r4_reactor_iri,rxn_exp_pending",
    [
        (TargetIRIs.VAPOURTECR4REACTOR_DUMMY_IRI.value, []),
        (TargetIRIs.VAPOURTECR4REACTOR_ANOTHER_DUMMY_IRI.value, []),
    ],
)
def test_get_rxn_exp_pending_for_r4_reactor(initialise_triples, r4_reactor_iri, rxn_exp_pending):
    sparql_client = initialise_triples
    response = sparql_client.get_rxn_exp_pending_for_r4_reactor(r4_reactor_iri)
    assert len(response) == len(rxn_exp_pending)
    assert len(set(response).difference(set(rxn_exp_pending))) == 0

@pytest.mark.parametrize(
    "rxnexp_iri,input_chemical_iri",
    [
        (TargetIRIs.EXAMPLE_RXN_EXP_1_IRI.value, TargetIRIs.LIST_RXN_EXP_1_INPUT_CHEMICAL_IRI.value),
        (TargetIRIs.EXAMPLE_RXN_EXP_2_IRI.value, TargetIRIs.LIST_RXN_EXP_2_INPUT_CHEMICAL_IRI.value),
        (TargetIRIs.EXAMPLE_RXN_EXP_3_IRI.value, TargetIRIs.LIST_RXN_EXP_3_INPUT_CHEMICAL_IRI.value),
        (TargetIRIs.EXAMPLE_RXN_EXP_4_IRI.value, TargetIRIs.LIST_RXN_EXP_4_INPUT_CHEMICAL_IRI.value),
        (TargetIRIs.EXAMPLE_RXN_EXP_5_IRI.value, TargetIRIs.LIST_RXN_EXP_5_INPUT_CHEMICAL_IRI.value),
    ],
)
def test_get_input_chemical_of_rxn_exp(initialise_triples, rxnexp_iri, input_chemical_iri):
    sparql_client = initialise_triples
    response = sparql_client.get_input_chemical_of_rxn_exp(rxnexp_iri)
    list_input_chemical = [res.instance_iri for res in response]
    assert len(list_input_chemical) == len(input_chemical_iri)
    assert len(set(list_input_chemical).difference(set(input_chemical_iri))) == 0

# TODO
# @pytest.mark.parametrize(
#     "new_rxn_exp_iri,list_r4_reactor_iri",
#     [
#         (TargetIRIs.NEW_RXN_EXP_1_IRI.value, TargetIRIs.LIST_DUMMY_R4REACTORS.value),
#         (TargetIRIs.NEW_RXN_EXP_2_IRI.value, TargetIRIs.LIST_DUMMY_R4REACTORS.value),
#         (TargetIRIs.NEW_RXN_EXP_3_IRI.value, TargetIRIs.LIST_DUMMY_R4REACTORS.value),
#     ],
# )
# def test_get_preferred_r4_reactor(initialise_triples, new_rxn_exp_iri, list_r4_reactor_iri):
#     sparql_client = initialise_triples
#     response = sparql_client.getReactionExperiment(new_rxn_exp_iri)
#     assert len(response) == 1
#     assert response[0].instance_iri == new_rxn_exp_iri
#     preferred_r4_reactor = sparql_client.get_preferred_r4_reactor(response[0])
#     assert preferred_r4_reactor in list_r4_reactor_iri

@pytest.mark.parametrize(
    "new_rxn_exp_iri,r4_reactor_iri",
    [
        (TargetIRIs.NEW_RXN_EXP_1_IRI.value, TargetIRIs.VAPOURTECR4REACTOR_DUMMY_IRI.value),
        (TargetIRIs.NEW_RXN_EXP_2_IRI.value, TargetIRIs.VAPOURTECR4REACTOR_DUMMY_IRI.value),
        (TargetIRIs.NEW_RXN_EXP_3_IRI.value, TargetIRIs.VAPOURTECR4REACTOR_DUMMY_IRI.value),
        (TargetIRIs.NEW_RXN_EXP_1_IRI.value, TargetIRIs.VAPOURTECR4REACTOR_ANOTHER_DUMMY_IRI.value),
        (TargetIRIs.NEW_RXN_EXP_2_IRI.value, TargetIRIs.VAPOURTECR4REACTOR_ANOTHER_DUMMY_IRI.value),
        (TargetIRIs.NEW_RXN_EXP_3_IRI.value, TargetIRIs.VAPOURTECR4REACTOR_ANOTHER_DUMMY_IRI.value),
    ],
)
def test_assign_and_remove_rxn_exp_to_r4_reactor(initialise_triples, new_rxn_exp_iri, r4_reactor_iri):
    sparql_client = initialise_triples
    response1 = sparql_client.get_rxn_exp_assigned_to_r4_reactor(r4_reactor_iri)
    assert new_rxn_exp_iri not in response1

    sparql_client.assign_rxn_exp_to_r4_reactor(new_rxn_exp_iri, r4_reactor_iri)
    response2 = sparql_client.get_rxn_exp_assigned_to_r4_reactor(r4_reactor_iri)
    assert new_rxn_exp_iri in response2

    sparql_client.remove_rxn_exp_from_r4_reactor(new_rxn_exp_iri, r4_reactor_iri)
    response3 = sparql_client.get_rxn_exp_assigned_to_r4_reactor(r4_reactor_iri)
    assert new_rxn_exp_iri not in response3

# TODO
# def test_get_vapourtec_rs400_given_autosampler(initialise_triples):
#     sparql_client = initialise_triples
#     response = sparql_client.get_vapourtec_rs400_given_autosampler()

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
