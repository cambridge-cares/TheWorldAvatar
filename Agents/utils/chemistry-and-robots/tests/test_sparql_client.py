from chemistry_and_robots.data_model.ontovapourtec import AutoSampler
from chemistry_and_robots.kg_operations import sparql_client
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

from chemistry_and_robots.kg_operations.sparql_client import ChemistryAndRobotsSparqlClient, ONTOVAPOURTEC_NULL, ONTOVAPOURTEC_IDLE

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
    NEW_RXN_EXP_1_IRI = 'https://www.example.com/triplestore/ontorxn/ReactionExperiment_1/ReactionVariation_d9b9223c-c993-44e2-80cf-dcd9111029b1'
    NEW_RXN_EXP_2_IRI = 'https://www.example.com/triplestore/ontorxn/ReactionExperiment_1/ReactionVariation_5fab2298-5bf0-4b2c-aab1-28deeb412f2a'
    NEW_RXN_EXP_3_IRI = 'https://www.example.com/triplestore/ontorxn/ReactionExperiment_1/ReactionVariation_d8c28dd6-ffb3-4669-a4e4-602c923ccf3c'
    CHEMICAL_REACTION_IRI = 'https://www.example.com/triplestore/ontorxn/ChemRxn_1/ChemRxn_1'
    REACTANT_SPECIES_DICTIONARY = {'https://www.example.com/triplestore/ontorxn/ChemRxn_1/Species_1': 'http://www.theworldavatar.com/kb/ontospecies/Species_54d8b46b-17bc-4bbd-a3cc-3b3a16d6ae4b',
    'https://www.example.com/triplestore/ontorxn/ChemRxn_1/Species_2': 'http://www.theworldavatar.com/kb/ontospecies/Species_353d4667-e25d-476a-bd74-5c34723c8ea3'}
    PRODUCT_SPECIES_DICTIONARY = {'https://www.example.com/triplestore/ontorxn/ChemRxn_1/Species_3': 'https://www.example.com/triplestore/ontorxn/ChemRxn_1/Species_placeholder_pubchemcid_637759',
    'https://www.example.com/triplestore/ontorxn/ChemRxn_1/Species_4': 'https://www.example.com/triplestore/ontorxn/ChemRxn_1/Species_placeholder_pubchemcid_640180'}
    CATALYST_SPECIES_DICTIONARY = {'https://www.example.com/triplestore/ontorxn/ChemRxn_1/Species_5': 'http://www.theworldavatar.com/kb/ontospecies/Species_cb3b0560-0df7-4deb-891e-bbb11e7c2b3d'}
    SOLVENT_SPECIES_DICTIONARY = {'https://www.example.com/triplestore/ontorxn/ChemRxn_1/Species_6': 'http://www.theworldavatar.com/kb/ontospecies/Species_0401f93b-b62d-488e-ba1f-7d5c37e365cb',
    'https://www.example.com/triplestore/ontorxn/ChemRxn_1/Species_7': 'http://www.theworldavatar.com/kb/ontospecies/Species_63fefc5a-d49d-4841-a946-2cdb5f356983'}
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
    # LIST_VAPR4_DUMMY_CONDUCTED_RXN_EXP = [EXAMPLE_RXN_EXP_1_IRI, EXAMPLE_RXN_EXP_2_IRI]
    LIST_RXN_EXP_ASSIGNEDTO_VAPR4_ANOTHER_DUMMY = [EXAMPLE_RXN_EXP_3_IRI, EXAMPLE_RXN_EXP_4_IRI, EXAMPLE_RXN_EXP_5_IRI]
    # LIST_VAPR4_ANOTHER_DUMMY_CONDUCTED_RXN_EXP = [EXAMPLE_RXN_EXP_3_IRI, EXAMPLE_RXN_EXP_4_IRI, EXAMPLE_RXN_EXP_5_IRI]
    LIST_DUMMY_R2PUMPS = [VAPOURTECR2PUMP_1_DUMMY_IRI, VAPOURTECR2PUMP_2_DUMMY_IRI, VAPOURTECR2PUMP_3_DUMMY_IRI, VAPOURTECR2PUMP_4_DUMMY_IRI]
    LIST_DUMMY_R4REACTORS = [VAPOURTECR4REACTOR_DUMMY_IRI, VAPOURTECR4REACTOR_ANOTHER_DUMMY_IRI]
    RXN_EXP_QUEUE_1 = 'http://example.com/blazegraph/namespace/test_temp/exp_1'
    RXN_EXP_QUEUE_2 = 'http://example.com/blazegraph/namespace/test_temp/exp_2'
    RXN_EXP_QUEUE_3 = 'http://example.com/blazegraph/namespace/test_temp/exp_3'
    RXN_EXP_QUEUE_4 = 'http://example.com/blazegraph/namespace/test_temp/exp_4'
    RXN_EXP_QUEUE_5 = 'http://example.com/blazegraph/namespace/test_temp/exp_5'
    RXN_EXP_QUEUE_6 = 'http://example.com/blazegraph/namespace/test_temp/exp_6'
    RXN_EXP_QUEUE_7 = 'http://example.com/blazegraph/namespace/test_temp/exp_7'
    RXN_EXP_1_PRIOR = []
    RXN_EXP_2_PRIOR = [RXN_EXP_QUEUE_1]
    RXN_EXP_3_PRIOR = [RXN_EXP_QUEUE_1, RXN_EXP_QUEUE_2]
    RXN_EXP_4_PRIOR = [RXN_EXP_QUEUE_1, RXN_EXP_QUEUE_2, RXN_EXP_QUEUE_3]
    RXN_EXP_5_PRIOR = [RXN_EXP_QUEUE_1, RXN_EXP_QUEUE_2, RXN_EXP_QUEUE_3, RXN_EXP_QUEUE_4]
    RXN_EXP_6_PRIOR = [RXN_EXP_QUEUE_1, RXN_EXP_QUEUE_2, RXN_EXP_QUEUE_3, RXN_EXP_QUEUE_4]
    RXN_EXP_7_PRIOR = [RXN_EXP_QUEUE_1, RXN_EXP_QUEUE_2, RXN_EXP_QUEUE_3, RXN_EXP_QUEUE_4]

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

# TODO commented out for now, decide whether to keep it before merging back to develop based on the function in sparql_client.py
# @pytest.mark.parametrize(
#     "r4_reactor_iri,rxn_exp_conducted",
#     [
#         (TargetIRIs.VAPOURTECR4REACTOR_DUMMY_IRI.value, TargetIRIs.LIST_VAPR4_DUMMY_CONDUCTED_RXN_EXP.value),
#         (TargetIRIs.VAPOURTECR4REACTOR_ANOTHER_DUMMY_IRI.value, TargetIRIs.LIST_RXN_EXP_ASSIGNEDTO_VAPR4_ANOTHER_DUMMY.value),
#     ],
# )
# def test_get_rxn_exp_conducted_in_r4_reactor(initialise_triples, r4_reactor_iri, rxn_exp_conducted):
#     sparql_client = initialise_triples
#     response = sparql_client.get_rxn_exp_conducted_in_r4_reactor(r4_reactor_iri)
#     assert len(response) == len(rxn_exp_conducted)
#     assert len(set(response).difference(set(rxn_exp_conducted))) == 0

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

# TODO commented out for now, decide whether to keep it before merging back to develop based on the function in sparql_client.py
# @pytest.mark.parametrize(
#     "r4_reactor_iri,rxn_exp_pending",
#     [
#         (TargetIRIs.VAPOURTECR4REACTOR_DUMMY_IRI.value, []),
#         (TargetIRIs.VAPOURTECR4REACTOR_ANOTHER_DUMMY_IRI.value, []),
#     ],
# )
# def test_get_rxn_exp_pending_for_r4_reactor(initialise_triples, r4_reactor_iri, rxn_exp_pending):
#     sparql_client = initialise_triples
#     response = sparql_client.get_rxn_exp_pending_for_r4_reactor(r4_reactor_iri)
#     assert len(response) == len(rxn_exp_pending)
#     assert len(set(response).difference(set(rxn_exp_pending))) == 0

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

def test_get_vapourtec_rs400_given_autosampler(initialise_triples):
    sparql_client = initialise_triples
    autosampler = sparql_client.get_autosampler(TargetIRIs.AUTOSAMPLER_DUMMY_IRI.value)
    assert autosampler.instance_iri == TargetIRIs.AUTOSAMPLER_DUMMY_IRI.value
    response = sparql_client.get_vapourtec_rs400_given_autosampler(autosampler)
    assert  response.instance_iri == TargetIRIs.VAPOURTECRS400_DUMMY_IRI.value

@pytest.mark.parametrize(
    "new_rxn_exp_iri,list_r4_reactor_iri,vapourtec_rs400",
    [
        (TargetIRIs.NEW_RXN_EXP_1_IRI.value, TargetIRIs.LIST_DUMMY_R4REACTORS.value, TargetIRIs.VAPOURTECRS400_DUMMY_IRI.value),
        (TargetIRIs.NEW_RXN_EXP_2_IRI.value, TargetIRIs.LIST_DUMMY_R4REACTORS.value, TargetIRIs.VAPOURTECRS400_DUMMY_IRI.value),
        (TargetIRIs.NEW_RXN_EXP_3_IRI.value, TargetIRIs.LIST_DUMMY_R4REACTORS.value, TargetIRIs.VAPOURTECRS400_DUMMY_IRI.value),
    ],
)
def test_get_preferred_vapourtec_rs400(initialise_triples, new_rxn_exp_iri, list_r4_reactor_iri, vapourtec_rs400):
    sparql_client = initialise_triples
    response = sparql_client.getReactionExperiment(new_rxn_exp_iri)
    assert len(response) == 1
    assert response[0].instance_iri == new_rxn_exp_iri
    preferred_rs400, preferred_r4_reactor = sparql_client.get_preferred_vapourtec_rs400(response[0])
    assert preferred_r4_reactor.instance_iri in list_r4_reactor_iri
    assert preferred_rs400.instance_iri == vapourtec_rs400

    # Change the status to Null
    sparql_client.update_vapourtec_rs400_state(vapourtec_rs400, ONTOVAPOURTEC_NULL)
    # Now perform the same checking
    new_rs400, new_r4_reactor = sparql_client.get_preferred_vapourtec_rs400(response[0])
    # Change back the status to Idle
    sparql_client.update_vapourtec_rs400_state(vapourtec_rs400, ONTOVAPOURTEC_IDLE)
    # Now perform the same checking, the returned values should be None, None
    assert None == new_rs400
    assert None == new_r4_reactor

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

@pytest.mark.parametrize(
    "rxn_exp_iri,prior_rxn_exp",
    [
        (TargetIRIs.RXN_EXP_QUEUE_1.value, TargetIRIs.RXN_EXP_1_PRIOR.value),
        (TargetIRIs.RXN_EXP_QUEUE_2.value, TargetIRIs.RXN_EXP_2_PRIOR.value),
        (TargetIRIs.RXN_EXP_QUEUE_3.value, TargetIRIs.RXN_EXP_3_PRIOR.value),
        (TargetIRIs.RXN_EXP_QUEUE_4.value, TargetIRIs.RXN_EXP_4_PRIOR.value),
        (TargetIRIs.RXN_EXP_QUEUE_5.value, TargetIRIs.RXN_EXP_5_PRIOR.value),
        (TargetIRIs.RXN_EXP_QUEUE_6.value, TargetIRIs.RXN_EXP_6_PRIOR.value),
        (TargetIRIs.RXN_EXP_QUEUE_7.value, TargetIRIs.RXN_EXP_7_PRIOR.value),
    ],
)
def test_get_prior_rxn_exp_in_queue(initialise_triples, rxn_exp_iri, prior_rxn_exp):
    sparql_client = initialise_triples
    rxn_exp_queue = sparql_client.get_prior_rxn_exp_in_queue(rxn_exp_iri)
    assert all(item in prior_rxn_exp for item in [*rxn_exp_queue])

@pytest.mark.parametrize(
    "rxn_exp_iri,chem_rxn_iri,reactant,product,catalyst,solvent",
    [
        (TargetIRIs.EXAMPLE_RXN_EXP_1_IRI.value, TargetIRIs.CHEMICAL_REACTION_IRI.value,
        TargetIRIs.REACTANT_SPECIES_DICTIONARY.value, TargetIRIs.PRODUCT_SPECIES_DICTIONARY.value,
        TargetIRIs.CATALYST_SPECIES_DICTIONARY.value, TargetIRIs.SOLVENT_SPECIES_DICTIONARY.value),
        (TargetIRIs.EXAMPLE_RXN_EXP_2_IRI.value, TargetIRIs.CHEMICAL_REACTION_IRI.value,
        TargetIRIs.REACTANT_SPECIES_DICTIONARY.value, TargetIRIs.PRODUCT_SPECIES_DICTIONARY.value,
        TargetIRIs.CATALYST_SPECIES_DICTIONARY.value, TargetIRIs.SOLVENT_SPECIES_DICTIONARY.value),
        (TargetIRIs.EXAMPLE_RXN_EXP_3_IRI.value, TargetIRIs.CHEMICAL_REACTION_IRI.value,
        TargetIRIs.REACTANT_SPECIES_DICTIONARY.value, TargetIRIs.PRODUCT_SPECIES_DICTIONARY.value,
        TargetIRIs.CATALYST_SPECIES_DICTIONARY.value, TargetIRIs.SOLVENT_SPECIES_DICTIONARY.value),
        (TargetIRIs.EXAMPLE_RXN_EXP_4_IRI.value, TargetIRIs.CHEMICAL_REACTION_IRI.value,
        TargetIRIs.REACTANT_SPECIES_DICTIONARY.value, TargetIRIs.PRODUCT_SPECIES_DICTIONARY.value,
        TargetIRIs.CATALYST_SPECIES_DICTIONARY.value, TargetIRIs.SOLVENT_SPECIES_DICTIONARY.value),
        (TargetIRIs.EXAMPLE_RXN_EXP_5_IRI.value, TargetIRIs.CHEMICAL_REACTION_IRI.value,
        TargetIRIs.REACTANT_SPECIES_DICTIONARY.value, TargetIRIs.PRODUCT_SPECIES_DICTIONARY.value,
        TargetIRIs.CATALYST_SPECIES_DICTIONARY.value, TargetIRIs.SOLVENT_SPECIES_DICTIONARY.value),
        (TargetIRIs.NEW_RXN_EXP_1_IRI.value, TargetIRIs.CHEMICAL_REACTION_IRI.value,
        TargetIRIs.REACTANT_SPECIES_DICTIONARY.value, TargetIRIs.PRODUCT_SPECIES_DICTIONARY.value,
        TargetIRIs.CATALYST_SPECIES_DICTIONARY.value, TargetIRIs.SOLVENT_SPECIES_DICTIONARY.value),
        (TargetIRIs.NEW_RXN_EXP_2_IRI.value, TargetIRIs.CHEMICAL_REACTION_IRI.value,
        TargetIRIs.REACTANT_SPECIES_DICTIONARY.value, TargetIRIs.PRODUCT_SPECIES_DICTIONARY.value,
        TargetIRIs.CATALYST_SPECIES_DICTIONARY.value, TargetIRIs.SOLVENT_SPECIES_DICTIONARY.value),
        (TargetIRIs.NEW_RXN_EXP_3_IRI.value, TargetIRIs.CHEMICAL_REACTION_IRI.value,
        TargetIRIs.REACTANT_SPECIES_DICTIONARY.value, TargetIRIs.PRODUCT_SPECIES_DICTIONARY.value,
        TargetIRIs.CATALYST_SPECIES_DICTIONARY.value, TargetIRIs.SOLVENT_SPECIES_DICTIONARY.value),
    ],
)
def test_get_chemical_reaction(initialise_triples, rxn_exp_iri, chem_rxn_iri, reactant, product, catalyst, solvent):
    sparql_client = initialise_triples
    chem_rxn = sparql_client.get_chemical_reaction(rxn_exp_iri)
    print(chem_rxn)
    print(chem_rxn.instance_iri)
    assert chem_rxn.instance_iri == chem_rxn_iri
    dict_reactant = {reactant.instance_iri:reactant.hasUniqueSpecies for reactant in chem_rxn.hasReactant}
    assert dict_reactant == reactant
    dict_product = {product.instance_iri:product.hasUniqueSpecies for product in chem_rxn.hasProduct}
    assert dict_product == product
    dict_catalyst = {catalyst.instance_iri:catalyst.hasUniqueSpecies for catalyst in chem_rxn.hasCatalyst}
    assert dict_catalyst == catalyst
    dict_solvent = {solvent.instance_iri:solvent.hasUniqueSpecies for solvent in chem_rxn.hasSolvent}
    assert dict_solvent == solvent

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
