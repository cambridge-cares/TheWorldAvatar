from rdflib import Graph
from rdflib import URIRef
from rdflib import RDF
import uuid
import pytest
import time

import vtexeagent.tests.utils as utils


# NOTE the derivation_periodic_timescale (6, 7, 8) are chosen randomly for the test cases
@pytest.mark.parametrize(
    "new_rxn_exp_iri,derivation_periodic_timescale",
    [
        (utils.cf.NEW_RXN_EXP_1_IRI, 6),
        (utils.cf.NEW_RXN_EXP_2_IRI, 7),
        (utils.cf.NEW_RXN_EXP_3_IRI, 8),
    ],
)
def test_monitor_derivation(
    initialise_client, create_vapourtec_execution_agent, generate_random_download_path,
    new_rxn_exp_iri, derivation_periodic_timescale
):
    # Initialise triples
    sparql_client = initialise_client
    utils.initialise_triples(generate_random_download_path, sparql_client)

    # Query reaction experiment, check it's not assigned to any reactor
    lst_unassigned_rxn_exp_instance = sparql_client.getReactionExperiment(new_rxn_exp_iri)
    assert len(lst_unassigned_rxn_exp_instance) == 1
    assert lst_unassigned_rxn_exp_instance[0].isAssignedTo is None

    # Create instance of agent and start monitor derivations
    vapourtec_execution_agent = create_vapourtec_execution_agent(
        maximum_concurrent_experiment=1,#maximum_concurrent_experiment,
        register_agent=True,
        random_agent_iri=True,
        derivation_periodic_timescale=derivation_periodic_timescale,
    )
    vapourtec_execution_agent.start_monitoring_derivations()

    # NOTE Add placeholder agent service iri to manage the digital twin of all hardware
    # NOTE This should actually be done by the VapourtecAgent/AgilentAgent themselves when they are deployed
    sparql_client.performUpdate("""INSERT {?hplc <%s> <%s>. ?vapourtec <%s> <%s>.} WHERE {?hplc a <%s>. ?vapourtec a <%s>.}""" % (
        utils.cf.ONTOLAB_ISMANAGEDBY, str(uuid.uuid4()), utils.cf.ONTOLAB_ISMANAGEDBY, str(uuid.uuid4()), utils.cf.ONTOHPLC_HIGHPERFORMANCELIQUIDCHROMATOGRAPHY, utils.cf.ONTOVAPOURTEC_VAPOURTECRS400
    ))

    # Add timestamp to new_rxn_exp_iri (pure inputs)
    vapourtec_execution_agent.derivationClient.addTimeInstance(new_rxn_exp_iri)
    vapourtec_execution_agent.derivationClient.updateTimestamp(new_rxn_exp_iri)

    # Instantiate derivation instance
    derivation_iri = vapourtec_execution_agent.derivationClient.createAsyncDerivationForNewInfo(vapourtec_execution_agent.agentIRI, [new_rxn_exp_iri])

    # Wait until derivations for vapourtec and agilent are instantiated
    agilent_derivation = None
    while not agilent_derivation:
        time.sleep(10)
        agilent_derivation = utils.get_agilent_derivation(new_rxn_exp_iri, sparql_client)

    # Insert placeholder triples to let the VapourtecExecutionAgent finish job
    g = Graph()
    placeholder_hplcjob = "http://placeholder/" + str(uuid.uuid4())
    placeholder_hplcreport = "http://placeholder/" + str(uuid.uuid4())
    g.add((URIRef(placeholder_hplcjob), RDF.type, URIRef(utils.cf.ONTOHPLC_HPLCJOB)))
    g.add((URIRef(placeholder_hplcreport), RDF.type, URIRef(utils.cf.ONTOHPLC_HPLCREPORT)))
    g.add((URIRef(placeholder_hplcjob), URIRef(utils.cf.ONTODERIVATION_BELONGSTO), URIRef(agilent_derivation)))
    g.add((URIRef(placeholder_hplcjob), URIRef(utils.cf.ONTOHPLC_HASREPORT), URIRef(placeholder_hplcreport)))
    sparql_client.uploadGraph(g)

    # Wait until derivation update is finished
    currentTimestamp_derivation = 0
    while currentTimestamp_derivation == 0:
        time.sleep(10)
        currentTimestamp_derivation = utils.get_timestamp(derivation_iri, sparql_client)

    # Check execution of derivation
    # (1) reaction experiment should be assigned to a reactor
    lst_done_rxn_exp_instance = sparql_client.getReactionExperiment(new_rxn_exp_iri)
    assert len(lst_done_rxn_exp_instance) == 1
    assert lst_done_rxn_exp_instance[0].isAssignedTo is not None
    # (2) HPLCReport instance should added to the derivation outputs
    lst_derivation_outputs = utils.get_derivation_outputs(derivation_iri, sparql_client)
    assert len(lst_derivation_outputs) == 1
    assert placeholder_hplcreport == lst_derivation_outputs[0]

    # Shutdown the scheduler to clean up before the next test
    vapourtec_execution_agent.scheduler.shutdown()


# # NOTE the derivation_periodic_timescale (6, 7, 8) are chosen randomly for the test cases
# @pytest.mark.parametrize(
#     "new_rxn_exp_iri,derivation_periodic_timescale",
#     [
#         (utils.cf.NEW_RXN_EXP_1_IRI, 6),
#         (utils.cf.NEW_RXN_EXP_2_IRI, 7),
#         (utils.cf.NEW_RXN_EXP_3_IRI, 8),
#     ],
# )
# def test_docker_integration(
#     initialise_client, create_vapourtec_execution_agent, generate_random_download_path,
#     new_rxn_exp_iri, derivation_periodic_timescale
# ):
#     pass
