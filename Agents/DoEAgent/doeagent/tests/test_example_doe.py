import time

import doeagent.tests.utils as utils

import logging
logger = logging.getLogger('test_example_doe')

def test_example_doe(generate_random_download_path, initialise_agent):
    sparql_client, derivation_client, doe_agent = initialise_agent

    # Verify that knowledge base is empty
    res = sparql_client.getAmountOfTriples()
    assert res == 0

    # Start the scheduler to monitor derivations
    doe_agent.start_monitoring_derivations()

    # Initialise all triples in the knowledge graph
    utils.initialise_triples(generate_random_download_path, sparql_client, derivation_client)

    # Assert that there's currently no new experiment associated with the DoE instance
    assert sparql_client.getNewExperimentFromDoE(utils.cf.DOE_IRI) is None

    # Create derivation instance for new information, the timestamp of this derivation is 0
    derivation_iri = derivation_client.createAsyncDerivationForNewInfo(doe_agent.agentIRI, utils.cf.DERIVATION_INPUTS)
    logger.info(f"Initialised successfully, created asynchronous derivation instance: {derivation_iri}")

    # Query timestamp of the derivation for every 20 seconds until it's updated
    currentTimestamp_derivation = 0
    while currentTimestamp_derivation == 0:
        time.sleep(20)
        currentTimestamp_derivation = utils.get_timestamp(derivation_iri, sparql_client)

    # Query the iri of the new proposed NewExperiment
    new_exp_iri = sparql_client.getNewExperimentFromDoE(utils.cf.DOE_IRI)
    assert new_exp_iri is not None
    logger.info(f"New experiment suggested successfully, suggested experiment instance: {new_exp_iri}")
