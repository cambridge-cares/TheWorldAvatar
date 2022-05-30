import time

import vtexeagent.tests.utils as utils

import logging
logger = logging.getLogger('test_docker_integration')


def test_rxn_integration(generate_random_download_path, initialise_clients):
    sparql_client, derivation_client = initialise_clients

    # Verify that knowledge base is empty
    res = sparql_client.getAmountOfTriples()
    assert res == 0

    # Initialise all triples in the knowledge graph
    utils.initialise_triples(generate_random_download_path, sparql_client, derivation_client)

    # Assert that there's currently no new experiment associated with the DoE instance
    assert sparql_client.getNewExperimentFromDoE(utils.cf.DOE_IRI) is None

    # vtext_agent = utils.cf.create_vtexe_agent(utils.cf.VTEXEAGENT_ENV)
    # vtext_agent.start_monitoring_derivations()

    # Create derivation instance for new information, the timestamp of this derivation is 0
    doe_derivation_iri = derivation_client.createAsyncDerivationForNewInfo(utils.cf.DOEAGENT_SERVICE_IRI, utils.cf.DERIVATION_INPUTS)
    logger.info(f"Initialised successfully, created asynchronous doe derivation instance: {doe_derivation_iri}")
    exe_derivation_iri = derivation_client.createAsyncDerivationForNewInfo(utils.cf.VTEXEAGENT_SERVICE_IRI, [doe_derivation_iri])
    logger.info(f"Initialised successfully, created asynchronous exe derivation instance: {exe_derivation_iri}")

    time.sleep(600)
