from chemistry_and_robots.data_model.base_ontology import ONTODERIVATION_BELONGSTO # TODO clean this up once the iri is moved to correct place
import postprocagent.tests.conftest as conftest
import logging
import pytest
import time
import os

logging.getLogger("py4j").setLevel(logging.INFO)

from pyasyncagent.data_model.iris import *

pytest_plugins = ["docker_compose"]

@pytest.mark.parametrize(
    "report_path_in_pkg,hplc_digital_twin,chemical_solution_iri,derivation_outputs",
    [
        (conftest.HPLC_REPORT_XLS_PATH_IN_PKG, conftest.HPLC_DIGITAL_TWIN_1, conftest.CHEMICAL_SOLUTION_1, conftest.PLACEHOLDER_PERFORMANCE_INDICATOR_LIST_1),
        # (conftest.HPLC_REPORT_TXT_PATH_IN_PKG, conftest.HPLC_DIGITAL_TWIN_2, conftest.CHEMICAL_SOLUTION_2, conftest.PLACEHOLDER_PERFORMANCE_INDICATOR_LIST_2),
    ],
)
def test_post_proc_agent(initialise_triples, retrieve_hplc_report, report_path_in_pkg, hplc_digital_twin, chemical_solution_iri, derivation_outputs):
    sparql_client, post_proc_agent = initialise_triples

    # Verify that knowledge base is NOT empty
    res = sparql_client.getAmountOfTriples()
    assert res > 0

    post_proc_agent.start_monitoring_derivations()

    # Upload HPLC report to file server
    local_file_path, timestamp_last_modified = retrieve_hplc_report(report_path_in_pkg)
    hplc_report_iri = sparql_client.upload_raw_hplc_report_to_fs_kg(local_file_path=local_file_path,
        timestamp_last_modified=timestamp_last_modified, hplc_digital_twin=hplc_digital_twin)

    # Make the connection between HPLCReport and ChemicalSolution
    # In normal operation, this should be done as part of Execution Agent
    sparql_client.connect_hplc_report_with_chemical_solution(hplc_report_iri, chemical_solution_iri)

    # Construct derivation_inputs with the iri of HPLCReport
    derivation_inputs = [hplc_report_iri]

    # Create derivation instance given above information, the timestamp of this derivation is 0
    derivation_iri = post_proc_agent.derivationClient.createAsynDerivation(derivation_outputs, post_proc_agent.agentIRI, derivation_inputs)

    # Check if the derivation instance is created correctly
    assert sparql_client.checkInstanceClass(derivation_iri, ONTODERIVATION_DERIVATIONASYN)

    # Iterate over the list of inputs to add and update the timestamp
    for input in derivation_inputs:
        print(input)
        post_proc_agent.derivationClient.addTimeInstance(input)
        # Update timestamp is needed as the timestamp added using addTimeInstance() is 0
        post_proc_agent.derivationClient.updateTimestamp(input)

    # Update the asynchronous derivation, it will be marked as "PendingUpdate"
    # The actual update will be handled by monitorDerivation method periodically run by DoE agent
    post_proc_agent.derivationClient.updateDerivationAsyn(derivation_iri)

    # Query timestamp of the derivation for every 20 seconds until it's updated
    currentTimestamp_derivation = 0
    query_timestamp = """SELECT ?time WHERE { <%s> <%s>/<%s>/<%s> ?time .}""" % (
        derivation_iri, TIME_HASTIME, TIME_INTIMEPOSITION, TIME_NUMERICPOSITION)
    while currentTimestamp_derivation == 0:
        time.sleep(20)
        currentTimestamp_derivation = sparql_client.performQuery(query_timestamp)[0]['time']

    # Wait some arbitrary time until the cleaning up is done by the derivation client
    time.sleep(20)

    # Query the new derived IRI
    query_new_derived_iri = """SELECT ?new_iri WHERE {?new_iri <%s> <%s>.}""" % (ONTODERIVATION_BELONGSTO, derivation_iri)
    response = sparql_client.performQuery(query_new_derived_iri)
    new_derived_iri = [list(r.values())[0] for r in response]

    print('=====================================================================================================')
    time.sleep(6000)
    # Check the new generated instance NewExperiment is different from the original one provided in the example
    assert all([iri not in derivation_outputs for iri in new_derived_iri])

    # Shutdown the scheduler to clean up before the next test
    # post_proc_agent.scheduler.shutdown()
