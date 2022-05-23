import agilentpostprocagent.tests.conftest as conftest
import logging
import pytest
import time

logging.getLogger("py4j").setLevel(logging.INFO)

pytest_plugins = ["docker_compose"]

@pytest.mark.parametrize(
    "rxn_exp_iri,report_path_in_pkg,hplc_digital_twin,chemical_solution_iri",
    [
        (conftest.NEW_RXN_EXP_1_IRI, conftest.HPLC_REPORT_XLS_PATH_IN_PKG, conftest.HPLC_DIGITAL_TWIN_1, conftest.CHEMICAL_SOLUTION_1),
        (conftest.NEW_RXN_EXP_2_IRI, conftest.HPLC_REPORT_TXT_PATH_IN_PKG, conftest.HPLC_DIGITAL_TWIN_2, conftest.CHEMICAL_SOLUTION_2),
    ],
)
def test_post_proc_agent(initialise_triples, retrieve_hplc_report, rxn_exp_iri, report_path_in_pkg, hplc_digital_twin, chemical_solution_iri):
    sparql_client, post_proc_agent = initialise_triples

    # Verify that knowledge base is NOT empty
    res = sparql_client.getAmountOfTriples()
    assert res > 0

    # Start the agent to monitor the derivations
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

    # Iterate over the list of inputs to add and update the timestamp
    for input in derivation_inputs:
        post_proc_agent.derivationClient.addTimeInstance(input)
        # Update timestamp is needed as the timestamp added using addTimeInstance() is 0
        post_proc_agent.derivationClient.updateTimestamp(input)

    # Create derivation instance given above information, the timestamp of this derivation is 0
    derivation_iri = post_proc_agent.derivationClient.createAsyncDerivationForNewInfo(post_proc_agent.agentIRI, derivation_inputs)

    # Check if the derivation instance is created correctly
    assert sparql_client.checkInstanceClass(derivation_iri, conftest.ONTODERIVATION_DERIVATIONASYN)

    # Query timestamp of the derivation for every 20 seconds until it's updated
    currentTimestamp_derivation = 0
    query_timestamp = """SELECT ?time WHERE { <%s> <%s>/<%s>/<%s> ?time .}""" % (
        derivation_iri, conftest.TIME_HASTIME, conftest.TIME_INTIMEPOSITION, conftest.TIME_NUMERICPOSITION)
    while currentTimestamp_derivation == 0:
        time.sleep(20)
        currentTimestamp_derivation = int(sparql_client.performQuery(query_timestamp)[0]['time'])

    # Wait some arbitrary time until the cleaning up is done by the derivation client
    time.sleep(20)

    # Query the new derived IRI
    query_new_derived_iri = """SELECT ?new_iri WHERE {?new_iri <%s> <%s>.}""" % (conftest.ONTODERIVATION_BELONGSTO, derivation_iri)
    response = sparql_client.performQuery(query_new_derived_iri)
    new_derived_iri = [list(r.values())[0] for r in response]

    # Reload the ReactionExperiment instance and check all its information (OutputChemical and PerformanceIndicator) are uploaded and parsed correctly
    reload_rxn_rxp_instance = sparql_client.getReactionExperiment(rxn_exp_iri)[0]
    reload_pi_lst = [pi.instance_iri for pi in reload_rxn_rxp_instance.hasPerformanceIndicator]
    assert all([iri in new_derived_iri for iri in reload_pi_lst])
    for pi in reload_rxn_rxp_instance.hasPerformanceIndicator:
        assert pi.hasValue.hasUnit is not None
        assert pi.hasValue.hasNumericalValue is not None
    reload_output_chemical_lst = reload_rxn_rxp_instance.hasOutputChemical
    for oc in reload_output_chemical_lst:
        assert oc.clz == conftest.ONTORXN_OUTPUTCHEMICAL
        assert oc.instance_iri is not None
        reload_phase_comp_lst = oc.thermodynamicBehaviour.isComposedOfSubsystem
        for phase_comp in reload_phase_comp_lst:
            assert phase_comp.representsOccurenceOf is not None
            assert phase_comp.hasProperty.hasValue.hasUnitOfMeasure is not None
            assert phase_comp.hasProperty.hasValue.numericalValue is not None
        reload_phase_comp_conc_lst = [pc.hasProperty for pc in oc.thermodynamicBehaviour.isComposedOfSubsystem]
        reload_conc_lst = oc.thermodynamicBehaviour.has_composition.comprisesDirectly
        assert all([conc in reload_phase_comp_conc_lst for conc in reload_conc_lst])
        assert all([conc in reload_conc_lst for conc in reload_phase_comp_conc_lst])

    # Shutdown the scheduler to clean up before the next test
    post_proc_agent.scheduler.shutdown()
