from postprocagent.agent import *
from postprocagent.tests import conftest

from pathlib import Path
from rdflib import Graph
import pkgutil
import uuid
import time
import os

###############################################################
### !!! Do NOT run this script before reading README.md !!! ###
###############################################################
# Use the same IRIs that are used in the integration test
chemical_solution_iri = conftest.CHEMICAL_SOLUTION_1
hplc_digital_twin = conftest.HPLC_DIGITAL_TWIN_1
hplc_report_xls_path_in_pkg = conftest.HPLC_REPORT_XLS_PATH_IN_PKG
rxn_exp_iri = conftest.NEW_RXN_EXP_1_IRI
derivation_outputs = conftest.PLACEHOLDER_PERFORMANCE_INDICATOR_LIST_1

def exampleEntryPoint():
    """
        !!! Do NOT run this script before reading README.md !!!
        As the monitorDerivations() is set to be running periodically once the PostProc agent is deployed,
        this function serve as an example that creats a working case once the developer has confirmed
        there are no valuable data in the knowledge graph endpoints specified in the conf,
        i.e. this script deletes ALL existing triples and upload example triples to the endpoint, it
        then creates the derivation instance based on the example data and execute asynchronous
        derivation update automatically.
        Response:
            the created OntoDerivation:Derivation instance
    """

    config = PostProcAgentConfig(str(Path(__file__).absolute().parent) + '/postprocagent/conf/agent_properties.json')

    clearAll = """DELETE WHERE {?s ?p ?o}"""
    example_sparql_client = ChemistryAndRobotsSparqlClient(
        config.SPARQL_QUERY_ENDPOINT, config.SPARQL_UPDATE_ENDPOINT,
        fs_url=config.FILESERVER_URL,fs_user=config.FS_USERNAME,fs_pwd=config.FS_PASSWORD
    )
    example_sparql_client.performUpdate(clearAll)

    # Upload all relevant example triples provided in the resources folder of 'chemistry_and_robots' package to triple store
    for f in ['ontoagent/Service__PostProc.ttl', 'sample_data/new_exp_data.ttl', 'sample_data/duplicate_ontorxn.ttl',
        'sample_data/dummy_lab.ttl', 'sample_data/rxn_data.ttl', 'sample_data/dummy_post_proc.ttl']:
        data = pkgutil.get_data('chemistry_and_robots', 'resources/'+f).decode("utf-8")
        g = Graph().parse(data=data)
        filePath = f'{str(uuid.uuid4())}.ttl'
        g.serialize(filePath, format='ttl')
        example_sparql_client.uploadOntology(filePath)
        os.remove(filePath)

    # Instantiate PostProc Agent
    post_proc_agent = PostProcAgent(
        fs_url=config.FILESERVER_URL, fs_user=config.FS_USERNAME, fs_pwd=config.FS_PASSWORD,
        agent_iri=config.ONTOAGENT_SERVICE, time_interval=config.PERIODIC_TIMESCALE,
        derivation_instance_base_url=config.DERIVATION_INSTANCE_BASE_URL,
        kg_url=config.SPARQL_QUERY_ENDPOINT,
        kg_user=config.KG_USERNAME,kg_password=config.KG_PASSWORD,
        logger_name='prod'
    )

    # Upload HPLC report to file server
    temp_local_file_path = f'{str(uuid.uuid4())}.xls'
    data = pkgutil.get_data('chemistry_and_robots', 'resources/'+hplc_report_xls_path_in_pkg)
    with open(temp_local_file_path, 'wb') as file_obj:
        file_obj.write(data)
    timestamp_last_modified = os.path.getmtime(temp_local_file_path)
    hplc_report_iri = example_sparql_client.upload_raw_hplc_report_to_fs_kg(local_file_path=temp_local_file_path,
        timestamp_last_modified=timestamp_last_modified, hplc_digital_twin=hplc_digital_twin)
    os.remove(temp_local_file_path)

    # Make the connection between HPLCReport and ChemicalSolution
    # In normal operation, this should be done as part of Execution Agent
    example_sparql_client.connect_hplc_report_with_chemical_solution(hplc_report_iri, chemical_solution_iri)

    # Construct derivation_inputs with the iri of HPLCReport
    derivation_inputs = [hplc_report_iri]

    # Create derivation instance given above information, the timestamp of this derivation is 0
    derivation_iri = post_proc_agent.derivationClient.createAsynDerivation(derivation_outputs, post_proc_agent.agentIRI, derivation_inputs)

    # Check if the derivation instance is created correctly
    assert example_sparql_client.checkInstanceClass(derivation_iri, conftest.ONTODERIVATION_DERIVATIONASYN)
    print(f'Initialised successfully, created derivation instance: <{derivation_iri}>')

    # Iterate over the list of inputs to add and update the timestamp
    for input in derivation_inputs:
        post_proc_agent.derivationClient.addTimeInstance(input)
        # Update timestamp is needed as the timestamp added using addTimeInstance() is 0
        post_proc_agent.derivationClient.updateTimestamp(input)

    # Update the asynchronous derivation, it will be marked as "Requested"
    # The actual update will be handled by monitorDerivation method periodically run by PostProc agent
    post_proc_agent.derivationClient.updateDerivationAsyn(derivation_iri)

    # Query timestamp of the derivation for every 20 seconds until it's updated
    currentTimestamp_derivation = 0
    query_timestamp = """SELECT ?time WHERE { <%s> <%s>/<%s>/<%s> ?time .}""" % (
        derivation_iri, conftest.TIME_HASTIME, conftest.TIME_INTIMEPOSITION, conftest.TIME_NUMERICPOSITION)
    while currentTimestamp_derivation == 0:
        time.sleep(20)
        currentTimestamp_derivation = int(example_sparql_client.performQuery(query_timestamp)[0]['time'])
        print("The current timestamp for the derivation <%s> is %d" % (derivation_iri, currentTimestamp_derivation))

    # Wait some arbitrary time until the cleaning up is done by the derivation client
    time.sleep(20)

    # Query the new derived IRI
    query_new_derived_iri = """SELECT ?new_iri WHERE {?new_iri <%s> <%s>.}""" % (conftest.ONTODERIVATION_BELONGSTO, derivation_iri)
    response = example_sparql_client.performQuery(query_new_derived_iri)
    new_derived_iri = [list(r.values())[0] for r in response]

    # Check the new populated instances of PerformanceIndicators are different from the original derivation outputs
    assert all([iri not in derivation_outputs for iri in new_derived_iri])

    # Reload the ReactionExperiment instance and check all its information (OutputChemical and PerformanceIndicator) are uploaded and parsed correctly
    reload_rxn_rxp_instance = example_sparql_client.getReactionExperiment(rxn_exp_iri)[0]
    reload_pi_lst = [pi.instance_iri for pi in reload_rxn_rxp_instance.hasPerformanceIndicator]
    assert all([iri in reload_pi_lst for iri in new_derived_iri])
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

    # If all previous codes are executed without error, we can assume that the update was successful
    return "All checks passed."

if __name__ == '__main__':
    !!! Do NOT run this script before reading README.md !!! # comment out this line before running this script (make sure you have read README.md)
    msg = exampleEntryPoint()
    print(msg)
