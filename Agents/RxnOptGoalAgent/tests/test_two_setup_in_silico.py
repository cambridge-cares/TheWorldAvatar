import pytest
import time
import json
import os

import tests.conftest as cf
# do two-setup test

@pytest.mark.parametrize(
    "lab1_vapourtec_env_file,lab1_hplc_env_file,lab2_vapourtec_env_file,lab2_hplc_env_file,derivation_inputs",
    [
        (cf.LAB1_VAPOURTEC_AGENT_ENV, cf.LAB1_HPLC_AGENT_ENV, cf.LAB2_VAPOURTEC_AGENT_ENV, cf.LAB2_HPLC_AGENT_ENV, cf.IRIs.DERIVATION_INPUTS.value),
    ],
)
def _test_two_setup_rogi(
    initialise_blazegraph_fileserver_with_test_triples,
    create_rogi_agent, create_doe_agent, create_vapourtec_schedule_agent, create_hplc_postpro_agent, create_vapourtec_agent, create_hplc_agent,
    lab1_vapourtec_env_file, lab1_hplc_env_file, lab2_vapourtec_env_file, lab2_hplc_env_file, derivation_inputs,
):
    sparql_client, derivation_client = initialise_blazegraph_fileserver_with_test_triples

    # Create agent instances, this also register the agents to the KG
    # NOTE that this should be done by agent themselves at real deployment
    rogi_agent = create_rogi_agent(
        register_agent=True,
    )
    doe_agent = create_doe_agent(
        register_agent=True,
    )
    vapourtec_schedule_agent = create_vapourtec_schedule_agent(
        register_agent=True,
    )
    hplc_postpro_agent = create_hplc_postpro_agent(
        register_agent=True,
    )
    lab1_vapourtec_agent = create_vapourtec_agent(
        env_file=lab1_vapourtec_env_file,
        register_agent=True,
        fcexp_file_container_folder=cf.LAB1_DIR,
        dry_run=True,
    )
    # Set the hplc_report_container_dir to be hplc_report_target_folder (on host machine) if it's for local_agent_test
    lab1_hplc_agent = create_hplc_agent(
        env_file=lab1_hplc_env_file,
        register_agent=True,
        hplc_report_container_dir=cf.LAB1_DIR,
        dry_run=True,
    )

    lab2_vapourtec_agent = create_vapourtec_agent(
        env_file=lab2_vapourtec_env_file,
        register_agent=True,
        fcexp_file_container_folder=cf.LAB2_DIR,
        dry_run=True,
    )
    lab2_hplc_agent = create_hplc_agent(
        env_file=lab2_hplc_env_file,
        register_agent=True,
        hplc_report_container_dir=cf.LAB2_DIR,
        dry_run=True,
    )

    # Start the scheduler to monitor derivations if it's local agent test
    rogi_agent.start_all_periodical_job()
    doe_agent.start_all_periodical_job()
    vapourtec_schedule_agent.start_all_periodical_job()
    hplc_postpro_agent.start_all_periodical_job()
    lab1_vapourtec_agent._start_monitoring_derivations()
    lab1_hplc_agent.start_all_periodical_job()
    lab1_vapourtec_agent.sparql_client.update_vapourtec_rs400_state(lab1_vapourtec_agent.vapourtec_digital_twin, cf.ONTOVAPOURTEC_IDLE, time.time())

    lab2_vapourtec_agent._start_monitoring_derivations()
    lab2_hplc_agent.start_all_periodical_job()
    lab2_vapourtec_agent.sparql_client.update_vapourtec_rs400_state(lab2_vapourtec_agent.vapourtec_digital_twin, cf.ONTOVAPOURTEC_IDLE, time.time())

    # Add timestamp to the derivation_inputs
    for input in derivation_inputs:
        derivation_client.addTimeInstance(input)
        derivation_client.updateTimestamp(input)

    # Create derivation instance for new information, the timestamp of this derivation is 0
    rogi_derivation_1 = derivation_client.createAsyncDerivationForNewInfo(rogi_agent.agentIRI, derivation_inputs)
    print(f"Initialised successfully, created asynchronous derivation instance for ROGI agent: {rogi_derivation_1}")

    time.sleep(60)
    rogi_derivation_2 = derivation_client.createAsyncDerivationForNewInfo(rogi_agent.agentIRI, derivation_inputs)
    print(f"Initialised successfully, created asynchronous derivation instance for ROGI agent: {rogi_derivation_1}")

    time.sleep(3000)


@pytest.mark.parametrize(
    "lab1_vapourtec_env_file,lab1_hplc_env_file,lab2_vapourtec_env_file,lab2_hplc_env_file,derivation_inputs",
    [
        (cf.LAB1_VAPOURTEC_AGENT_ENV, cf.LAB1_HPLC_AGENT_ENV, cf.LAB2_VAPOURTEC_AGENT_ENV, cf.LAB2_HPLC_AGENT_ENV, cf.IRIs.DERIVATION_INPUTS.value),
    ],
)
def test_two_setup_rog(
    initialise_blazegraph_fileserver_with_test_triples, create_rog_agent,
    create_rogi_agent, create_doe_agent, create_vapourtec_schedule_agent, create_hplc_postpro_agent, create_vapourtec_agent, create_hplc_agent,
    lab1_vapourtec_env_file, lab1_hplc_env_file, lab2_vapourtec_env_file, lab2_hplc_env_file, derivation_inputs,
):
    sparql_client, derivation_client = initialise_blazegraph_fileserver_with_test_triples

    # Create agent instances, this also register the agents to the KG
    # NOTE that this should be done by agent themselves at real deployment
    rogi_agent = create_rogi_agent(
        register_agent=True,
    )
    doe_agent = create_doe_agent(
        register_agent=True,
    )
    vapourtec_schedule_agent = create_vapourtec_schedule_agent(
        register_agent=True,
    )
    hplc_postpro_agent = create_hplc_postpro_agent(
        register_agent=True,
    )
    lab1_vapourtec_agent = create_vapourtec_agent(
        env_file=lab1_vapourtec_env_file,
        register_agent=True,
        fcexp_file_container_folder=os.path.join(cf.THIS_DIR, '_lab1'),
        dry_run=True,
    )
    # Set the hplc_report_container_dir to be hplc_report_target_folder (on host machine) if it's for local_agent_test
    lab1_hplc_agent = create_hplc_agent(
        env_file=lab1_hplc_env_file,
        register_agent=True,
        hplc_report_container_dir=os.path.join(cf.THIS_DIR, '_lab1'),
        dry_run=True,
    )

    lab2_vapourtec_agent = create_vapourtec_agent(
        env_file=lab2_vapourtec_env_file,
        register_agent=True,
        fcexp_file_container_folder=os.path.join(cf.THIS_DIR, '_lab2'),
        dry_run=True,
    )
    lab2_hplc_agent = create_hplc_agent(
        env_file=lab2_hplc_env_file,
        register_agent=True,
        hplc_report_container_dir=os.path.join(cf.THIS_DIR, '_lab2'),
        dry_run=True,
    )

    rog_agent = create_rog_agent(
        goal_iter_agent_iri=rogi_agent.agentIRI,
    )

    # assert that no derivations isDerivedUsing the rogi agent yet
    assert not rog_agent.sparql_client.check_if_triple_exist(
        None, cf.ONTODERIVATION_ISDERIVEDUSING, rogi_agent.agentIRI
    )

    # Start the scheduler to monitor derivations if it's local agent test
    rogi_agent.start_all_periodical_job()
    doe_agent.start_all_periodical_job()
    vapourtec_schedule_agent.start_all_periodical_job()
    hplc_postpro_agent.start_all_periodical_job()
    lab1_vapourtec_agent._start_monitoring_derivations()
    lab1_hplc_agent.start_all_periodical_job()
    lab1_vapourtec_agent.sparql_client.update_vapourtec_rs400_state(lab1_vapourtec_agent.vapourtec_digital_twin, cf.ONTOVAPOURTEC_IDLE, time.time())

    lab2_vapourtec_agent._start_monitoring_derivations()
    lab2_hplc_agent.start_all_periodical_job()
    lab2_vapourtec_agent.sparql_client.update_vapourtec_rs400_state(lab2_vapourtec_agent.vapourtec_digital_twin, cf.ONTOVAPOURTEC_IDLE, time.time())

    # TODO delete below - no need to do this, as the derivation inputs will be generated upon goal request
    # # Add timestamp to the derivation_inputs
    # for input in derivation_inputs:
    #     derivation_client.addTimeInstance(input)
    #     derivation_client.updateTimestamp(input)

    # send goal request to rog agent
    # sample_goal_request = cf.sample_goal_request
    sample_goal_request = {
        "chem_rxn": "https://www.example.com/triplestore/testlab/chem_rxn/ChemRxn_1",
        "cycleAllowance": 20,
        "deadline": "2022-12-12T17:05",
        "first_goal_clz": "https://raw.githubusercontent.com/cambridge-cares/TheWorldAvatar/main/JPS_Ontology/ontology/ontoreaction/OntoReaction.owl#Yield",
        "first_goal_desires": "https://raw.githubusercontent.com/cambridge-cares/TheWorldAvatar/main/JPS_Ontology/ontology/ontogoal/OntoGoal.owl#desiresGreaterThan",
        "first_goal_num_val": 99,
        "first_goal_unit": "http://www.ontology-of-units-of-measure.org/resource/om-2/percent",
        "rxn_opt_goal_plan": "http://www.theworldavatar.com/resource/plans/RxnOpt/rxnoptplan",
        "second_goal_clz": "https://raw.githubusercontent.com/cambridge-cares/TheWorldAvatar/main/JPS_Ontology/ontology/ontoreaction/OntoReaction.owl#RunMaterialCost",
        "second_goal_desires": "https://raw.githubusercontent.com/cambridge-cares/TheWorldAvatar/main/JPS_Ontology/ontology/ontogoal/OntoGoal.owl#desiresLessThan",
        "second_goal_num_val": 0.001,
        "second_goal_unit": "http://www.ontology-of-units-of-measure.org/resource/om-2/poundSterlingPerKilogram",
        "labs": ['http://example.com/blazegraph/namespace/testlab/lab1/Laboratory_Dummy', 'http://example.com/blazegraph/namespace/testlab/lab2/Laboratory_Dummy']
    }

    with rog_agent.app.test_client() as test_client:
        r = test_client.post(rog_agent.goal_agent_endpoint, data=sample_goal_request)
        j = json.loads(r.text)
        rogi_derivation_lst = j[rog_agent.GOAL_SPECS_RESPONSE_KEY]
        assert rogi_derivation_lst is not None
        assert len(rogi_derivation_lst) > 0
        # assert the rogi derivation is created correctly and isDerivedUsing the rogi agent
        # it's important to distinguish it with rog_agent.goal_agent_iri
        for rogi_derivation in rogi_derivation_lst:
            assert rog_agent.sparql_client.check_if_triple_exist(
                rogi_derivation, cf.ONTODERIVATION_ISDERIVEDUSING, rogi_agent.agentIRI
            )

        time.sleep(3600*24)
