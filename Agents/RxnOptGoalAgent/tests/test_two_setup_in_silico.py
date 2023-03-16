from datetime import datetime
import pytest
import time
import json
import copy
import os

import tests.conftest as cf

from py4jps import agentlogging
logger = agentlogging.get_logger('dev')


# override the sample_goal_request with cycleAllowance = 2, deadline to be 2 hrs from now
_sample_goal_request = copy.deepcopy(cf.sample_goal_request)
_sample_goal_request['cycleAllowance'] = 6
_sample_goal_request['deadline'] = str(datetime.fromtimestamp(int(time.time()) + 2 * 60 * 60).isoformat())

# 1. both labs should be participating in the test, so we keep them as they are
# 2. the hplc_report_container_dir should be updated to the correct path
hplc_report_target_folder = cf.HPLC_REPORT_LOCAL_TEST_DIR
# 3. the vapourtec_ip_address is not needed as we will not connect to the real vapourtec
# instead, we just update the vapourtec_digital_twin to be idle state
# 4. here we do local agent test
local_agent_test = True

@pytest.mark.parametrize(
    "lab1_vapourtec_env_file, lab1_hplc_env_file, lab1_dir, lab2_vapourtec_env_file, lab2_hplc_env_file, lab2_dir, goal_request",
    [
        (
            cf.LAB1_VAPOURTEC_AGENT_ENV,
            cf.LAB1_HPLC_AGENT_ENV,
            cf.LAB1_DIR,
            cf.LAB2_VAPOURTEC_AGENT_ENV,
            cf.LAB2_HPLC_AGENT_ENV,
            cf.LAB2_DIR,
            _sample_goal_request,
        ),
    ],
)
def test_two_setup_rog_LOCAL(
    initialise_blazegraph_fileserver_with_test_triples, create_rog_agent,
    create_rogi_agent, create_doe_agent, create_vapourtec_schedule_agent, create_hplc_postpro_agent, create_vapourtec_agent, create_hplc_agent,
    lab1_vapourtec_env_file, lab1_hplc_env_file, lab1_dir, lab2_vapourtec_env_file, lab2_hplc_env_file, lab2_dir, goal_request,
):
    sparql_client, derivation_client = initialise_blazegraph_fileserver_with_test_triples

    # Create agent instances, this also register the agents to the KG
    # NOTE that this should be done by agent themselves at real deployment
    rogi_agent = create_rogi_agent(
        register_agent=True,
    )
    logger.info(f"ROGI Agent created: {rogi_agent.agentIRI}")
    doe_agent = create_doe_agent(
        register_agent=True,
    )
    logger.info(f"DoE Agent created: {doe_agent.agentIRI}")
    vapourtec_schedule_agent = create_vapourtec_schedule_agent(
        register_agent=True,
    )
    logger.info(f"VapourtecSchedule Agent created: {vapourtec_schedule_agent.agentIRI}")
    hplc_postpro_agent = create_hplc_postpro_agent(
        register_agent=True,
    )
    logger.info(f"HPLCPostPro Agent created: {hplc_postpro_agent.agentIRI}")
    lab1_vapourtec_agent = create_vapourtec_agent(
        env_file=lab1_vapourtec_env_file,
        register_agent=True,
        fcexp_file_container_folder=lab1_dir,
        dry_run=True,
    )
    logger.info(f"Lab1 Vapourtec Agent created: {lab1_vapourtec_agent.agentIRI}")
    # Set the hplc_report_container_dir to be hplc_report_target_folder (on host machine) if it's for local_agent_test
    lab1_hplc_agent = create_hplc_agent(
        env_file=lab1_hplc_env_file,
        register_agent=True,
        hplc_report_container_dir=lab1_dir,
        dry_run=True,
    )
    logger.info(f"Lab1 HPLC Agent created: {lab1_hplc_agent.agentIRI}")

    lab2_vapourtec_agent = create_vapourtec_agent(
        env_file=lab2_vapourtec_env_file,
        register_agent=True,
        fcexp_file_container_folder=lab2_dir,
        dry_run=True,
    )
    lab2_hplc_agent = create_hplc_agent(
        env_file=lab2_hplc_env_file,
        register_agent=True,
        hplc_report_container_dir=lab2_dir,
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

    with rog_agent.app.test_client() as test_client:
        r = test_client.post(rog_agent.goal_agent_endpoint, data=goal_request)
        j = json.loads(r.text)
        rogi_derivation_lst = j[rog_agent.GOAL_SPECS_RESPONSE_KEY]
        logger.debug(f"rogi_derivation_lst: {rogi_derivation_lst}")
        goal_set_iri = j[rog_agent.GOAL_SET_IRI_KEY]
        logger.debug(f"goal_set_iri: {goal_set_iri}")
        assert len(rogi_derivation_lst) == len(goal_request['labs'])
        # assert the rogi derivation is created correctly and isDerivedUsing the rogi agent
        # it's important to distinguish it with rog_agent.goal_agent_iri
        for rogi_derivation in rogi_derivation_lst:
            assert rog_agent.sparql_client.check_if_triple_exist(
                rogi_derivation, cf.ONTODERIVATION_ISDERIVEDUSING, rogi_agent.agentIRI
            )

        # Wait until the cycleAllowance is 0
        cycle_allowance = goal_request['cycleAllowance']
        while cycle_allowance > 0:
            time.sleep(60)
            cycle_allowance = int(sparql_client.performQuery(
                f"SELECT ?cycleAllowance WHERE {{ <{goal_set_iri}> <{cf.ONTOGOAL_HASRESTRICTION}>/<{cf.ONTOGOAL_CYCLEALLOWANCE}> ?cycleAllowance . }}"
            )[0]['cycleAllowance'])
            print(f"Goal Set {goal_set_iri} has cycleAllowance {cycle_allowance}, current time: {time.time()}")

        # Wait until both rogi derivation is done
        both_rogi_completed_iter = False
        while not both_rogi_completed_iter:
            time.sleep(10)
            _bool_lst = []
            for rogi_derivation in rogi_derivation_lst:
                rogi_completed_iter = sparql_client.check_if_rogi_complete_one_iter(rogi_derivation)
                _bool_lst.append(rogi_completed_iter)
                print(f"ROGI derivation {rogi_derivation} is completed: {rogi_completed_iter}, current time: {time.time()}")
            both_rogi_completed_iter = all(_bool_lst)

        # Shutdown the scheduler to clean up if it's local agent test (as the agent scheduler must have started)
        if local_agent_test:
            rogi_agent.scheduler.shutdown()
            doe_agent.scheduler.shutdown()
            vapourtec_schedule_agent.scheduler.shutdown()
            hplc_postpro_agent.scheduler.shutdown()
            lab1_vapourtec_agent.scheduler.shutdown()
            lab1_hplc_agent.scheduler.shutdown()
            lab2_vapourtec_agent.scheduler.shutdown()
            lab2_hplc_agent.scheduler.shutdown()

    try:
        # Test all triples are correctly generated
        cf.assert_rxn_iterations(
            sparql_client,
            doe_agent,
            vapourtec_schedule_agent,
            [lab1_vapourtec_agent, lab2_vapourtec_agent],
            [lab1_hplc_agent, lab2_hplc_agent],
            hplc_postpro_agent,
            rogi_agent,
            goal_set_iri,
        )
        logger.debug(f"All tests passed.")
    except Exception as e:
        print(e)
        raise e
