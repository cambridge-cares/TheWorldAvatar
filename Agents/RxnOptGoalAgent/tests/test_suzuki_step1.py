from datetime import datetime
import pytest
import time
import json
import copy

import tests.conftest as cf

from py4jps import agentlogging
logger = agentlogging.get_logger('dev')


# override the suzuki_goal_request with cycleAllowance = 2, deadline to be 2 hrs from now
_sample_goal_request = copy.deepcopy(cf.suzuki_goal_request)
_sample_goal_request['cycleAllowance'] = 2
_sample_goal_request['deadline'] = str(datetime.fromtimestamp(int(time.time()) + 2 * 60 * 60).isoformat())

# 1. the available labs should also be updated to only include the physical lab that to be tested
# possible choice for available labs:
lab_to_test = 'https://www.theworldavatar.com/kg/lab_auto/lab1/Laboratory_Dummy'
_sample_goal_request['labs'] = [lab_to_test]
# 2. the hplc_report_container_dir should be updated to the correct path
hplc_report_target_folder = cf.HPLC_REPORT_LOCAL_TEST_DIR
# 3. the vapourtec_ip_address is not needed as we will not connect to the real vapourtec
# instead, we just update the vapourtec_digital_twin to be idle state
# 4. here we do local agent test
local_agent_test = True

@pytest.mark.parametrize(
    "test_triples_folder, vapourtec_agent_env_file, fcexp_file_container_folder, hplc_agent_env_file, hplc_report_target_folder, local_agent_test, goal_request, hplc_method",
    [
        (
            cf.TEST_TRIPLES_SUZUKI_STEP1_DIR,
            cf.IRIs.VAPOURTEC_ENV_FILE_DICT.value[lab_to_test],
            cf.FCEXP_FILE_DIR,
            cf.IRIs.HPLC_ENV_FILE_DICT.value[lab_to_test],
            hplc_report_target_folder,
            local_agent_test,
            _sample_goal_request,
            cf.IRIs.HPLC_METHOD_SUZUKI.value,
        ),
    ],
)
def test_suzuki_rxn_goal_iterations_LOCAL(
    initialise_blazegraph_fileserver_with_test_triples,
    create_rog_agent, create_rogi_agent, create_doe_agent, create_vapourtec_schedule_agent, create_hplc_postpro_agent, create_vapourtec_agent, create_hplc_agent,
    test_triples_folder, vapourtec_agent_env_file, fcexp_file_container_folder, hplc_agent_env_file, hplc_report_target_folder, local_agent_test, goal_request, hplc_method,
):
    sparql_client, derivation_client = initialise_blazegraph_fileserver_with_test_triples(test_triples_folder)

    # Create agent instances, this also register the agents to the KG
    # NOTE that this should be done by agent themselves at real deployment
    rogi_agent = create_rogi_agent(
        register_agent=True,
    )
    logger.debug(f"rogi_agent created")
    rog_agent = create_rog_agent(
        goal_iter_agent_iri=rogi_agent.agentIRI,
    )
    logger.debug(f"rog_agent created")
    doe_agent = create_doe_agent(
        register_agent=True,
    )
    logger.debug(f"doe_agent created")
    vapourtec_schedule_agent = create_vapourtec_schedule_agent(
        register_agent=True,
    )
    logger.debug(f"vapourtec_schedule_agent created")
    hplc_postpro_agent = create_hplc_postpro_agent(
        register_agent=True,
    )
    logger.debug(f"hplc_postpro_agent created")
    vapourtec_agent = create_vapourtec_agent(
        env_file=vapourtec_agent_env_file,
        register_agent=True,
        fcexp_file_container_folder=fcexp_file_container_folder,
        dry_run=True,
    )
    logger.debug(f"vapourtec_agent created")
    # Set the hplc_report_container_dir to be hplc_report_target_folder (on host machine) if it's for local_agent_test
    hplc_agent = create_hplc_agent(
        env_file=hplc_agent_env_file,
        register_agent=True,
        hplc_report_container_dir=hplc_report_target_folder,
        dry_run=True,
        hplc_method=hplc_method,
    )
    logger.debug(f"hplc_agent created")

    # Start the scheduler to monitor derivations if it's local agent test
    if local_agent_test:
        rogi_agent.start_all_periodical_job()
        doe_agent.start_all_periodical_job()
        vapourtec_schedule_agent.start_all_periodical_job()
        hplc_postpro_agent.start_all_periodical_job()
        hplc_agent.start_all_periodical_job()
        # NOTE here we do not start_all_periodical_job for vapourtec_agent as it's we don't want to connect to the real vapourtec
        # instead, we just update the vapourtec_digital_twin to be idle state, which will then be updated to DryRunState during the test
        vapourtec_agent._start_monitoring_derivations()
        vapourtec_agent.sparql_client.update_vapourtec_rs400_state(vapourtec_agent.vapourtec_digital_twin, cf.ONTOVAPOURTEC_IDLE, time.time())

    with rog_agent.app.test_client() as test_client:
        r = test_client.post(rog_agent.goal_agent_endpoint, data=goal_request)
        j = json.loads(r.text)
        rogi_derivation_lst = j[rog_agent.GOAL_SPECS_RESPONSE_KEY]
        logger.debug(f"rogi_derivation_lst: {rogi_derivation_lst}")
        goal_set_iri = j[rog_agent.GOAL_SET_IRI_KEY]
        logger.debug(f"goal_set_iri: {goal_set_iri}")
        assert len(rogi_derivation_lst) == 1
        rogi_derivation_iri = rogi_derivation_lst[0]

        # Wait until the cycleAllowance is 0
        cycle_allowance = goal_request['cycleAllowance']
        while cycle_allowance > 0:
            time.sleep(60)
            cycle_allowance = int(sparql_client.performQuery(
                f"SELECT ?cycleAllowance WHERE {{ <{goal_set_iri}> <{cf.ONTOGOAL_HASRESTRICTION}>/<{cf.ONTOGOAL_CYCLEALLOWANCE}> ?cycleAllowance . }}"
            )[0]['cycleAllowance'])
            print(f"Goal Set {goal_set_iri} has cycleAllowance {cycle_allowance}, current time: {time.time()}")

        # Wait until the rogi derivation is done
        rogi_completed_one_iter = False
        while not rogi_completed_one_iter:
            time.sleep(10)
            rogi_completed_one_iter = sparql_client.check_if_rogi_complete_one_iter(rogi_derivation_iri)
            print(f"ROGI derivation {rogi_derivation_iri} is completed: {rogi_completed_one_iter}, current time: {time.time()}")

        # Remove the periodic job monitoring the goal set
        rog_agent.scheduler.remove_job(f'monitor_goal_set__{cf.getShortName(goal_set_iri)}')
        # Shutdown the scheduler to clean up if it's local agent test (as the agent scheduler must have started)
        if local_agent_test:
            rogi_agent.scheduler.shutdown()
            doe_agent.scheduler.shutdown()
            vapourtec_schedule_agent.scheduler.shutdown()
            hplc_postpro_agent.scheduler.shutdown()
            vapourtec_agent.scheduler.shutdown()
            hplc_agent.scheduler.shutdown()

    try:
        # Test all triples are correctly generated
        cf.assert_rxn_iterations(
            sparql_client,
            doe_agent,
            vapourtec_schedule_agent,
            vapourtec_agent,
            hplc_agent,
            hplc_postpro_agent,
            rogi_agent,
            goal_set_iri,
        )
        logger.debug(f"All tests passed.")
    except Exception as e:
        print(e)
        raise e
