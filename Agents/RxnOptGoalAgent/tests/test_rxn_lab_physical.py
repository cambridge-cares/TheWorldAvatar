from datetime import datetime
import pytest
import time
import json
import copy

import tests.conftest as cf

from py4jps import agentlogging
logger = agentlogging.get_logger('dev')

# override the sample_goal_request with cycleAllowance = 1, deadline to be 2 hrs from now
_sample_goal_request = copy.deepcopy(cf.sample_goal_request)
_sample_goal_request['cycleAllowance'] = 1
_sample_goal_request['deadline'] = str(datetime.fromtimestamp(int(time.time()) + 2 * 60 * 60).isoformat())

# TODO [before running test]
# 0. MAKE SURE ALL THE HARDWARE SIDE IS READY (VAPOURTEC (REAGENT BOTTLE AND FLOW COMMANDER), HPLC SEQUENCE AND DATA OUTPUT FORMAT)
# 1. the available labs should also be updated to only include the physical lab that to be tested
# possible choice for available labs:
# lab_to_test = 'http://example.com/blazegraph/namespace/testlab/lab1/Laboratory_Dummy'
lab_to_test = 'http://example.com/blazegraph/namespace/testlab/lab2/Laboratory_Dummy'
_sample_goal_request['labs'] = [lab_to_test]
# 2. the hplc_report_container_dir should be updated to the correct path
# depending on if running in Windows or WSL2, choose the correct format, e.g.:
# hplc_report_target_folder = "C:\\Chem32\\1\\Data\\Optimization\\Optimization 2022-09-19 22-41-02" # Windows
# hplc_report_target_folder = "/mnt/c/Chem32/1/Data/Optimization/Optimization 2022-09-19 22-41-02" # WSL2 (path in Windows mounted in WSL2)
hplc_report_target_folder = cf.HPLC_REPORT_LOCAL_TEST_DIR
# 3. the vapourtec_ip should be updated to the correct ip address if running in WSL2
# the ip address can be found by running the following command in WSL2:
# echo $(ipconfig.exe | grep 'vEthernet (WSL)' -A4 | cut -d":" -f 2 | tail -n1 | sed -e 's/\s*//g')
# for more details, see https://pscheit.medium.com/get-the-ip-address-of-the-desktop-windows-host-in-wsl2-7dc61653ad51
# vapourtec_ip_address = "localhost" # Windows
vapourtec_ip_address = "172.20.128.1"
# 4. pick either the local agent test or dockerised agent test be commenting out the corresponding test function
local_agent_test = True # local agent test
# local_agent_test = False # dockerised agent test


@pytest.mark.parametrize(
    "vapourtec_agent_env_file, vapourtec_ip_address, fcexp_file_container_folder, hplc_agent_env_file, hplc_report_target_folder, local_agent_test, goal_request",
    [
        (
            cf.IRIs.VAPOURTEC_ENV_FILE_DICT.value[lab_to_test],
            vapourtec_ip_address,
            cf.FCEXP_FILE_DIR,
            cf.IRIs.HPLC_ENV_FILE_DICT.value[lab_to_test],
            hplc_report_target_folder,
            local_agent_test,
            _sample_goal_request,
        ),
    ],
)
def test_rxn_rog_PHYSICAL(
    initialise_blazegraph_fileserver_with_test_triples,
    create_rog_agent, create_rogi_agent, create_doe_agent, create_vapourtec_schedule_agent, create_hplc_postpro_agent, create_vapourtec_agent, create_hplc_agent,
    vapourtec_agent_env_file, vapourtec_ip_address, fcexp_file_container_folder, hplc_agent_env_file, hplc_report_target_folder, local_agent_test, goal_request,
):
    # endpoint = initialise_test_triples
    sparql_client, derivation_client = initialise_blazegraph_fileserver_with_test_triples

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
        vapourtec_ip_address=vapourtec_ip_address,
        fcexp_file_container_folder=fcexp_file_container_folder,
        dry_run=False,
    )
    logger.debug(f"vapourtec_agent created")
    # Set the hplc_report_container_dir to be hplc_report_target_folder (on host machine) if it's for local_agent_test
    hplc_agent = create_hplc_agent(
        env_file=hplc_agent_env_file,
        register_agent=True,
        hplc_report_container_dir=hplc_report_target_folder,
        dry_run=False,
    )
    logger.debug(f"hplc_agent created")

    # Start the scheduler to monitor derivations if it's local agent test
    if local_agent_test:
        rogi_agent.start_all_periodical_job()
        doe_agent.start_all_periodical_job()
        vapourtec_schedule_agent.start_all_periodical_job()
        hplc_postpro_agent.start_all_periodical_job()
        vapourtec_agent.start_all_periodical_job()
        hplc_agent.start_all_periodical_job()

    with rog_agent.app.test_client() as test_client:
        r = test_client.post(rog_agent.goal_agent_endpoint, data=goal_request)
        j = json.loads(r.text)
        rogi_derivation_lst = j[rog_agent.GOAL_SPECS_RESPONSE_KEY]
        logger.debug(f"rogi_derivation_lst: {rogi_derivation_lst}")
        goal_set_iri = j[rog_agent.GOAL_SET_IRI_KEY]
        logger.debug(f"goal_set_iri: {goal_set_iri}")
        assert len(rogi_derivation_lst) == 1
        rogi_derivation_iri = rogi_derivation_lst[0]

        # Wait until the rogi derivation is done
        rogi_completed_one_iter = False
        while not rogi_completed_one_iter:
            time.sleep(10)
            rogi_completed_one_iter = sparql_client.check_if_rogi_complete_one_iter(rogi_derivation_iri)
            print(f"ROGI derivation {rogi_derivation_iri} is completed: {rogi_completed_one_iter}, current time: {time.time()}")

    # Shutdown the scheduler to clean up if it's local agent test (as the doe_agent scheduler must have started)
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
