from datetime import datetime
import requests
import pytest
import copy
import time
import json

from . import conftest as cf

from py4jps import agentlogging
logger = agentlogging.get_logger('dev')

# This test requires docker image of all agents available in local docker registry
# Or one need to have access to ghcr.io/cambridge-cares

#####################################################################################################
# rogi agent and all rxn agents given a goal request with iteration (all seven agents are involved) #
#####################################################################################################

# override the sample_goal_request with cycleAllowance = 2, deadline to be 2 hrs from now
_sample_goal_request = copy.deepcopy(cf.sample_goal_request)
_sample_goal_request['cycleAllowance'] = 2
_sample_goal_request['deadline'] = str(datetime.fromtimestamp(int(time.time()) + 2 * 60 * 60).isoformat())

# the available labs should also be updated to only include the physical lab that to be tested
# possible choice for available labs:
lab_to_test = 'http://www.theworldavatar.com/kg/lab_auto/lab2/Laboratory_Dummy'
_sample_goal_request['labs'] = [lab_to_test]

@pytest.mark.parametrize(
    "vapourtec_agent_env_file, hplc_agent_env_file, goal_request",
    [
        (
            cf.IRIs.VAPOURTEC_ENV_FILE_DICT.value[lab_to_test],
            cf.IRIs.HPLC_ENV_FILE_DICT.value[lab_to_test],
            _sample_goal_request,
        ),
    ],
)
def test_all_agents_iterations_DOCKERISED(
    initialise_all_dockerised_agent_and_triples,
    create_rog_agent, create_rogi_agent, create_doe_agent, create_vapourtec_schedule_agent, create_hplc_postpro_agent, create_vapourtec_agent, create_hplc_agent,
    vapourtec_agent_env_file, hplc_agent_env_file, goal_request,
):
    sparql_client, derivation_client = initialise_all_dockerised_agent_and_triples

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
    )
    logger.debug(f"vapourtec_agent created")
    # Set the hplc_report_container_dir to be hplc_report_target_folder (on host machine) if it's for local_agent_test
    hplc_agent = create_hplc_agent(
        env_file=hplc_agent_env_file,
        register_agent=True,
    )
    logger.debug(f"hplc_agent created")

    # Send HTTP POST request to the rog agent to invoke the goal iteration
    r = requests.post(cf.host_docker_internal_to_localhost(rog_agent.goal_agent_endpoint), data=goal_request)
    j = json.loads(r.text)
    logger.debug(f"rog_agent goal request response: {j}")
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
            dockerised_test=True,
        )
        logger.debug(f"All tests passed.")
    except Exception as e:
        print(e)
        raise e
