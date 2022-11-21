import json
import time

import tests.conftest as cf

from py4jps import agentlogging
logger = agentlogging.get_logger('dev')

# ----------------------------------------------------------------------------------
# Test cases for the integration of the goal iteration process
# i.e. the connection between RxnOptGoalAgent and RxnOptGoalIterAgent
# 1. rogi agent run given a goal request
# 2. TODO rog agent iterates the goal derivation
# ----------------------------------------------------------------------------------

# TODO below test needs to be updated
# as the cf.rogi_cf.create_dummy_triples_for_performance_indicator
# doesn't create triple to declare the rdf:type of the reaction variation, which is required for the agent to detect
# the post processing derivation
def test_rogi_picks_up_derivation(
    initialise_test_triples,
    create_rog_agent,
    create_rogi_agent
):
    endpoint = initialise_test_triples

    # create rogi agent and rog agent
    rogi_agent = create_rogi_agent(
        register_agent=True,
        random_agent_iri=True, # TODO double check if this matters?
        kg_url=endpoint,
        kg_update_url=endpoint,
    )
    rog_agent = create_rog_agent(
        goal_iter_agent_iri=rogi_agent.agentIRI,
        kg_url=endpoint,
        kg_update_url=endpoint,
    )

    # assert that no derivations isDerivedUsing the rogi agent yet
    assert not rog_agent.sparql_client.check_if_triple_exist(
        None, cf.ONTODERIVATION_ISDERIVEDUSING, rogi_agent.agentIRI
    )

    # start rogi agent to monitor derivations
    rogi_agent.start_all_periodical_job()

    # send goal request to rog agent
    sample_goal_request = cf.sample_goal_request
    with rog_agent.app.test_client() as test_client:
        r = test_client.post(rog_agent.goal_agent_endpoint, data=sample_goal_request)
        j = json.loads(r.text)
        rogi_derivation = j[rog_agent.GOAL_SPECS_RESPONSE_KEY]
        assert rogi_derivation is not None
        # assert the rogi derivation is created correctly and isDerivedUsing the rogi agent
        # it's important to distinguish it with rog_agent.goal_agent_iri
        assert rog_agent.sparql_client.check_if_triple_exist(
            rogi_derivation, cf.ONTODERIVATION_ISDERIVEDUSING, rogi_agent.agentIRI
        )

        # get the goal set iri
        goal_set_query_res = rog_agent.sparql_client.performQuery(
            f"""SELECT ?goal_set_iri WHERE {{<{rogi_derivation}> <{cf.ONTODERIVATION_ISDERIVEDFROM}> ?goal_set_iri. ?goal_set_iri a <{cf.ONTOGOAL_GOALSET}>.}}"""
        )
        assert len(goal_set_query_res) == 1
        goal_set_iri = goal_set_query_res[0]['goal_set_iri']

        # now check if rogi agent has started the rogi derivation process, i.e. if the postpro derivation is created
        # if so, then we consider the rogi agent has successfully collected the input triples and started the derivation process
        postpro_derivation_iri = None
        while not postpro_derivation_iri:
            time.sleep(10)
            postpro_derivation_iri = cf.rogi_cf.get_postpro_derivation(goal_set_iri, rog_agent.sparql_client)
        # as the postpro derivation is created, rogi derivation should be now InProgress
        assert rog_agent.sparql_client.performQuery(
            f"""ASK {{<{rogi_derivation}> <{cf.ONTODERIVATION_HASSTATUS}>/a <{cf.ONTODERIVATION_INPROGRESS}>.}}"""
        )[0]['ASK']

        # NOTE in theory now we can shutdown rogi_agent and stop test
        # however, if we do rogi_agent.scheduler.shutdown(wait=False), although the scheduler will be shutdown, the threadpool will remain active
        # e.g., <Thread(APScheduler, started daemon 139864592848640)>, <Thread(ThreadPoolExecutor-0_0, started daemon 139864584455936)>
        # (also see https://github.com/agronholm/apscheduler/issues/567#issuecomment-953530326)
        # so this will create problem for clear_loggers() in conftest.py
        # i.e., it will hang forever waiting for removing handler <StreamHandler <stdout> (DEBUG)> from logger <RootLogger root (DEBUG)>
        # as thread of APScheduler is still writing to stdout
        # therefore, here we add lines to create dummy triples to let rogi derivation finish before shutdown rogi agent

        # Create dummy triples that mimic the output of the postpro derivation
        # This function also uploads dummy triples to KG, later these will be picked up by ROGI agent
        goal_set_instance = rogi_agent.sparql_client.get_goal_set_instance(goal_set_iri)
        g = cf.rogi_cf.create_dummy_triples_for_performance_indicator(goal_set_instance, postpro_derivation_iri, rogi_agent.sparql_client)

        # Query timestamp of the derivation for every 20 seconds until it's updated
        currentTimestamp_derivation = 0
        while currentTimestamp_derivation == 0:
            time.sleep(20)
            currentTimestamp_derivation = cf.rogi_cf.get_timestamp(rogi_derivation, rogi_agent.sparql_client)

    # now we can safely shutdown the rogi agent
    rogi_agent.scheduler.shutdown()

    # also need to shutdown the rog agent if any job is started
    if rog_agent.scheduler.running:
        rog_agent.scheduler.shutdown(wait=False)
