import builtins
import requests
import pytest
import random
import time

from tests.agents.sparql_client_for_test import RANDOM_EXAMPLE_DIFFERENCE
from tests.agents.sparql_client_for_test import RANDOM_EXAMPLE_DIFFERENCEREVERSE
from tests.agents.sparql_client_for_test import RANDOM_EXAMPLE_LISTOFPOINTS
from tests.agents.sparql_client_for_test import RANDOM_EXAMPLE_MAXVALUE
from tests.agents.sparql_client_for_test import RANDOM_EXAMPLE_MINVALUE
from tests.agents.sparql_client_for_test import RANDOM_STRING_WITH_SPACES
from pyderivationagent.data_model.iris import ONTODERIVATION_DERIVATION

from tests.conftest import create_rng_agent
from tests.conftest import create_min_agent
from tests.conftest import create_max_agent
from tests.conftest import create_diff_agent
from tests.conftest import create_diff_reverse_agent
from tests.conftest import RNGAGENT_ENV
from tests.conftest import MINAGENT_ENV
from tests.conftest import MAXAGENT_ENV
from tests.conftest import DIFFAGENT_ENV
from tests.conftest import DIFFREVERSEAGENT_ENV

from tests.conftest import host_docker_internal_to_localhost

import tests.utils as utils

import logging
logger = logging.getLogger('test_docker_integration')

pytest_plugins = ["docker_compose"]


# ----------------------------------------------------------------------------------
# Docker integration tests
# ----------------------------------------------------------------------------------

@pytest.mark.parametrize(
    "rng, max, min, diff",
    [ # True - sync; False - async
        (True, True, True, True),
        (True, True, True, False),
        (True, True, False, False),
        (True, False, True, False),
        (True, False, False, False),
        (False, False, False, False),
    ],
)
def test_docker_integration(initialise_clients_and_agents, rng, max, min, diff):
    ###################################################
    ## I. Get pytest fixture and register all agents ##
    ###################################################
    sparql_client, derivation_client, update_endpoint = initialise_clients_and_agents

    logger.info(f"==================== Test case [{rng}-{max}-{min}-{diff}] start ====================")

    logger.info(
        f"""Testing derivation DAG (True for sync, False for async):
        rng_derivation [{rng}], max_derivation [{max}], min_derivation [{min}], diff_derivation [{diff}]."""
    )

    ###########################################################################################
    ## II. Upload all initial triples prepared in resources folder and register agents in KG ##
    ###########################################################################################
    try:
        all_instances = utils.initialise_triples_assert_pure_inputs(
            sparql_client=sparql_client,
            derivation_client=derivation_client,
            delete_all_triples=True
        )
    except Exception as e:
        logger.error(f"Failed to initialise triples: {e}")
        raise e

    logger.info("Uploading prepared triples finished.")

    # Initialise derivation agents
    # NOTE register_agent_in_kg is not done by agents in docker due to delay between spinning up docker containers and the blazegraph becoming reachable
    # register_agent_in_kg for each agent is automatically called when instantiating rng_agent, min_agent, max_agent, diff_agent, diff_reverse_agent (as register_agent=True)
    rng_agent = create_rng_agent(env_file=RNGAGENT_ENV, register_agent=True, in_docker=False)
    min_agent = create_min_agent(env_file=MINAGENT_ENV, register_agent=True, in_docker=False)
    max_agent = create_max_agent(env_file=MAXAGENT_ENV, register_agent=True, in_docker=False)
    diff_agent = create_diff_agent(env_file=DIFFAGENT_ENV, register_agent=True, in_docker=False)
    diff_reverse_agent = create_diff_reverse_agent(env_file=DIFFREVERSEAGENT_ENV, register_agent=True, in_docker=False)

    logger.info("Initialising derivation agents finished.")

    ################################################################
    ## III. Initialise derivations that generates new information ##
    ################################################################
    try:
        all_instances = execute_test_generate_new_info(
            all_instances=all_instances,
            sparql_client=sparql_client,
            derivation_client=derivation_client,
            rng_agent=rng_agent, min_agent=min_agent, max_agent=max_agent, diff_agent=diff_agent,
            rng=rng, max=max, min=min, diff=diff
        )
    except Exception as e:
        logger.error(f"Failed to execute test generate new info: {e}")
        raise e

    # Now generate a few difference reverse derivations
    diff_reverse_derivation_iri_lst = []
    random_int = random.randint(1, 5)
    for i in range(random_int):
        diff_reverse_derivation_iri_lst.append(
            derivation_client.createAsyncDerivationForNewInfo(
                diff_reverse_agent.agentIRI, [sparql_client.getMaxValueIRI(), sparql_client.getMinValueIRI()]
            )
        )

    # Wait for all difference reverse derivations to finish
    diff_reverse_derivation_current_timestamp = 0
    while diff_reverse_derivation_current_timestamp == 0:
        time.sleep(8)
        # the queried results must be converted to int, otherwise it will never equal to 0
        diff_reverse_derivation_current_timestamp = builtins.min(
            [
                utils.get_timestamp(iri, sparql_client) for iri in diff_reverse_derivation_iri_lst
            ]
        )

    # Check if the difference reverse is calculated correctly
    queried_diff_reverse_dct = sparql_client.getDiffReverseValues()
    # the number of diff reverse derivations should be the same as the number of calculated diff reverse values
    print("======================================")
    print(f"queried_diff_reverse_dct: {queried_diff_reverse_dct}")
    print(f"diff_reverse_derivation_iri_lst: {diff_reverse_derivation_iri_lst}")
    print(sparql_client.getValue(sparql_client.getDifferenceIRI()))
    print("======================================")
    assert len(queried_diff_reverse_dct) == len(diff_reverse_derivation_iri_lst)
    assert all([queried_diff_reverse_dct[iri] + sparql_client.getValue(sparql_client.getDifferenceIRI()) == 0 for iri in queried_diff_reverse_dct])

    logger.info("New information generation finished.")

    ####################################################################
    ## IV. Update one of the pure inputs to make derivations outdated ##
    ####################################################################
    try:
        execute_update_pure_input(sparql_client=sparql_client, derivation_client=derivation_client)
    except Exception as e:
        logger.error(f"Failed to execute update pure input: {e}")
        raise e

    logger.info("Pure input modification finished, the derivations are now outdated.")

    #############################################
    ## V. Request for an update of derivations ##
    #############################################
    # We also need to check the difference reverse derivation
    ts = time.time()

    try:
        execute_update_derivation(
            all_instances=all_instances,
            sparql_client=sparql_client,
            update_endpoint=update_endpoint
        )
    except Exception as e:
        logger.error(f"Failed to execute update derivation: {e}")
        raise e

    # Wait until all difference reverse derivation are updated
    # i.e. the timestamp of all these derivations are greater than the timestamp before the update
    _derivation_current_timestamp = ts
    while _derivation_current_timestamp <= ts:
        time.sleep(8)
        # the queried results must be converted to int, otherwise it will never equal to 0
        _derivation_current_timestamp = builtins.min([utils.get_timestamp(iri, sparql_client) for iri in diff_reverse_derivation_iri_lst])
        logger.info(f"Waiting for difference reverse derivation to be updated, current minimum timestamp: {_derivation_current_timestamp}, timestamp before update request: {ts}")

    # Check if the difference reverse is calculated correctly
    queried_diff_reverse_dct = sparql_client.getDiffReverseValues()
    _difference_after_update = sparql_client.getValue(sparql_client.getDifferenceIRI())
    # the number of diff reverse derivations should be the same as the number of calculated diff reverse values
    assert len(queried_diff_reverse_dct) == len(diff_reverse_derivation_iri_lst)
    assert all([queried_diff_reverse_dct[iri] + _difference_after_update == 0 for iri in queried_diff_reverse_dct])

    logger.info("All existing information update finished.")
    logger.info(f"==================== Test case [{rng}-{max}-{min}-{diff}] end ====================")


# ----------------------------------------------------------------------------------
# Common test functions
# ----------------------------------------------------------------------------------

def assert_all_information_are_up_to_date(all_instances, sparql_client, before_invoke_input_agent: bool):

    upperlimit_instance = sparql_client.getUpperLimit()
    lowerlimit_instance = sparql_client.getLowerLimit()
    number_of_points_instance = sparql_client.getNumOfPoints()

    # test pure input UpperLimit and LowerLimit still remain the same instance IRI
    assert all_instances.IRI_UPPER_LIMIT == upperlimit_instance
    assert all_instances.IRI_LOWER_LIMIT == lowerlimit_instance

    # test if the value for upperlimit and lowerlimit are still initialised value
    assert all_instances.VAL_UPPER_LIMIT == sparql_client.getValue(upperlimit_instance)
    assert all_instances.VAL_LOWER_LIMIT == sparql_client.getValue(lowerlimit_instance)

    # test IRI and value of NumberOfPoints if it's before invoking the InputAgent
    if before_invoke_input_agent:
        assert all_instances.IRI_NUM_OF_PTS == number_of_points_instance
        assert all_instances.VAL_NUM_OF_PTS == sparql_client.getValue(number_of_points_instance)

    # get all timestamp
    timestamp_upper_limit_instance = utils.get_timestamp(upperlimit_instance, sparql_client)
    timestamp_lower_limit_instance = utils.get_timestamp(lowerlimit_instance, sparql_client)
    timestamp_num_of_point_instance = utils.get_timestamp(number_of_points_instance, sparql_client)
    timestamp_difference_derivation = utils.get_timestamp(all_instances.DERIV_DIFF, sparql_client)
    timestamp_maxvalue_derivation = utils.get_timestamp(all_instances.DERIV_MAX, sparql_client)
    timestamp_minvalue_derivation = utils.get_timestamp(all_instances.DERIV_MIN, sparql_client)
    timestamp_rng_derivation = utils.get_timestamp(all_instances.DERIV_RNG, sparql_client)

    # test if all the timestamp of derivations are up-to-date
    assert timestamp_upper_limit_instance > 0
    assert timestamp_lower_limit_instance > 0
    assert timestamp_num_of_point_instance > 0
    assert timestamp_rng_derivation >= timestamp_upper_limit_instance
    assert timestamp_rng_derivation >= timestamp_lower_limit_instance
    assert timestamp_rng_derivation >= timestamp_num_of_point_instance
    assert timestamp_maxvalue_derivation >= timestamp_rng_derivation
    assert timestamp_minvalue_derivation >= timestamp_rng_derivation
    assert timestamp_difference_derivation >= timestamp_maxvalue_derivation
    assert timestamp_difference_derivation >= timestamp_minvalue_derivation

    # test if all values in the KG are correct
    # test if it contains correct number of points in the derivation DAG
    assert sparql_client.getValue(sparql_client.getNumOfPoints()) == len(sparql_client.getPointsInList(sparql_client.getListOfPoints()))
    # test if the rdfs:comment of all points are correct
    pt_comment_dict = sparql_client.getPointsRdfsCommentInKG()
    assert sparql_client.getValue(sparql_client.getNumOfPoints()) == len(pt_comment_dict)
    assert all([pt_comment_dict[pt] == RANDOM_STRING_WITH_SPACES for pt in pt_comment_dict])
    # Also check if the special values are correctly generated and parsed
    assert all([sparql_client.pointHasAllSpecialValues(_pt_iri) for _pt_iri in pt_comment_dict])
    # test if no duplicate information written to KG in the situation of concurrent
    # HTTP request
    assert sparql_client.getValue(sparql_client.getNumOfPoints()) == len(sparql_client.getPointsInKG())
    # test if the value is the same as the max value
    assert sparql_client.getExtremeValueInList(sparql_client.getListOfPoints(), True) == sparql_client.getValue(sparql_client.getMaxValueIRI())
    # test if the value is the same as the min value
    assert sparql_client.getExtremeValueInList(sparql_client.getListOfPoints(), False) == sparql_client.getValue(sparql_client.getMinValueIRI())
    # test if the value is the same as the difference value
    difference = sparql_client.getValue(sparql_client.getMaxValueIRI()) - sparql_client.getValue(sparql_client.getMinValueIRI())
    assert difference == sparql_client.getValue(sparql_client.getDifferenceIRI())

    logger.debug("All information in the knowledge graph for RNG/Min/Max/Diff derivations are correct and up-to-date. Information on DiffReverse derivations remain to be checked.")


def initialise_derivations(
    all_instances, sparql_client, derivation_client,
    rng_agent, min_agent, max_agent, diff_agent,
    rng: bool, max: bool, min: bool, diff: bool
):
    pure_inputs = [all_instances.IRI_UPPER_LIMIT, all_instances.IRI_LOWER_LIMIT, all_instances.IRI_NUM_OF_PTS]
    max_deriv_inputs = []
    min_deriv_inputs = []
    diff_deriv_inputs = []

    if rng:
        rng_derivation = derivation_client.createSyncDerivationForNewInfo(
            rng_agent.agentIRI, host_docker_internal_to_localhost(rng_agent.agentEndpoint),
            pure_inputs, ONTODERIVATION_DERIVATION
        )

        all_instances.DERIV_RNG = rng_derivation.getIri()
        logger.info("Created RNG derivation <" + all_instances.DERIV_RNG + ">")

        listOfRandomPoints_iris = rng_derivation.getBelongsToIris(RANDOM_EXAMPLE_LISTOFPOINTS)
        if len(listOfRandomPoints_iris) != 1:
            raise Exception("RNG derivation should have exactly one listOfPoints as output, but got " + listOfRandomPoints_iris)

        all_instances.IRI_LST_PTS = listOfRandomPoints_iris[0]
        logger.info("Created ListOfRandomPoints instance <" + all_instances.IRI_LST_PTS + ">")
        dct_of_pts = sparql_client.getPointsInList(all_instances.IRI_LST_PTS)
        if len(dct_of_pts) != all_instances.VAL_NUM_OF_PTS:
            raise Exception("ListOfRandomPoints should have " + str(all_instances.VAL_NUM_OF_PTS) + " points, but got " + str(dct_of_pts))
        logger.info("Generated ListOfRandomPoints: " + str(dct_of_pts))

        max_deriv_inputs.append(all_instances.IRI_LST_PTS)
        min_deriv_inputs.append(all_instances.IRI_LST_PTS)

        # create maxvalue, minvalue via derivations
        if max:
            max_derivation = derivation_client.createSyncDerivationForNewInfo(
                max_agent.agentIRI, host_docker_internal_to_localhost(max_agent.agentEndpoint),
                max_deriv_inputs, ONTODERIVATION_DERIVATION
            )
            all_instances.DERIV_MAX = max_derivation.getIri()
            logger.info("Created MaxValue derivation <" + all_instances.DERIV_MAX + ">")

            maxValue_iris = max_derivation.getBelongsToIris(RANDOM_EXAMPLE_MAXVALUE)
            if len(maxValue_iris) != 1:
                raise Exception("MaxValue derivation should have exactly one maxValue as output, but got " + maxValue_iris)
            all_instances.IRI_MAX = maxValue_iris[0]
            all_instances.VAL_MAX = sparql_client.getValue(all_instances.IRI_MAX)
            logger.info("Created MaxValue instance <" + all_instances.IRI_MAX + ">")
            logger.info("MaxValue instance value is " + str(all_instances.VAL_MAX))

            diff_deriv_inputs.append(all_instances.IRI_MAX)
        else:
            all_instances.DERIV_MAX = derivation_client.createAsyncDerivationForNewInfo(
                all_instances.MAXAGENT_SERVICE, max_deriv_inputs
            )
            diff_deriv_inputs.append(all_instances.DERIV_MAX)

        if min:
            min_derivation = derivation_client.createSyncDerivationForNewInfo(
                min_agent.agentIRI, host_docker_internal_to_localhost(min_agent.agentEndpoint),
                min_deriv_inputs, ONTODERIVATION_DERIVATION
            )
            all_instances.DERIV_MIN = min_derivation.getIri()
            logger.info("Created MinValue derivation <" + all_instances.DERIV_MIN + ">")

            minValue_iris = min_derivation.getBelongsToIris(RANDOM_EXAMPLE_MINVALUE)
            if len(minValue_iris) != 1:
                raise Exception("MinValue derivation should have exactly one minValue as output, but got " + minValue_iris)
            all_instances.IRI_MIN = minValue_iris[0]
            all_instances.VAL_MIN = sparql_client.getValue(all_instances.IRI_MIN)
            logger.info("Created MinValue instance <" + all_instances.IRI_MIN + ">")
            logger.info("MinValue instance value is " + str(all_instances.VAL_MIN))

            diff_deriv_inputs.append(all_instances.IRI_MIN)
        else:
            all_instances.DERIV_MIN = derivation_client.createAsyncDerivationForNewInfo(
                all_instances.MINAGENT_SERVICE, min_deriv_inputs
            )
            diff_deriv_inputs.append(all_instances.DERIV_MIN)

        if diff:
            # create difference via difference derivation
            diff_derivation = derivation_client.createSyncDerivationForNewInfo(
                diff_agent.agentIRI, host_docker_internal_to_localhost(diff_agent.agentEndpoint),
                diff_deriv_inputs, ONTODERIVATION_DERIVATION
            )
            all_instances.DERIV_DIFF = diff_derivation.getIri()
            logger.info("Created Difference derivation <" + all_instances.DERIV_DIFF + ">")

            diff_iris = diff_derivation.getBelongsToIris(RANDOM_EXAMPLE_DIFFERENCE)
            if len(diff_iris) != 1:
                raise Exception("Difference derivation should have exactly one difference as output, but got " + diff_iris)
            all_instances.IRI_DIFF = diff_iris[0]
            all_instances.VAL_DIFF = sparql_client.getValue(all_instances.IRI_DIFF)
            logger.info("Created Difference instance <" + all_instances.IRI_DIFF + ">")
            logger.info("Difference instance value is " + str(all_instances.VAL_DIFF))
        else:
            all_instances.DERIV_DIFF = derivation_client.createAsyncDerivationForNewInfo(
                all_instances.DIFFAGENT_SERVICE, diff_deriv_inputs
            )

    else:
        all_instances.DERIV_RNG = derivation_client.createAsyncDerivationForNewInfo(
            all_instances.RNGAGENT_SERVICE, pure_inputs
        )

        all_instances.DERIV_MAX = derivation_client.createAsyncDerivationForNewInfo(
            all_instances.MAXAGENT_SERVICE, [all_instances.DERIV_RNG]
        )

        all_instances.DERIV_MIN = derivation_client.createAsyncDerivationForNewInfo(
            all_instances.MINAGENT_SERVICE, [all_instances.DERIV_RNG]
        )

        all_instances.DERIV_DIFF = derivation_client.createAsyncDerivationForNewInfo(
            all_instances.DIFFAGENT_SERVICE, [all_instances.DERIV_MAX, all_instances.DERIV_MIN]
        )

    return all_instances


def execute_test_generate_new_info(
    all_instances, sparql_client, derivation_client,
    rng_agent, min_agent, max_agent, diff_agent,
    rng: bool, max: bool, min: bool, diff: bool
):
    # initialise all derivations first
    all_instances = initialise_derivations(
        all_instances=all_instances,
        sparql_client=sparql_client,
        derivation_client=derivation_client,
        rng_agent=rng_agent, min_agent=min_agent, max_agent=max_agent, diff_agent=diff_agent,
        rng=rng, max=max, min=min, diff=diff
    )

    logger.debug(
        f"""Initialised derivations for new information generation:
        rng_derivation <{all_instances.DERIV_RNG}>,
        max_derivation <{all_instances.DERIV_MAX}>,
        min_derivation <{all_instances.DERIV_MIN}>,
        diff_derivation <{all_instances.DERIV_DIFF}>."""
    )

    # for each created instance, test that only one instance should be created if
    # it is sync derivation, otherwise, no instance should be created
    if rng:
        listofrandompoints_instance = all_instances.IRI_LST_PTS
        assert listofrandompoints_instance == sparql_client.getListOfPoints()
    else:
        assert not all_instances.IRI_LST_PTS

    if max:
        maxvalue_instance = all_instances.IRI_MAX
        assert maxvalue_instance == sparql_client.getMaxValueIRI()
    else:
        assert not all_instances.IRI_MAX

    if min:
        minvalue_instance = all_instances.IRI_MIN
        assert minvalue_instance == sparql_client.getMinValueIRI()
    else:
        assert not all_instances.IRI_MIN

    if diff:
        difference_instance = all_instances.IRI_DIFF
        assert difference_instance == sparql_client.getDifferenceIRI()
    else:
        assert not all_instances.IRI_DIFF

    # get IRI of difference derivation
    difference_derivation = all_instances.DERIV_DIFF

    # once timestamp of difference derivation updated, all information in the KG
    # should also be up-to-date
    currentTimestamp_difference_derivation = utils.get_timestamp(difference_derivation, sparql_client)
    while currentTimestamp_difference_derivation == 0:
        time.sleep(8)
        currentTimestamp_difference_derivation = utils.get_timestamp(difference_derivation, sparql_client)

    # wait arbitrary amount of time so that the cleaning up is finished
    time.sleep(3)

    # now all information should be up-to-date
    assert_all_information_are_up_to_date(all_instances, sparql_client, True)
    return all_instances


def execute_update_pure_input(sparql_client, derivation_client):
    # save information about the old NumberOfPoints instance
    numOfPoint = sparql_client.getNumOfPoints()
    numOfPoints_val_old = sparql_client.getValue(numOfPoint)
    numOfPoints_timestamp_old = utils.get_timestamp(numOfPoint, sparql_client)

    # modify the value of num of points and update the timestamp
    sparql_client.increaseNumOfPointsByOne()
    derivation_client.updateTimestamp(numOfPoint)

    # get information about new NumberOfPoints instance
    numOfPoints_val_new = sparql_client.getValue(numOfPoint)
    numOfPoints_timestamp_new = utils.get_timestamp(numOfPoint, sparql_client)

    # test if InputAgent increased the value
    assert numOfPoints_val_old + 1 == numOfPoints_val_new
    # test if InputAgent modified the timestamp
    assert numOfPoints_timestamp_new > numOfPoints_timestamp_old

    logger.debug(f"Pure input number of points <{numOfPoint}> updated from {numOfPoints_val_old} to {numOfPoints_val_new}")


def execute_update_derivation(all_instances, sparql_client, update_endpoint):
    # get IRI of difference derivation
    difference_derivation = all_instances.DERIV_DIFF
    # get old timestamp of difference derivation
    difference_derivation_timestamp = utils.get_timestamp(difference_derivation, sparql_client)
    # get information about old instance of difference
    difference_instance_old = sparql_client.getDifferenceIRI()

    # invoke UpdateDerivations via HTTP request
    # endpoint of update agent in docker, e.g., "http://localhost:7004/update"
    response = requests.get(update_endpoint)
    logger.info(f"Update request sent to endpoint <{update_endpoint}>, received response status: {response}, response body: {str(response.content)}")

    # once timestamp of difference derivation updated, the iri of difference should
    # be different from the previous one, all information in the KG should also be
    # up-to-date
    currentTimestamp_difference_derivation = utils.get_timestamp(difference_derivation, sparql_client)
    while currentTimestamp_difference_derivation <= difference_derivation_timestamp:
        time.sleep(20)
        currentTimestamp_difference_derivation = utils.get_timestamp(difference_derivation, sparql_client)

    # wait arbitrary amount of time so that the cleaning up is finished
    time.sleep(3)
    difference_instance_new = sparql_client.getDifferenceIRI()
    assert difference_instance_old != difference_instance_new

    # now all information in the KG should be up-to-date
    assert_all_information_are_up_to_date(all_instances, sparql_client, False)
