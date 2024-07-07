import builtins
import requests
import pytest
import random
import time

from .agents.sparql_client_for_test import RANDOM_EXAMPLE_DIFFERENCE
from .agents.sparql_client_for_test import RANDOM_EXAMPLE_DIFFERENCEREVERSE
from .agents.sparql_client_for_test import RANDOM_EXAMPLE_MAXVALUE
from .agents.sparql_client_for_test import RANDOM_EXAMPLE_MINVALUE
from .agents.sparql_client_for_test import RANDOM_EXAMPLE_POINT
from .agents.sparql_client_for_test import RANDOM_STRING_WITH_SPACES
from twa.data_model.iris import ONTODERIVATION_DERIVATION

from .conftest import create_rng_agent
from .conftest import create_min_agent
from .conftest import create_max_agent
from .conftest import create_diff_agent
from .conftest import create_diff_reverse_agent
from .conftest import RNGAGENT_ENV
from .conftest import MINAGENT_ENV
from .conftest import MAXAGENT_ENV
from .conftest import DIFFAGENT_ENV
from .conftest import DIFFREVERSEAGENT_ENV

from .conftest import host_docker_internal_to_localhost

from . import utils

from twa import agentlogging
logger = agentlogging.get_logger('dev')

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

    ####################################################################################
    ## III. Initialise derivations that generates new information (no output for now) ##
    ####################################################################################
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
    random_int = random.randint(2, 4)
    for i in range(random_int):
        diff_reverse_derivation_iri_lst.append(
            derivation_client.createAsyncDerivationForNewInfo(
                diff_reverse_agent.agentIRI, [all_instances.DERIV_MAX, all_instances.DERIV_MIN]
            )
        )
    all_instances.DERIV_DIFF_REVERSE = diff_reverse_derivation_iri_lst

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
    print("======================================")
    assert len(queried_diff_reverse_dct) == 0 # as now there's no output yet

    logger.info("New information generation (no outputs init) finished.")

    ###########################################################
    ## IV. Update number of points to produce actual outputs ##
    ###########################################################
    execute_and_test_pure_input_and_update(
        sparql_client=sparql_client,
        derivation_client=derivation_client,
        all_instances=all_instances,
        update_endpoint=update_endpoint,
        exist_outputs=True,
        change_num_of_pts_to=5,
    )

    logger.info("Testing of no output to actual outputs passed.")

    ##############################################
    ## IV. Update number of points to no output ##
    ##############################################
    execute_and_test_pure_input_and_update(
        sparql_client=sparql_client,
        derivation_client=derivation_client,
        all_instances=all_instances,
        update_endpoint=update_endpoint,
        exist_outputs=False,
        change_num_of_pts_to=0,
    )

    logger.info("Testing of actual outputs to no output passed.")

    logger.info(f"==================== Test case [{rng}-{max}-{min}-{diff}] end ====================")


# ----------------------------------------------------------------------------------
# Common test functions
# ----------------------------------------------------------------------------------

def execute_and_test_pure_input_and_update(
    sparql_client,
    derivation_client,
    all_instances,
    update_endpoint,
    exist_outputs,
    change_num_of_pts_to: int=None,
):
    ###################################################################
    ## I. Update one of the pure inputs to make derivations outdated ##
    ###################################################################
    try:
        execute_update_pure_input(sparql_client=sparql_client, derivation_client=derivation_client, change_num_of_pts_to=change_num_of_pts_to)
    except Exception as e:
        logger.error(f"Failed to execute update pure input: {e}")
        raise e

    logger.info("Pure input modification finished, the derivations are now outdated.")

    ##############################################
    ## II. Request for an update of derivations ##
    ##############################################
    # We also need to check the difference reverse derivation
    ts = time.time()

    try:
        execute_update_derivation(
            all_instances=all_instances,
            sparql_client=sparql_client,
            update_endpoint=update_endpoint,
            exist_outputs=exist_outputs
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
        _derivation_current_timestamp = builtins.min([utils.get_timestamp(iri, sparql_client) for iri in all_instances.DERIV_DIFF_REVERSE])
        logger.info(f"Waiting for difference reverse derivation to be updated, current minimum timestamp: {_derivation_current_timestamp}, timestamp before update request: {ts}")

    # Check if the difference reverse is calculated correctly
    queried_diff_reverse_dct = sparql_client.getDiffReverseValues()
    if exist_outputs:
        _difference_after_update = sparql_client.getValue(sparql_client.getDifferenceIRI())
        # the number of diff reverse derivations should be the same as the number of calculated diff reverse values
        assert len(queried_diff_reverse_dct) == len(all_instances.DERIV_DIFF_REVERSE)
        assert all([queried_diff_reverse_dct[iri] + _difference_after_update == 0 for iri in queried_diff_reverse_dct])
    else:
        assert len(queried_diff_reverse_dct) == 0 # as now there's no output yet

    logger.info("All existing information update finished.")


def assert_all_information_are_up_to_date(all_instances, sparql_client, exist_outputs: bool):

    upperlimit_instance = sparql_client.getUpperLimit()
    lowerlimit_instance = sparql_client.getLowerLimit()
    number_of_points_instance = sparql_client.getNumOfPoints()

    # test pure input UpperLimit and LowerLimit still remain the same instance IRI
    assert all_instances.IRI_UPPER_LIMIT == upperlimit_instance
    assert all_instances.IRI_LOWER_LIMIT == lowerlimit_instance

    # test if the value for upperlimit and lowerlimit are still initialised value
    assert all_instances.VAL_UPPER_LIMIT == sparql_client.getValue(upperlimit_instance)
    assert all_instances.VAL_LOWER_LIMIT == sparql_client.getValue(lowerlimit_instance)

    # test IRI of NumberOfPoints
    assert all_instances.IRI_NUM_OF_PTS == number_of_points_instance

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
    # this also tests if no duplicate information written to KG in the situation of concurrent HTTP request
    assert sparql_client.getValue(sparql_client.getNumOfPoints()) == len(sparql_client.getPointsInKG())
    # test if the rdfs:comment of all points are correct
    pt_comment_dict = sparql_client.getPointsRdfsCommentInKG()
    assert sparql_client.getValue(sparql_client.getNumOfPoints()) == len(pt_comment_dict)
    if exist_outputs:
        assert all([pt_comment_dict[pt] == RANDOM_STRING_WITH_SPACES for pt in pt_comment_dict])
        # Also check if the special values are correctly generated and parsed
        assert all([sparql_client.pointHasAllSpecialValues(_pt_iri) for _pt_iri in pt_comment_dict])
        # test if the value is the same as the max value
        assert sparql_client.getExtremeValueInList(list(sparql_client.getPointsInKG().keys()), True) == sparql_client.getValue(sparql_client.getMaxValueIRI())
        # test if the value is the same as the min value
        assert sparql_client.getExtremeValueInList(list(sparql_client.getPointsInKG().keys()), False) == sparql_client.getValue(sparql_client.getMinValueIRI())
        # test if the value is the same as the difference value
        difference = sparql_client.getValue(sparql_client.getMaxValueIRI()) - sparql_client.getValue(sparql_client.getMinValueIRI())
        assert difference == sparql_client.getValue(sparql_client.getDifferenceIRI())
    else:
        # test that min/max/diff derivations are not calculated
        assert not bool(sparql_client.getMinValueIRI())
        assert not bool(sparql_client.getMaxValueIRI())
        assert not bool(sparql_client.getDifferenceIRI())

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
        rng_derivation = derivation_client.createSyncDerivationForNewInfoWithHttpUrl(
            rng_agent.agentIRI, host_docker_internal_to_localhost(rng_agent.syncDerivationEndpoint),
            pure_inputs, ONTODERIVATION_DERIVATION
        )

        all_instances.DERIV_RNG = rng_derivation.getIri()
        logger.info("Created RNG derivation <" + all_instances.DERIV_RNG + ">")

        list_points_iris = rng_derivation.getBelongsToIris(RANDOM_EXAMPLE_POINT)

        if bool(list_points_iris):
            all_instances.IRI_LST_PTS = list_points_iris
            logger.info(f"Created list of random Point instance <{all_instances.IRI_LST_PTS}>")
            dct_of_pts = sparql_client.getPointsInKG()
            if len(dct_of_pts) != all_instances.VAL_NUM_OF_PTS:
                raise Exception("ListOfRandomPoints should have " + str(all_instances.VAL_NUM_OF_PTS) + " points, but got " + str(dct_of_pts))
            logger.info("Generated list of random Point(s): " + str(dct_of_pts))

            max_deriv_inputs.extend(all_instances.IRI_LST_PTS)
            min_deriv_inputs.extend(all_instances.IRI_LST_PTS)
        else:
            logger.info("No random Point instance created")
            max_deriv_inputs.append(all_instances.DERIV_RNG)
            min_deriv_inputs.append(all_instances.DERIV_RNG)

        # create maxvalue, minvalue via derivations
        if max:
            max_derivation = derivation_client.createSyncDerivationForNewInfoWithHttpUrl(
                max_agent.agentIRI, host_docker_internal_to_localhost(max_agent.syncDerivationEndpoint),
                max_deriv_inputs, ONTODERIVATION_DERIVATION
            )
            all_instances.DERIV_MAX = max_derivation.getIri()
            logger.info("Created MaxValue derivation <" + all_instances.DERIV_MAX + ">")

            maxValue_iris = max_derivation.getBelongsToIris(RANDOM_EXAMPLE_MAXVALUE)
            if len(maxValue_iris) > 1:
                raise Exception("MaxValue derivation should have maximum one maxValue as output, but got " + maxValue_iris)
            elif len(maxValue_iris) == 0:
                logger.info("No MaxValue instance created")
                diff_deriv_inputs.append(all_instances.DERIV_MAX)
            else:
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
            min_derivation = derivation_client.createSyncDerivationForNewInfoWithHttpUrl(
                min_agent.agentIRI, host_docker_internal_to_localhost(min_agent.syncDerivationEndpoint),
                min_deriv_inputs, ONTODERIVATION_DERIVATION
            )
            all_instances.DERIV_MIN = min_derivation.getIri()
            logger.info("Created MinValue derivation <" + all_instances.DERIV_MIN + ">")

            minValue_iris = min_derivation.getBelongsToIris(RANDOM_EXAMPLE_MINVALUE)
            if len(minValue_iris) > 1:
                raise Exception("MinValue derivation should have maximum one minValue as output, but got " + minValue_iris)
            elif len(minValue_iris) == 0:
                logger.info("No MinValue instance created")
                diff_deriv_inputs.append(all_instances.DERIV_MIN)
            else:
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
            diff_derivation = derivation_client.createSyncDerivationForNewInfoWithHttpUrl(
                diff_agent.agentIRI, host_docker_internal_to_localhost(diff_agent.syncDerivationEndpoint),
                diff_deriv_inputs, ONTODERIVATION_DERIVATION
            )
            all_instances.DERIV_DIFF = diff_derivation.getIri()
            logger.info("Created Difference derivation <" + all_instances.DERIV_DIFF + ">")

            diff_iris = diff_derivation.getBelongsToIris(RANDOM_EXAMPLE_DIFFERENCE)
            if len(diff_iris) > 1:
                raise Exception("Difference derivation should have maximun one difference as output, but got " + diff_iris)
            elif len(diff_iris) == 0:
                logger.info("No Difference instance created")
            else:
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

    # for each created instance, test that no output is created as the number of points is 0
    assert len(sparql_client.getPointsInKG()) == 0
    assert not all_instances.IRI_LST_PTS

    assert not bool(sparql_client.getMaxValueIRI())
    assert not all_instances.IRI_MAX

    assert not bool(sparql_client.getMinValueIRI())
    assert not all_instances.IRI_MIN

    assert not bool(sparql_client.getDifferenceIRI())
    assert not all_instances.IRI_DIFF

    # get IRI of difference derivation
    difference_derivation = all_instances.DERIV_DIFF

    # once timestamp of difference derivation updated, all information in the KG
    # should also be up-to-date
    currentTimestamp_difference_derivation = utils.get_timestamp(difference_derivation, sparql_client)
    while currentTimestamp_difference_derivation == 0:
        time.sleep(8)
        currentTimestamp_difference_derivation = utils.get_timestamp(difference_derivation, sparql_client)

    # now all information should be up-to-date
    assert_all_information_are_up_to_date(all_instances, sparql_client, False)
    return all_instances


def execute_update_pure_input(sparql_client, derivation_client, change_num_of_pts_to: int=None):
    # save information about the old NumberOfPoints instance
    numOfPoint = sparql_client.getNumOfPoints()
    numOfPoints_val_old = sparql_client.getValue(numOfPoint)
    numOfPoints_timestamp_old = utils.get_timestamp(numOfPoint, sparql_client)

    # modify the value of num of points and update the timestamp
    if change_num_of_pts_to is not None:
        # change the value to the given value if provided
        sparql_client.updateNumOfPointsTo(change_num_of_pts_to)
    else:
        # by default, increase the value by 1
        sparql_client.increaseNumOfPointsByOne()
    derivation_client.updateTimestamp(numOfPoint)

    # get information about new NumberOfPoints instance
    numOfPoints_val_new = sparql_client.getValue(numOfPoint)
    numOfPoints_timestamp_new = utils.get_timestamp(numOfPoint, sparql_client)

    # test if InputAgent increased the value
    if change_num_of_pts_to is not None:
        assert numOfPoints_val_new == change_num_of_pts_to
    else:
        assert numOfPoints_val_new == numOfPoints_val_old + 1
    # test if InputAgent modified the timestamp
    assert numOfPoints_timestamp_new > numOfPoints_timestamp_old

    logger.debug(f"Pure input number of points <{numOfPoint}> updated from {numOfPoints_val_old} to {numOfPoints_val_new}")


def execute_update_derivation(all_instances, sparql_client, update_endpoint, exist_outputs):
    # get IRI of difference derivation
    difference_derivation = all_instances.DERIV_DIFF
    # get old timestamp of difference derivation
    difference_derivation_timestamp = utils.get_timestamp(difference_derivation, sparql_client)
    # get information about old instance of difference
    difference_instance_old = sparql_client.getDifferenceIRI()

    # invoke UpdateDerivations via HTTP request
    # endpoint of update agent in docker, e.g., "http://localhost:7004/update"
    response = requests.post(update_endpoint, timeout=120)
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
    assert_all_information_are_up_to_date(all_instances, sparql_client, exist_outputs)
