from builtins import max as builtins_max
from builtins import min as builtins_min
import requests
import pytest
import random
import time

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
def test_docker_integration(initialise_clients, rng, max, min, diff):
    sparql_client, derivation_client, update_endpoint = initialise_clients

    logger.info(f"==================== Test case [{rng}-{max}-{min}-{diff}] start ====================")

    logger.info(
        f"""Testing derivation DAG (True for sync, False for async):
        rng_derivation [{rng}], max_derivation [{max}], min_derivation [{min}], diff_derivation [{diff}]."""
    )

    ################################################################
    ## I. Upload all initial triples prepared in resources folder ##
    ################################################################
    all_instances = utils.initialise_triples_assert_pure_inputs(
        sparql_client=sparql_client,
        derivation_client=derivation_client
    )

    logger.info("Uploading prepared triples finished.")

    ###############################################################
    ## II. Initialise derivations that generates new information ##
    ###############################################################
    all_instances = execute_test_generate_new_info(
        all_instances=all_instances,
        sparql_client=sparql_client,
        derivation_client=derivation_client,
        rng=rng, max=max, min=min, diff=diff
    )

    logger.info("New information generation finished.")

    #####################################################################
    ## III. Update one of the pure inputs to make derivations outdated ##
    #####################################################################
    execute_update_pure_input(sparql_client=sparql_client, derivation_client=derivation_client)

    logger.info("Pure input modification finished, the derivations are now outdated.")

    ##############################################
    ## IV. Request for an update of derivations ##
    ##############################################
    execute_update_derivation(
        all_instances=all_instances,
        sparql_client=sparql_client,
        update_endpoint=update_endpoint
    )

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

    logger.debug("All information in the knowledge graph are correct and up-to-date.")


def initialise_derivations(all_instances, sparql_client, derivation_client, rng: bool, max: bool, min: bool, diff: bool):
    pure_inputs = [all_instances.IRI_UPPER_LIMIT, all_instances.IRI_LOWER_LIMIT, all_instances.IRI_NUM_OF_PTS]
    max_deriv_inputs = []
    min_deriv_inputs = []
    diff_deriv_inputs = []

    if rng:
        new_points = []
        for i in range(all_instances.VAL_NUM_OF_PTS):
            new_points.append(random.randint(all_instances.VAL_LOWER_LIMIT, all_instances.VAL_UPPER_LIMIT))
        all_instances.IRI_LST_PTS, pt_iris = sparql_client.createListOfPoints(new_points)
        max_deriv_inputs.append(all_instances.IRI_LST_PTS)
        min_deriv_inputs.append(all_instances.IRI_LST_PTS)

        all_instances.DERIV_RNG = derivation_client.createDerivation(
            [all_instances.IRI_LST_PTS] + pt_iris,
            all_instances.RNGAGENT_SERVICE,
            pure_inputs
        )
        derivation_client.updateTimestamp(all_instances.DERIV_RNG)

        # create maxvalue, minvalue
        if max:
            all_instances.VAL_MAX = builtins_max(new_points)
            all_instances.IRI_MAX = sparql_client.createMaxValue(all_instances.VAL_MAX)
            diff_deriv_inputs.append(all_instances.IRI_MAX)

            all_instances.DERIV_MAX = derivation_client.createDerivation(
                [all_instances.IRI_MAX], all_instances.MAXAGENT_SERVICE, max_deriv_inputs
            )
            derivation_client.updateTimestamp(all_instances.DERIV_MAX)
        else:
            all_instances.DERIV_MAX = derivation_client.createAsyncDerivationForNewInfo(
                all_instances.MAXAGENT_SERVICE, max_deriv_inputs
            )
            diff_deriv_inputs.append(all_instances.DERIV_MAX)

        if min:
            all_instances.VAL_MIN = builtins_min(new_points)
            all_instances.IRI_MIN = sparql_client.createMinValue(all_instances.VAL_MIN)
            diff_deriv_inputs.append(all_instances.IRI_MIN)

            all_instances.DERIV_MIN = derivation_client.createDerivation(
                [all_instances.IRI_MIN], all_instances.MINAGENT_SERVICE, min_deriv_inputs
            )
            derivation_client.updateTimestamp(all_instances.DERIV_MIN)
        else:
            all_instances.DERIV_MIN = derivation_client.createAsyncDerivationForNewInfo(
                all_instances.MINAGENT_SERVICE, min_deriv_inputs
            )
            diff_deriv_inputs.append(all_instances.DERIV_MIN)

        if diff:
            # create difference
            all_instances.VAL_DIFF = all_instances.VAL_MAX - all_instances.VAL_MIN
            all_instances.IRI_DIFF = sparql_client.createDiffValue(all_instances.VAL_DIFF)

            all_instances.DERIV_DIFF = derivation_client.createDerivation(
                [all_instances.IRI_DIFF], all_instances.DIFFAGENT_SERVICE,
                [all_instances.IRI_MAX, all_instances.IRI_MIN]
            )
            derivation_client.updateTimestamp(all_instances.DERIV_DIFF)
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


def execute_test_generate_new_info(all_instances, sparql_client, derivation_client, rng: bool, max: bool, min: bool, diff: bool):
    # initialise all derivations first
    all_instances = initialise_derivations(
        all_instances=all_instances,
        sparql_client=sparql_client,
        derivation_client=derivation_client,
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
    logger.info(f"Update request sent to endpoint <{update_endpoint}>, received response: {response}")

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
