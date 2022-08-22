import pytest
import time

import rxnoptgoaliteragent.tests.conftest as cf

# ----------------------------------------------------------------------------------
# Test cases for sparql_client
# ----------------------------------------------------------------------------------

def test_get_goal_set_instance(initialise_test_triples):
    sparql_client = initialise_test_triples
    goal_set = sparql_client.get_goal_set_instance(cf.IRIs.GOALSET_1.value)
    assert goal_set.instance_iri == cf.IRIs.GOALSET_1.value

    # restriction
    assert goal_set.hasRestriction.instance_iri == cf.IRIs.RESTRICTION_1.value
    assert goal_set.hasRestriction.cycleAllowance == cf.IRIs.RESTRICTION_1_CYCLEALLOWANCE.value
    assert goal_set.hasRestriction.deadline == cf.IRIs.RESTRICTION_1_DEADLINE.value

    # goal
    assert len(goal_set.hasGoal) == 2
    if goal_set.hasGoal[0].instance_iri == cf.IRIs.GOAL_1.value:
        assert goal_set.hasGoal[1].instance_iri == cf.IRIs.GOAL_2.value
        goal_1 = goal_set.hasGoal[0]
        goal_2 = goal_set.hasGoal[1]
    else:
        assert goal_set.hasGoal[0].instance_iri == cf.IRIs.GOAL_2.value
        assert goal_set.hasGoal[1].instance_iri == cf.IRIs.GOAL_1.value
        goal_1 = goal_set.hasGoal[1]
        goal_2 = goal_set.hasGoal[0]

    # desired quantity
    assert goal_1.desires().instance_iri == cf.IRIs.DESIRED_QUANTITY_1.value
    assert goal_1.desiresGreaterThan.instance_iri == cf.IRIs.DESIRED_QUANTITY_1.value
    assert goal_1.desiresGreaterThan.clz == cf.IRIs.DESIRED_QUANTITY_1_TYPE.value
    assert goal_1.desiresGreaterThan.hasValue.instance_iri == cf.IRIs.DESIRED_QUANTITY_MEASURE_1.value
    assert goal_1.desiresGreaterThan.hasValue.hasUnit == cf.IRIs.DESIRED_QUANTITY_MEASURE_1_UNIT.value
    assert goal_1.desiresGreaterThan.hasValue.hasNumericalValue == cf.IRIs.DESIRED_QUANTITY_MEASURE_1_NUMVAL.value
    assert goal_1.desiresLessThan is None
    assert goal_2.desires().instance_iri == cf.IRIs.DESIRED_QUANTITY_2.value
    assert goal_2.desiresGreaterThan is None
    assert goal_2.desiresLessThan.instance_iri == cf.IRIs.DESIRED_QUANTITY_2.value
    assert goal_2.desiresLessThan.clz == cf.IRIs.DESIRED_QUANTITY_2_TYPE.value
    assert goal_2.desiresLessThan.hasValue.instance_iri == cf.IRIs.DESIRED_QUANTITY_MEASURE_2.value
    assert goal_2.desiresLessThan.hasValue.hasUnit == cf.IRIs.DESIRED_QUANTITY_MEASURE_2_UNIT.value
    assert goal_2.desiresLessThan.hasValue.hasNumericalValue == cf.IRIs.DESIRED_QUANTITY_MEASURE_2_NUMVAL.value

    # plan
    assert len(goal_1.hasPlan) == 1
    assert goal_1.hasPlan[0].instance_iri == cf.IRIs.RXN_OPT_PLAN.value
    assert len(goal_2.hasPlan) == 1
    assert goal_2.hasPlan[0].instance_iri == cf.IRIs.RXN_OPT_PLAN.value

    # step
    for goal in [goal_1, goal_2]:
        assert len(goal.hasPlan[0].hasStep) == 3

        # step 1 - doe
        doe_step = goal.hasPlan[0].get_step(cf.ONTOGOAL_DESIGNOFEXPERIMENT)
        assert doe_step.instance_iri == cf.IRIs.STEP_DOE.value
        assert len(doe_step.canBePerformedBy) == 1
        assert doe_step.canBePerformedBy[0] == cf.IRIs.STEP_DOE_AGENT.value
        assert len(doe_step.hasNextStep) == 1
        assert doe_step.hasNextStep[0].instance_iri == cf.IRIs.STEP_EXE.value
        assert len(doe_step.hasNextStep[0].canBePerformedBy) == 1
        assert doe_step.hasNextStep[0].canBePerformedBy[0] == cf.IRIs.STEP_EXE_AGENT.value
        assert len(doe_step.hasNextStep[0].hasNextStep) == 1
        assert doe_step.hasNextStep[0].hasNextStep[0].instance_iri == cf.IRIs.STEP_POSTPRO.value
        assert len(doe_step.hasNextStep[0].hasNextStep[0].canBePerformedBy) == 1
        assert doe_step.hasNextStep[0].hasNextStep[0].canBePerformedBy[0] == cf.IRIs.STEP_POSTPRO_AGENT.value
        assert doe_step.hasNextStep[0].hasNextStep[0].hasNextStep is None

        # step 2 - exe
        exe_step = goal.hasPlan[0].get_step(cf.ONTOGOAL_RXNEXPEXECUTION)
        assert exe_step.instance_iri == cf.IRIs.STEP_EXE.value
        assert len(exe_step.canBePerformedBy) == 1
        assert exe_step.canBePerformedBy[0] == cf.IRIs.STEP_EXE_AGENT.value
        assert len(exe_step.hasNextStep) == 1
        assert exe_step.hasNextStep[0].instance_iri == cf.IRIs.STEP_POSTPRO.value
        assert len(exe_step.hasNextStep[0].canBePerformedBy) == 1
        assert exe_step.hasNextStep[0].canBePerformedBy[0] == cf.IRIs.STEP_POSTPRO_AGENT.value
        assert exe_step.hasNextStep[0].hasNextStep is None

        # step 3 - postpro
        postpro_step = goal.hasPlan[0].get_step(cf.ONTOGOAL_POSTPROCESSING)
        assert postpro_step.instance_iri == cf.IRIs.STEP_POSTPRO.value
        assert len(postpro_step.canBePerformedBy) == 1
        assert postpro_step.canBePerformedBy[0] == cf.IRIs.STEP_POSTPRO_AGENT.value
        assert postpro_step.hasNextStep is None

    # result
    assert len(goal_1.hasResult) == 1
    assert goal_1.hasResult[0].instance_iri == cf.IRIs.RESULT_QUANTITY_1.value
    assert goal_1.hasResult[0].clz == cf.IRIs.RESULT_QUANTITY_1_TYPE.value
    assert goal_1.hasResult[0].hasValue.instance_iri == cf.IRIs.RESULT_QUANTITY_MEASURE_1.value
    assert goal_1.hasResult[0].hasValue.hasUnit == cf.IRIs.RESULT_QUANTITY_MEASURE_1_UNIT.value
    assert goal_1.hasResult[0].hasValue.hasNumericalValue == cf.IRIs.RESULT_QUANTITY_MEASURE_1_NUMVAL.value
    assert goal_2.hasResult is None
