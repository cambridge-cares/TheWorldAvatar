from __future__ import annotations

import pydantic
from typing import List, Optional

import chemistry_and_robots.data_model as dm
from rxnoptgoaliteragent.data_model import iris as iris


class Step(dm.BaseOntology):
    clz: str = iris.ONTOGOAL_STEP
    canBePerformedBy: List[str]
    hasNextStep: Optional[List[Step]]


class Plan(dm.BaseOntology):
    clz: str = iris.ONTOGOAL_PLAN
    hasStep: List[Step]

    def get_step(self, step_iri: str) -> Optional[Step]:
        for step in self.hasStep:
            if step.clz == step_iri:
                return step
        return None


class Goal(dm.BaseOntology):
    clz: str = iris.ONTOGOAL_GOAL
    hasPlan: List[Plan]
    desiresGreaterThan: Optional[dm.OM_Quantity]
    desiresLessThan: Optional[dm.OM_Quantity]
    # NOTE desires is implemented as either desiresGreaterThan or desiresLessThan, to be generated on the fly
    hasResult: Optional[List[dm.OM_Quantity]]

    @pydantic.root_validator
    @classmethod
    def desires_subproperty(cls, values):
        # either desiresGreaterThan or desiresLessThan
        if values.get('desiresGreaterThan') is None and values.get('desiresLessThan') is None:
            raise ValueError(f"desiresGreaterThan and desiresLessThan cannot both be None for Goal {values.get('instance_iri')}")
        elif values.get('desiresGreaterThan') is not None and values.get('desiresLessThan') is not None:
            raise ValueError(f"desiresGreaterThan and desiresLessThan cannot both be NOT None for Goal {values.get('instance_iri')}")
        return values

    def desires(self) -> dm.OM_Quantity:
        if self.desiresGreaterThan is not None:
            return self.desiresGreaterThan
        elif self.desiresLessThan is not None:
            return self.desiresLessThan
        else:
            raise ValueError(f"desiresGreaterThan and desiresLessThan cannot both be None for Goal {self.instance_iri}")

class Restriction(dm.BaseOntology):
    clz: str = iris.ONTOGOAL_RESTRICTION
    cycleAllowance: int
    deadline: float


class GoalSet(dm.BaseOntology):
    clz: str = iris.ONTOGOAL_GOALSET
    hasGoal: List[Goal]
    hasRestriction: Restriction

    def get_goal_given_desired_quantity(self, desired_quantity: str) -> Optional[Goal]:
        for goal in self.hasGoal:
            if goal.desires.instance_iri == desired_quantity:
                return goal
        return None


class Result(dm.BaseOntology):
    clz: str = iris.ONTOGOAL_RESULT
    refersTo: str


#########################################
## Put all update_forward_refs() below ##
#########################################
Step.update_forward_refs()
