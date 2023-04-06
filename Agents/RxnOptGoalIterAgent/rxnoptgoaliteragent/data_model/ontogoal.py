from __future__ import annotations

from datetime import datetime
from rdflib import Literal
from rdflib import URIRef
from rdflib import Graph
from rdflib import RDF
from rdflib import XSD
from typing import Optional
from typing import List
from typing import Dict
import pydantic
import time

import chemistry_and_robots.data_model as dm
from rxnoptgoaliteragent.data_model import iris as iris


class Step(dm.BaseOntology):
    clz: str = iris.ONTOGOAL_STEP
    canBePerformedBy: List[str]
    hasNextStep: Optional[List[Step]]

    def create_instance_for_kg(self, g: Graph) -> Graph:
        g.add((URIRef(self.instance_iri), RDF.type, URIRef(self.clz)))

        for agent in self.canBePerformedBy:
            g.add((URIRef(self.instance_iri), URIRef(iris.ONTOGOAL_CANBEPERFORMEDBY), URIRef(agent)))

        # NOTE that the following is not a recursive call, but a call to the method of Step
        # NOTE it should be noted tho, multiple SAME triples can be added to the graph
        # NOTE but this is considered as safe for now as the rdflib.Graph will eliminate the duplicates
        for step in self.hasNextStep:
            g.add((URIRef(self.instance_iri), URIRef(iris.ONTOGOAL_HASNEXTSTEP), URIRef(step.instance_iri)))
            step.create_instance_for_kg(g)

        return g


class Plan(dm.BaseOntology):
    clz: str = iris.ONTOGOAL_PLAN
    hasStep: List[Step]

    def get_step(self, step_iri: str) -> Optional[Step]:
        for step in self.hasStep:
            if step.clz == step_iri:
                return step
        return None

    def create_instance_for_kg(self, g: Graph) -> Graph:
        g.add((URIRef(self.instance_iri), RDF.type, URIRef(self.clz)))

        for step in self.hasStep:
            g.add((URIRef(self.instance_iri), URIRef(iris.ONTOGOAL_HASSTEP), URIRef(step.instance_iri)))
            step.create_instance_for_kg(g)

        return g


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

    def get_best_result(self) -> Optional[dm.OM_Measure]:
        if self.hasResult is None:
            return None
        dct_result = {res.instance_iri:dm.unit_conv.unit_conversion_return_value(
            res.hasValue.hasNumericalValue, res.hasValue.hasUnit, self.desires().hasValue.hasUnit
        ) for res in self.hasResult}
        if self.desiresGreaterThan is not None:
            return self.retrieve_result_given_iri(max(dct_result, key=dct_result.get))
        else:
            return self.retrieve_result_given_iri(min(dct_result, key=dct_result.get))

    def retrieve_result_given_iri(self, result_iri: str) -> Optional[dm.OM_Quantity]:
        if self.hasResult is None:
            return None
        for result in self.hasResult:
            if result.instance_iri == result_iri:
                return result
        return None

    def if_goal_met(self) -> bool:
        if self.hasResult is None:
            return False
        # the code will only execute below if there's previous results presented already
        # first we compile a list of previous results in the unit of the goal desires
        list_results = [dm.unit_conv.unit_conversion_return_value(
            res.hasValue.hasNumericalValue, res.hasValue.hasUnit, self.desires().hasValue.hasUnit
        ) for res in self.hasResult]
        if self.desiresGreaterThan is not None:
            # then means we want the result to be greater than the desires
            if max(list_results) >= self.desires().hasValue.hasNumericalValue:
                return True
        else:
            if min(list_results) <= self.desires().hasValue.hasNumericalValue:
                return True
        return False

    def create_instance_for_kg(self, g: Graph) -> Graph:
        g.add((URIRef(self.instance_iri), RDF.type, URIRef(self.clz)))

        for plan in self.hasPlan:
            g.add((URIRef(self.instance_iri), URIRef(iris.ONTOGOAL_HASPLAN), URIRef(plan.instance_iri)))
            # NOTE here we don't need to create the plan instance, as it should already be part of KG

        if self.desiresGreaterThan is not None:
            g.add((URIRef(self.instance_iri), URIRef(iris.ONTOGOAL_DESIRESGREATERTHAN), URIRef(self.desiresGreaterThan.instance_iri)))
            g.add((URIRef(self.desiresGreaterThan.instance_iri), RDF.type, URIRef(self.desiresGreaterThan.clz)))
            g.add((URIRef(self.desiresGreaterThan.instance_iri), URIRef(iris.OM_HASVALUE), URIRef(self.desiresGreaterThan.hasValue.instance_iri)))
            g = self.desiresGreaterThan.hasValue.create_instance_for_kg(g)
        elif self.desiresLessThan is not None:
            g.add((URIRef(self.instance_iri), URIRef(iris.ONTOGOAL_DESIRESLESSTHAN), URIRef(self.desiresLessThan.instance_iri)))
            g.add((URIRef(self.desiresLessThan.instance_iri), RDF.type, URIRef(self.desiresLessThan.clz)))
            g.add((URIRef(self.desiresLessThan.instance_iri), URIRef(iris.OM_HASVALUE), URIRef(self.desiresLessThan.hasValue.instance_iri)))
            g = self.desiresLessThan.hasValue.create_instance_for_kg(g)
        else:
            raise ValueError(f"desiresGreaterThan and desiresLessThan cannot both be None for Goal {self.instance_iri}")

        # TODO think about if we need to create instances for hasResult

        return g


class Restriction(dm.BaseOntology):
    clz: str = iris.ONTOGOAL_RESTRICTION
    cycleAllowance: int
    deadline: float

    def create_instance_for_kg(self, g: Graph) -> Graph:
        g.add((URIRef(self.instance_iri), RDF.type, URIRef(self.clz)))
        g.add((URIRef(self.instance_iri), URIRef(iris.ONTOGOAL_CYCLEALLOWANCE), Literal(self.cycleAllowance)))
        g.add((URIRef(self.instance_iri), URIRef(iris.ONTOGOAL_DEADLINE), Literal(datetime.fromtimestamp(self.deadline), datatype=XSD.dateTime)))
        return g


class GoalSet(dm.BaseOntology):
    clz: str = iris.ONTOGOAL_GOALSET
    hasGoal: List[Goal]
    hasRestriction: Restriction

    def get_goal_given_desired_quantity(self, desired_quantity: str) -> Optional[Goal]:
        for goal in self.hasGoal:
            if goal.desires().instance_iri == desired_quantity:
                return goal
        return None

    def get_unmet_goals(self) -> List[Goal]:
        return [goal for goal in self.hasGoal if not goal.if_goal_met()]

    def if_restrictions_are_okay(self) -> bool:
        if self.hasRestriction.cycleAllowance > 0 and self.hasRestriction.deadline > time.time():
            return True
        return False

    def get_best_results(self) -> Dict[str, dm.OM_Quantity]:
        return {goal.instance_iri:goal.get_best_result() for goal in self.hasGoal if goal.get_best_result() is not None}

    def create_instance_for_kg(self, g: Graph) -> Graph:
        g.add((URIRef(self.instance_iri), RDF.type, URIRef(self.clz)))

        for goal in self.hasGoal:
            g.add((URIRef(self.instance_iri), URIRef(iris.ONTOGOAL_HASGOAL), URIRef(goal.instance_iri)))
            g = goal.create_instance_for_kg(g)

        g.add((URIRef(self.instance_iri), URIRef(iris.ONTOGOAL_HASRESTRICTION), URIRef(self.hasRestriction.instance_iri)))
        g = self.hasRestriction.create_instance_for_kg(g)

        return g


class Result(dm.BaseOntology):
    clz: str = iris.ONTOGOAL_RESULT
    refersTo: str


#########################################
## Put all update_forward_refs() below ##
#########################################
Step.update_forward_refs()
