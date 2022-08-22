from datetime import datetime

from rxnoptgoaliteragent.data_model import *
from chemistry_and_robots.kg_operations import *
import chemistry_and_robots.kg_operations.dict_and_list as dal

import logging
logger = logging.getLogger(__name__)

class RxnOptGoalIterSparqlClient(ChemistryAndRobotsSparqlClient):
    # TODO: implement
    def get_goal_set_instance(self, goal_set_iri) -> GoalSet:
        goal_set_iri = trimIRI(goal_set_iri)
        query = f"""{PREFIX_RDFS}
        SELECT ?restriction ?cycleAllowance ?deadline ?goal ?desire ?desiredQuantity ?desiredQuantityType ?desiredQuantityMeasure ?numVal ?unit ?plan ?step ?agent ?nextStep
        WHERE {{
            OPTIONAL {{
                <{goal_set_iri}> <{ONTOGOAL_HASRESTRICTION}> ?restriction.
                ?restriction <{ONTOGOAL_CYCLEALLOWANCE}> ?cycleAllowance; <{ONTOGOAL_DEADLINE}> ?deadline.
            }}
            OPTIONAL {{
                <{goal_set_iri}> <{ONTOGOAL_HASGOAL}> ?goal.
                VALUES ?desire {{ <{ONTOGOAL_DESIRES}> <{ONTOGOAL_DESIRESGREATERTHAN}> <{ONTOGOAL_DESIRESLESSTHAN}> }}
                ?goal ?desire ?desiredQuantity.
                ?desiredQuantity a ?desiredQuantityType; <{OM_HASVALUE}> ?desiredQuantityMeasure.
                ?desiredQuantityMeasure <{OM_HASNUMERICALVALUE}> ?numVal; <{OM_HASUNIT}> ?unit.
                ?goal <{ONTOGOAL_HASPLAN}> ?plan.
                ?plan <{ONTOGOAL_HASSTEP}> ?step.
                ?step <{ONTOGOAL_CANBEPERFORMEDBY}> ?agent.
                OPTIONAL {{?step <{ONTOGOAL_HASNEXTSTEP}> ?nextStep.}}
            }}
        }}"""
        response = self.performQuery(query)
        logger.debug(f"Obtained response: {response} with query: {query}")
        # return None # TODO remove this line

        # get restriction
        restriction_iri = dal.get_the_unique_value_in_list_of_dict(response, "restriction")
        cycle_allowance = dal.get_the_unique_value_in_list_of_dict(response, "cycleAllowance")
        deadline = dal.get_the_unique_value_in_list_of_dict(response, "deadline")
        try:
            deadline = datetime.timestamp(datetime.fromisoformat(deadline))
        except Exception as e:
            deadline = datetime.timestamp(datetime.fromisoformat(deadline[:-1]))

        restriction = Restriction(
            instance_iri=restriction_iri,
            cycleAllowance=int(cycle_allowance),
            deadline=deadline
        )

        # create placeholder dict for steps
        dict_step = {} # format: {'step_iri': Step}
        dict_next_steps = {} # format: {'step_iri': ['next_step_iri_1', 'next_step_iri_2']}

        # get goal
        list_goal = []
        list_goal_iri = dal.get_unique_values_in_list_of_dict(response, "goal")
        for goal_iri in list_goal_iri:
            _goal_info = dal.get_sublist_in_list_of_dict_matching_key_value(response, 'goal', goal_iri)
            _goal_info = dal.remove_unwanted_keys_from_list_of_dict(_goal_info, ['restriction', 'cycleAllowance', 'deadline'])
            _goal_info = dal.remove_duplicate_dict_from_list_of_dict(_goal_info)

            # process desire
            _desire = dal.get_the_unique_value_in_list_of_dict(_goal_info, 'desire')
            _desired_quantity_iri = dal.get_the_unique_value_in_list_of_dict(_goal_info, 'desiredQuantity')
            _desired_quantity_type = dal.get_the_unique_value_in_list_of_dict(_goal_info, 'desiredQuantityType')
            _dqm_iri = dal.get_the_unique_value_in_list_of_dict(_goal_info, 'desiredQuantityMeasure')
            _dqm_unit = dal.get_the_unique_value_in_list_of_dict(_goal_info, 'unit')
            _dqm_num_val = dal.get_the_unique_value_in_list_of_dict(_goal_info, 'numVal')
            _desired_quantity = OM_Quantity(
                instance_iri=_desired_quantity_iri,
                clz=_desired_quantity_type,
                hasValue=OM_Measure(
                    instance_iri=_dqm_iri,
                    hasUnit=_dqm_unit,
                    hasNumericalValue=_dqm_num_val
                )
            )

            # process plan
            _goal_info = dal.remove_unwanted_keys_from_list_of_dict(_goal_info, ['desire', 'desiredQuantity', 'desiredQuantityType', 'desiredQuantityMeasure', 'unit', 'numVal'])
            _goal_info = dal.remove_duplicate_dict_from_list_of_dict(_goal_info)
            _list_plan_iri = dal.get_unique_values_in_list_of_dict(response, "plan")
            _list_plan = []
            for pl_iri in _list_plan_iri:
                _plan_info = dal.get_sublist_in_list_of_dict_matching_key_value(_goal_info, 'plan', pl_iri)
                _plan_info = dal.remove_duplicate_dict_from_list_of_dict(_plan_info)

                # process step
                _list_step_iri = dal.get_unique_values_in_list_of_dict(_plan_info, "step")
                _list_step = []
                for _step_iri in _list_step_iri:
                    _step_info = dal.get_sublist_in_list_of_dict_matching_key_value(_plan_info, 'step', _step_iri)
                    _step_info = dal.remove_duplicate_dict_from_list_of_dict(_step_info)

                    step_instance = Step(
                        instance_iri=_step_iri,
                        canBePerformedBy=dal.get_unique_values_in_list_of_dict(_step_info, 'agent'),
                        # NOTE hasNextStep is processed after all steps are created
                    )
                    _list_step.append(step_instance)
                    dict_step[_step_iri] = step_instance
                    dict_next_steps[_step_iri] = dal.get_unique_values_in_list_of_dict(_step_info, 'nextStep')

                # NOTE here we process the next steps
                for step in _list_step:
                    step.hasNextStep = [dict_step[next_step_iri] for next_step_iri in dict_next_steps[step.instance_iri]]

                _plan = Plan(
                    instance_iri=pl_iri,
                    hasStep=_list_step,
                )
                _list_plan.append(_plan)

            # construct goal
            goal_instance = Goal(
                instance_iri=goal_iri,
                hasPlan=_list_plan,
                desiresGreaterThan=_desired_quantity if _desire == ONTOGOAL_DESIRESGREATERTHAN else None,
                desiresLessThan=_desired_quantity if _desire == ONTOGOAL_DESIRESLESSTHAN else None,
                # hasResult=, # TODO implement this
            )
            list_goal.append(goal_instance)

        goal_set_instance = GoalSet(
            instance_iri=goal_set_iri,
            hasGoal=list_goal,
            hasRestriction=restriction,
        )
        return goal_set_instance

    # TODO: implement
    def generate_doe_instance_from_goal(self, goal_set: GoalSet, rxn_exp_beliefs: List[ReactionExperiment]) -> DesignOfExperiment:
        list_design_variables = []
        list_system_responses = []
        doe_instance = DesignOfExperiment(
            instance_iri=INSTANCE_IRI_TO_BE_INITIALISED,
            namespace_for_init=getNameSpace(goal_set.instance_iri),
            # TODO add support for Strategy defined by user
            usesStrategy=TSEMO(
                instance_iri=INSTANCE_IRI_TO_BE_INITIALISED,
                namespace_for_init=getNameSpace(goal_set.instance_iri),
            ),
            hasDomain=Domain(
                instance_iri=INSTANCE_IRI_TO_BE_INITIALISED,
                namespace_for_init=getNameSpace(goal_set.instance_iri),
                hasDesignVariable=list_design_variables,
            ),
            hasSystemResponse=list_system_responses,
            utilisesHistoricalData=rxn_exp_beliefs,
        )
        pass

    # TODO: implement
    def detect_postpro_derivation_result(
        self, postpro_derivation_iri: str, interested_performance_indicators: list
    ) -> ReactionExperiment:
        postpro_derivation_iri = trimIRI(postpro_derivation_iri)
        if self.check_if_triple_exist(None, ONTODERIVATION_BELONGSTO, postpro_derivation_iri):            
            query = f"""
                SELECT ?rxn_exp
                WHERE {{
                    VALUES ?rxn_rdfType {{ <{ONTOREACTION_REACTIONEXPERIMENT}> <{ONTOREACTION_REACTIONVARIATION}> }}
                    <{postpro_derivation_iri}> <{ONTODERIVATION_ISDERIVEDFROM}> ?rxn_exp. 
                    ?rxn_exp a ?rxn_rdfType.
                }}"""
            response = self.performQuery(query)
            if len(response) != 1:
                raise Exception(f"""Exactly one OntoReaction:ReactionExperiment or OntoReaction:ReactionVariation 
                is expected as input of HPLCPostProDerivation {postpro_derivation_iri}, but found: {response}""")
        pass
