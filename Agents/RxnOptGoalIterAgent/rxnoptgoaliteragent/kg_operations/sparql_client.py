from datetime import datetime

from rxnoptgoaliteragent.data_model import *
from chemistry_and_robots.kg_operations import *
import chemistry_and_robots.kg_operations.dict_and_list as dal

from py4jps import agentlogging
logger = agentlogging.get_logger('dev')

class RxnOptGoalIterSparqlClient(ChemistryAndRobotsSparqlClient):
    def get_goal_set_instance(self, goal_set_iri) -> GoalSet:
        goal_set_iri = trimIRI(goal_set_iri)
        query = f"""{PREFIX_RDFS}
        SELECT ?restriction ?cycleAllowance ?deadline
            ?goal ?desire ?desiredQuantity ?desiredQuantityType ?desiredQuantityMeasure ?numVal ?unit
            ?plan ?step ?stepType ?agent ?nextStep
            ?resultQuantity ?resultQuantityType ?resultQuantityMeasure ?resultNumVal ?resultUnit

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
                ?step a ?stepType; <{ONTOGOAL_CANBEPERFORMEDBY}> ?agent.
                OPTIONAL {{?step <{ONTOGOAL_HASNEXTSTEP}> ?nextStep.}}

                OPTIONAL {{
                    ?goal <{ONTOGOAL_HASRESULT}> ?resultQuantity.
                    ?resultQuantity a ?resultQuantityType; <{OM_HASVALUE}> ?resultQuantityMeasure.
                    ?resultQuantityMeasure <{OM_HASNUMERICALVALUE}> ?resultNumVal; <{OM_HASUNIT}> ?resultUnit.
                }}
            }}
        }}"""
        response = self.performQuery(query)

        # return None if no goal set is found with the given goal_set_iri
        if not bool(response) or not bool(response[0]):
            return None

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
                        clz=dal.get_the_unique_value_in_list_of_dict(_step_info, 'stepType'),
                        canBePerformedBy=dal.get_unique_values_in_list_of_dict(_step_info, 'agent'),
                        # NOTE hasNextStep is processed after all steps are created
                    )
                    _list_step.append(step_instance)
                    dict_step[_step_iri] = step_instance
                    dict_next_steps[_step_iri] = dal.get_unique_values_in_list_of_dict(_step_info, 'nextStep')

                # NOTE here we process the next steps
                for step in _list_step:
                    _temp_step_list = [dict_step[next_step_iri] for next_step_iri in dict_next_steps[step.instance_iri]]
                    step.hasNextStep = _temp_step_list if len(_temp_step_list) > 0 else None

                _plan = Plan(
                    instance_iri=pl_iri,
                    hasStep=_list_step,
                )
                _list_plan.append(_plan)

            # process result
            _goal_result_info = dal.remove_unwanted_keys_from_list_of_dict(_goal_info, ['plan', 'step', 'stepType', 'agent', 'nextStep'])
            _goal_result_info = dal.remove_duplicate_dict_from_list_of_dict(_goal_result_info)
            logger.debug(f"_goal_result_info: {_goal_result_info}")
            _list_result_iri = dal.get_unique_values_in_list_of_dict(_goal_result_info, "resultQuantity")
            if len(_list_result_iri) > 0:
                _list_result_quantity = []
                for result_iri in _list_result_iri:
                    _result_info = dal.get_sublist_in_list_of_dict_matching_key_value(_goal_result_info, 'resultQuantity', result_iri)
                    _result_quantity = OM_Quantity(
                        instance_iri=dal.get_the_unique_value_in_list_of_dict(_result_info, 'resultQuantity'),
                        clz=dal.get_the_unique_value_in_list_of_dict(_result_info, 'resultQuantityType'),
                        hasValue=OM_Measure(
                            instance_iri=dal.get_the_unique_value_in_list_of_dict(_result_info, 'resultQuantityMeasure'),
                            hasUnit=dal.get_the_unique_value_in_list_of_dict(_result_info, 'resultUnit'),
                            hasNumericalValue=dal.get_the_unique_value_in_list_of_dict(_result_info, 'resultNumVal')
                        )
                    )
                    _list_result_quantity.append(_result_quantity)
            else:
                _list_result_quantity = None

            # construct goal
            goal_instance = Goal(
                instance_iri=goal_iri,
                hasPlan=_list_plan,
                desiresGreaterThan=_desired_quantity if _desire == ONTOGOAL_DESIRESGREATERTHAN else None,
                desiresLessThan=_desired_quantity if _desire == ONTOGOAL_DESIRESLESSTHAN else None,
                hasResult=_list_result_quantity,
            )
            list_goal.append(goal_instance)

        goal_set_instance = GoalSet(
            instance_iri=goal_set_iri,
            hasGoal=list_goal,
            hasRestriction=restriction,
        )
        return goal_set_instance

    def generate_doe_instance_from_goal(
        self,
        goal_set: GoalSet,
        chem_rxn: OntoCAPE_ChemicalReaction,
        rxn_exp_as_beliefs: List[ReactionExperiment]=None,
    ) -> DesignOfExperiment:
        # get the doe template
        doe_template = self.get_doe_instance(chem_rxn.hasDoETemplate)

        # process design variables
        list_design_variables = []
        # NOTE here we are taking the design variables from the doe template
        for var in doe_template.hasDomain.hasDesignVariable:
            if isinstance(var, ContinuousVariable):
                design_var = ContinuousVariable(
                    instance_iri=INSTANCE_IRI_TO_BE_INITIALISED,
                    namespace_for_init=getNameSpace(var.instance_iri),
                    upperLimit=var.upperLimit,
                    lowerLimit=var.lowerLimit,
                    positionalID=var.positionalID,
                    refersTo=OM_Quantity(
                        instance_iri=INSTANCE_IRI_TO_BE_INITIALISED,
                        namespace_for_init=getNameSpace(var.refersTo.instance_iri),
                        clz=var.refersTo.clz,
                        hasUnit=var.refersTo.hasUnit,
                    ),
                )
                list_design_variables.append(design_var)
            elif isinstance(var, CategoricalVariable):
                # TODO [future work]: implement
                raise NotImplementedError(f"Design variable type {type(var)} is not implemented yet.")
            else:
                raise NotImplementedError(f"Design variable type {type(var)} is not implemented yet.")

        # process fixed parameters
        list_fixed_parameters = []
        for param in doe_template.hasDomain.hasFixedParameter:
            fixed_param = FixedParameter(
                instance_iri=INSTANCE_IRI_TO_BE_INITIALISED,
                namespace_for_init=getNameSpace(param.instance_iri),
                positionalID=param.positionalID,
                refersTo=OM_Quantity(
                    instance_iri=INSTANCE_IRI_TO_BE_INITIALISED,
                    namespace_for_init=getNameSpace(param.refersTo.instance_iri),
                    clz=param.refersTo.clz,
                    hasValue=OM_Measure(
                        instance_iri=INSTANCE_IRI_TO_BE_INITIALISED,
                        namespace_for_init=getNameSpace(param.refersTo.hasValue.instance_iri),
                        hasUnit=param.refersTo.hasValue.hasUnit,
                        hasNumericalValue=param.refersTo.hasValue.hasNumericalValue,
                    ),
                )
            )
            list_fixed_parameters.append(fixed_param)

        # process system responses
        list_system_responses = []
        for goal in goal_set.hasGoal:
            boolean_maximise = True if goal.desiresGreaterThan is not None else False
            sys_res = SystemResponse(
                instance_iri=INSTANCE_IRI_TO_BE_INITIALISED,
                namespace_for_init=getNameSpace(goal_set.instance_iri),
                maximise=boolean_maximise,
                # positionalID=None, # TODO [only when we are supporting same type of goal]
                refersTo=goal.desires().clz,
            )
            list_system_responses.append(sys_res)

        # construct doe domain
        constructed_domain = Domain(
            instance_iri=INSTANCE_IRI_TO_BE_INITIALISED,
            namespace_for_init=getNameSpace(goal_set.instance_iri),
            hasDesignVariable=list_design_variables,
            hasFixedParameter=list_fixed_parameters,
        )

        # filter out the experiments that are not in the domain
        filtered_rxn_exp_as_beliefs = constructed_domain.filter_reaction_experiment_as_beliefs(rxn_exp_as_beliefs)
        logger.info(f"Reaction experiments chosen as beliefs:{[rxn.instance_iri for rxn in filtered_rxn_exp_as_beliefs]}")

        # construct design of experiment instance
        doe_instance = DesignOfExperiment(
            instance_iri=INSTANCE_IRI_TO_BE_INITIALISED,
            namespace_for_init=getNameSpace(goal_set.instance_iri),
            # TODO [nice-to-have] add support for Strategy defined by user
            # NOTE at the moment, we use TSEMO and its default parameters
            usesStrategy=TSEMO(
                instance_iri=INSTANCE_IRI_TO_BE_INITIALISED,
                namespace_for_init=getNameSpace(goal_set.instance_iri),
            ),
            hasDomain=constructed_domain,
            hasSystemResponse=list_system_responses,
            utilisesHistoricalData=HistoricalData(
                instance_iri=INSTANCE_IRI_TO_BE_INITIALISED,
                namespace_for_init=getNameSpace(goal_set.instance_iri),
                refersTo=filtered_rxn_exp_as_beliefs,
                # NOTE in utilisesHistoricalData, the default value 1 is used for numOfNewExp
            ),
            designsChemicalReaction=chem_rxn.instance_iri,
        )

        return doe_instance

    def detect_postpro_derivation_result(
        self, postpro_derivation_iri: str, interested_performance_indicators: list
    ) -> ReactionExperiment:
        postpro_derivation_iri = trimIRI(postpro_derivation_iri)
        interested_performance_indicators = trimIRI(interested_performance_indicators)
        query = f"""
            SELECT DISTINCT ?rxn_exp
            WHERE {{
                VALUES ?performance_indicator_type {{ <{'> <'.join(interested_performance_indicators)}> }}
                ?performance_indicator <{ONTODERIVATION_BELONGSTO}> <{postpro_derivation_iri}>; a ?performance_indicator_type.
                VALUES ?rxn_rdfType {{ <{ONTOREACTION_REACTIONEXPERIMENT}> <{ONTOREACTION_REACTIONVARIATION}> }}
                ?rxn_exp ?has_performance_indicator ?performance_indicator; a ?rxn_rdfType.
            }}"""
        response = self.performQuery(query)
        if len(response) == 0:
            logger.info(f'No reaction experiment found for postpro derivation <{postpro_derivation_iri}> yet')
            return None
        if len(response) > 1:
            raise Exception(f"""Exactly one OntoReaction:ReactionExperiment or OntoReaction:ReactionVariation 
            is expected to be connected to <{postpro_derivation_iri}>, but found: {response}""")
        rxn_exp_instance = self.getReactionExperiment(response[0]['rxn_exp'])
        if len(rxn_exp_instance) != 1:
            raise Exception(f"""Exactly one OntoReaction:ReactionExperiment or OntoReaction:ReactionVariation 
            is expected to be identified by <{response[0]['rxn_exp']}>, but found: {rxn_exp_instance}""")
        return rxn_exp_instance[0]

    def get_goal_plan(self, goal_plan_iri_or_list: str or list) -> List[Plan]:
        goal_plan_iri_or_list = trimIRI(goal_plan_iri_or_list)
        if not isinstance(goal_plan_iri_or_list, list):
            goal_plan_iri_or_list = [goal_plan_iri_or_list]
        query = f"""
        SELECT ?plan ?step ?stepType ?agent ?nextStep
        WHERE {{
            VALUES ?plan {{ <{'> <'.join(goal_plan_iri_or_list)}> }}
            ?plan <{ONTOGOAL_HASSTEP}> ?step.
            ?step a ?stepType; <{ONTOGOAL_CANBEPERFORMEDBY}> ?agent.
            OPTIONAL {{?step <{ONTOGOAL_HASNEXTSTEP}> ?nextStep.}}
        }}"""
        response = self.performQuery(query)

        # create placeholder dict for steps
        dict_step = {} # format: {'step_iri': Step}
        dict_next_steps = {} # format: {'step_iri': ['next_step_iri_1', 'next_step_iri_2']}

        # NOTE that _list_plan_iri is different from goal_plan_iri_or_list
        # as not all plans in goal_plan_iri_or_list may be presented in the KG
        _list_plan_iri = dal.get_unique_values_in_list_of_dict(response, "plan")
        _list_plan = []
        for pl_iri in _list_plan_iri:
            _plan_info = dal.get_sublist_in_list_of_dict_matching_key_value(response, 'plan', pl_iri)
            _plan_info = dal.remove_duplicate_dict_from_list_of_dict(_plan_info)

            # process step
            _list_step_iri = dal.get_unique_values_in_list_of_dict(_plan_info, "step")
            _list_step = []
            for _step_iri in _list_step_iri:
                _step_info = dal.get_sublist_in_list_of_dict_matching_key_value(_plan_info, 'step', _step_iri)
                _step_info = dal.remove_duplicate_dict_from_list_of_dict(_step_info)

                step_instance = Step(
                    instance_iri=_step_iri,
                    clz=dal.get_the_unique_value_in_list_of_dict(_step_info, 'stepType'),
                    canBePerformedBy=dal.get_unique_values_in_list_of_dict(_step_info, 'agent'),
                    # NOTE hasNextStep is processed after all steps are created
                )
                _list_step.append(step_instance)
                dict_step[_step_iri] = step_instance
                dict_next_steps[_step_iri] = dal.get_unique_values_in_list_of_dict(_step_info, 'nextStep')

            # NOTE here we process the next steps
            for step in _list_step:
                _temp_step_list = [dict_step[next_step_iri] for next_step_iri in dict_next_steps[step.instance_iri]]
                step.hasNextStep = _temp_step_list if len(_temp_step_list) > 0 else None

            _plan = Plan(
                instance_iri=pl_iri,
                hasStep=_list_step,
            )
            _list_plan.append(_plan)

        return _list_plan

    # TODO add unit test
    def check_if_rogi_complete_one_iter(self, rogi_derivation: str) -> bool:
        # NOTE here we simplify the check by assuming that one rogi iter is done if has no status
        # TODO [next iteration] improve the check by checking the status of each step in the plan
        return not self.check_if_triple_exist(rogi_derivation, ONTODERIVATION_HASSTATUS, None)

    # TODO add unit test
    def get_goal_set_instance_from_rogi_derivation(self, rogi_derivation: str) -> GoalSet:
        query = f"""SELECT ?goal_set WHERE {{
            <{rogi_derivation}> <{ONTODERIVATION_ISDERIVEDFROM}> ?goal_set.
            ?goal_set a <{ONTOGOAL_GOALSET}>.
        }}"""
        response = self.performQuery(query)
        return self.get_goal_set_instance(goal_set_iri=response[0]['goal_set'])

    # TODO add unit test
    def get_rogi_derivations_of_goal_set(self, goal_set_iri: str, rogi_agent_iri: str) -> List[str]:
        goal_set_iri = trimIRI(goal_set_iri)
        rogi_agent_iri = trimIRI(rogi_agent_iri)
        query = f"""
            SELECT ?rogi_derivation
            WHERE {{
                ?rogi_derivation <{ONTODERIVATION_ISDERIVEDFROM}> <{goal_set_iri}>; <{ONTODERIVATION_ISDERIVEDUSING}> <{rogi_agent_iri}>.
            }}"""
        response = self.performQuery(query)
        return dal.get_unique_values_in_list_of_dict(response, 'rogi_derivation')

    # TODO add unit test
    def get_latest_rxn_exp_of_rogi_derivation(self, rogi_derivation: str):
        rogi_derivation = trimIRI(rogi_derivation)
        query = f"""
            SELECT DISTINCT ?rxn
            WHERE {{
                ?result <{ONTODERIVATION_BELONGSTO}> <{rogi_derivation}>;
                        <{ONTOGOAL_REFERSTO}>/^<{ONTOREACTION_HASPERFORMANCEINDICATOR}> ?rxn.
            }}"""
        response = self.performQuery(query)
        if len(response) == 0:
            return None
        elif len(response) > 1:
            raise Exception(f"More than one rxn found as the latest experiment for rogi derivation {rogi_derivation}: {response}")
        else:
            return response[0]['rxn']

    # TODO add unit test
    def get_all_rxn_exp_of_goal_set(self, goal_set_iri: str):
        goal_set_iri = trimIRI(goal_set_iri)
        query = f"""
            SELECT DISTINCT ?rxn
            WHERE {{
                <{goal_set_iri}> <{ONTOGOAL_HASGOAL}> ?goal.
                ?goal <{ONTOGOAL_HASRESULT}>/^<{ONTOREACTION_HASPERFORMANCEINDICATOR}> ?rxn.
            }}"""
        response = self.performQuery(query)
        return dal.get_unique_values_in_list_of_dict(response, 'rxn')

    # TODO add unit test
    def get_all_existing_goal_set(self):
        query = f"""SELECT DISTINCT ?goal_set WHERE {{ ?goal_set a <{ONTOGOAL_GOALSET}>. }}"""
        response = self.performQuery(query)
        return [list(x.values())[0] for x in response]

    # TODO add unit test
    def update_goal_set_restrictions(
        self,
        goal_set_iri: str,
        cycle_allowance: int=None,
        deadline: float=None
    ):
        _delete = ""
        _insert = ""
        _where = f"""
            WHERE {{
                <{goal_set_iri}> <{ONTOGOAL_HASRESTRICTION}> ?restriction.
                ?restriction <{ONTOGOAL_CYCLEALLOWANCE}> ?cycle_allowance.
                ?restriction <{ONTOGOAL_DEADLINE}> ?deadline.
            }}"""
        if cycle_allowance is not None:
            _delete += f"?restriction <{ONTOGOAL_CYCLEALLOWANCE}> ?cycle_allowance."
            _insert += f"?restriction <{ONTOGOAL_CYCLEALLOWANCE}> {cycle_allowance}."
        if deadline is not None:
            _delete += f"?restriction <{ONTOGOAL_DEADLINE}> ?deadline."
            _insert += f"""?restriction <{ONTOGOAL_DEADLINE}> "{datetime.fromtimestamp(deadline)}"^^<{XSD.dateTime.toPython()}>."""
        update = f"""DELETE {{ {_delete} }} INSERT {{ {_insert} }} {_where}"""
        self.performUpdate(update)
