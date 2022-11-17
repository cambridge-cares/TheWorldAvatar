import time

from pyderivationagent import DerivationAgent
from pyderivationagent import DerivationInputs
from pyderivationagent import DerivationOutputs

from rxnoptgoaliteragent.kg_operations import *
from rxnoptgoaliteragent.data_model import *


class RxnOptGoalIterAgent(DerivationAgent):
    def __init__(
        self,
        **kwargs
    ):
        super().__init__(**kwargs)

        self.sparql_client = self.get_sparql_client(RxnOptGoalIterSparqlClient)

    def agent_input_concepts(self) -> list:
        return [ONTOGOAL_GOALSET, ONTOCAPE_CHEMICALREACTION, ONTOREACTION_REACTIONEXPERIMENT, ONTOLAB_LABORATORY]

    def agent_output_concepts(self) -> list:
        return [ONTOGOAL_RESULT]

    def validate_inputs(self, http_request) -> bool:
        return super().validate_inputs(http_request)

    def process_request_parameters(self, derivation_inputs: DerivationInputs, derivation_outputs: DerivationOutputs):
        # I. Get the goal set
        list_goal_set_iri = derivation_inputs.getIris(ONTOGOAL_GOALSET)
        if len(list_goal_set_iri) != 1:
            raise Exception(f"Exactly one goal set is expected, but found: {list_goal_set_iri}")
        goal_set_iri = list_goal_set_iri[0]
        goal_set_instance = self.sparql_client.get_goal_set_instance(goal_set_iri)

        # II. Get the chemical reaction and reaction experiment
        # NOTE reaction experiment might not in the derivation inputs as this might be the first reaction where no prior data is available
        # Check if the input is in correct format, and return OntoReaction.ReactionExperiment/ReactionVariation instance
        list_chemical_reaction_iri = derivation_inputs.getIris(ONTOCAPE_CHEMICALREACTION)
        if len(list_chemical_reaction_iri) != 1:
            raise Exception(f"Exactly one chemical reaction is expected, but found: {list_chemical_reaction_iri}")
        chem_rxn_iri = list_chemical_reaction_iri[0]
        chem_rxn_instance = self.sparql_client.get_chemical_reaction_given_iri(chem_rxn_iri)
        if chem_rxn_instance.hasDoETemplate is None:
            raise Exception(f"ChemicalReaction {chem_rxn_iri} does not have a DoE template")

        _full_derivation_inputs = derivation_inputs.getInputs()
        list_rxn_exp_instance = None
        if ONTOREACTION_REACTIONEXPERIMENT in _full_derivation_inputs:
            list_rxn_exp_instance = self.sparql_client.getReactionExperiment(derivation_inputs.getIris(ONTOREACTION_REACTIONEXPERIMENT))

        # III. Get the laboratory
        # If not provied as input, then the vapourtec schedule agent will assume all laboratories in the knowledge graph are available
        list_laboratory_iri = derivation_inputs.getIris(ONTOLAB_LABORATORY) if ONTOLAB_LABORATORY in derivation_inputs.getInputs() else []

        # IV. Create a set of derivations
        # Create and upload the DesignOfExperiment triples to triple store as pure input
        # NOTE the timestamp will be added automatically when marking up the derivations
        # NOTE not all list_rxn_exp_instance passed in will be used, only those have reaction conditions within the range will be used
        doe_instance = self.sparql_client.generate_doe_instance_from_goal(
            goal_set=goal_set_instance,
            chem_rxn=chem_rxn_instance,
            rxn_exp_as_beliefs=list_rxn_exp_instance,
        )
        g = Graph()
        g = doe_instance.create_instance_for_kg(g)
        self.sparql_client.uploadGraph(g)

        # Get plan and steps
        # TODO: [next iteration] implement a more generic way of processing plan and step, here we took a shortcut that only works for all goals have the same plan and step
        plan = goal_set_instance.hasGoal[0].hasPlan[0]
        doe_step = plan.get_step(ONTOGOAL_DESIGNOFEXPERIMENT)
        exe_step = plan.get_step(ONTOGOAL_RXNEXPEXECUTION)
        postpro_step = plan.get_step(ONTOGOAL_POSTPROCESSING)

        # Create derivation instance for new information, the timestamp of this derivation is 0
        # TODO: [next iteration] implement a more generic way of deciding the agent to perform the derivation, here we took a shortcut to use the first agent (Step.canBePerformedBy[0])
        doe_derivation_iri = self.derivation_client.createAsyncDerivationForNewInfo(doe_step.canBePerformedBy[0], [doe_instance.instance_iri])
        self.logger.info(f"Initialised successfully, created asynchronous doe derivation instance: {doe_derivation_iri}")
        exe_derivation_iri = self.derivation_client.createAsyncDerivationForNewInfo(exe_step.canBePerformedBy[0], [doe_derivation_iri] + list_laboratory_iri)
        self.logger.info(f"Initialised successfully, created asynchronous exe derivation instance: {exe_derivation_iri}")
        postpro_derivation_iri = self.derivation_client.createAsyncDerivationForNewInfo(postpro_step.canBePerformedBy[0], [exe_derivation_iri])
        self.logger.info(f"Initialised successfully, created asynchronous postproc derivation instance: {postpro_derivation_iri}")

        # V. Create derivation outputs after experiment is finished
        # Monitor the status of the postpro_derivation_iri, until it produced outputs
        interested_performance_indicators = [goal.desires().clz for goal in goal_set_instance.hasGoal]
        new_rxn_exp = self.sparql_client.detect_postpro_derivation_result(postpro_derivation_iri, interested_performance_indicators)
        while not new_rxn_exp:
            time.sleep(30)
            try:
                new_rxn_exp = self.sparql_client.detect_postpro_derivation_result(postpro_derivation_iri, interested_performance_indicators)
            except Exception as e:
                self.logger.error(f"Error in detecting postpro derivation result: {e}")
                new_rxn_exp = None

        # Add the Result instances to the derivation_outputs
        dct_desires_iri_clz = {goal.desires().instance_iri:goal.desires().clz for goal in goal_set_instance.hasGoal}
        list_results = []
        for desire_iri in dct_desires_iri_clz:
            result_iri = initialiseInstanceIRI(getNameSpace(goal_set_iri), ONTOGOAL_RESULT)
            derivation_outputs.createNewEntity(result_iri, ONTOGOAL_RESULT)
            _goal = goal_set_instance.get_goal_given_desired_quantity(desire_iri).instance_iri
            _quantity = new_rxn_exp.get_performance_indicator(dct_desires_iri_clz[desire_iri], None).instance_iri
            derivation_outputs.addTriple(_goal, ONTOGOAL_HASRESULT, _quantity)
            derivation_outputs.addTriple(result_iri, ONTOGOAL_REFERSTO, _quantity)
            list_results.append(result_iri)

        self.logger.info(f"The generated Result for GoalSet <{goal_set_iri}> can be identified as: {list_results}")


# Show an instructional message at the RxnOptGoalIterAgent servlet root
def default():
    """
        Instructional message at the app root.
    """
    msg = "This is an asynchronous agent that capable of conducting one iteration of pursuring the reaction optimisation goal.<BR>"
    msg += "For more information, please visit https://github.com/cambridge-cares/TheWorldAvatar/tree/main/Agents/RxnOptGoalIterAgent#readme<BR>"
    return msg
