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
        return [ONTOGOAL_GOALSET, ONTOREACTION_REACTIONEXPERIMENT]

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


        # II. Get the reaction experiment
        # Check if the input is in correct format, and return OntoReaction.ReactionExperiment/ReactionVariation instance
        list_rxn_exp_instance = self.sparql_client.getReactionExperiment(derivation_inputs.getIris(ONTOREACTION_REACTIONEXPERIMENT))


        # III. Create a set of derivations
        # Create and upload the DesignOfExperiment triples to triple store as pure input, and add timestamp
        doe_instance = self.sparql_client.generate_doe_instance_from_goal(goal_set_instance, list_rxn_exp_instance)
        g = Graph()
        g = doe_instance.create_instance_for_kg(g)
        self.sparql_client.uploadGraph(g)
        self.derivationClient.addTimeInstance(doe_instance.instance_iri)
        self.derivationClient.updateTimestamp(doe_instance.instance_iri)

        # Get plan and steps
        # TODO: implement a more generic way of processing plan and step, here we took a shortcut that only works for all goals have the same plan and step
        plan = goal_set_instance.hasGoal[0].hasPlan[0]
        doe_step = plan.get_step(ONTOGOAL_DESIGNOFEXPERIMENT)
        exe_step = plan.get_step(ONTOGOAL_RXNEXPEXECUTION)
        postpro_step = plan.get_step(ONTOGOAL_POSTPROCESSING)

        # Create derivation instance for new information, the timestamp of this derivation is 0
        doe_derivation_iri = self.derivationClient.createAsyncDerivationForNewInfo(doe_step.canBePerformedBy, doe_instance.instance_iri)
        self.logger.info(f"Initialised successfully, created asynchronous doe derivation instance: {doe_derivation_iri}")
        exe_derivation_iri = self.derivationClient.createAsyncDerivationForNewInfo(exe_step.canBePerformedBy, [doe_derivation_iri])
        self.logger.info(f"Initialised successfully, created asynchronous exe derivation instance: {exe_derivation_iri}")
        postpro_derivation_iri = self.derivationClient.createAsyncDerivationForNewInfo(postpro_step.canBePerformedBy, [exe_derivation_iri])
        self.logger.info(f"Initialised successfully, created asynchronous postproc derivation instance: {postpro_derivation_iri}")


        # IV. Create derivation outputs after experiment is finished
        # Monitor the status of the postpro_derivation_iri, until it produced outputs
        new_rxn_exp = self.sparql_client.detect_postpro_derivation_result(postpro_derivation_iri)
        while not new_rxn_exp:
            time.sleep(60)
            new_rxn_exp = self.sparql_client.detect_postpro_derivation_result(postpro_derivation_iri)

        # Add the Result instances to the derivation_outputs
        list_desires = [goal.desires().instance_iri for goal in goal_set_instance.hasGoal]
        list_results = []
        for desire in list_desires:
            result_iri = initialiseInstanceIRI(getNameSpace(goal_set_iri), ONTOGOAL_RESULT)
            derivation_outputs.createNewEntity(result_iri, ONTOGOAL_RESULT)
            _goal = goal_set_instance.get_goal_given_desired_quantity(desire).instance_iri
            _quantity = new_rxn_exp.get_performance_indicator(desire, None).instance_iri
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
