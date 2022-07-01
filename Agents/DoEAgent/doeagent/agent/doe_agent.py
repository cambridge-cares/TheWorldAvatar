from typing import List
import json

from pyderivationagent import DerivationAgent
from pyderivationagent import DerivationInputs
from pyderivationagent import DerivationOutputs

from doeagent.kg_operations import *
from doeagent.data_model import *
from doeagent.doe_algo import *


class DoEAgent(DerivationAgent):
    # TODO consider making __init__ of DerivationAgent to accept **kwargs
    def __init__(self,
        register_agent: bool=True,
        **kwargs
    ):
        super().__init__(**kwargs)
        self.register_agent = register_agent

        # Initialise the sparql_client
        self.sparql_client = ChemistryAndRobotsSparqlClient(
            self.kgUrl, self.kgUrl, kg_user=self.kgUser, kg_password=self.kgPassword
        )

    def register(self):
        # TODO think about standardised way of specify if to register?
        if self.register_agent:
            try:
                self.sparql_client.generate_ontoagent_instance(
                    self.agentIRI,
                    self.agentEndpoint,
                    [ONTODOE_DESIGNOFEXPERIMENT],
                    [ONTOREACTION_REACTIONEXPERIMENT]
                )
            except Exception as e:
                self.logger.error(e, stack_info=True, exc_info=True)
                raise Exception("Agent <%s> registration failed." % self.agentIRI)

    def process_request_parameters(self, derivation_inputs: DerivationInputs, derivation_outputs: DerivationOutputs):
        # Check if the input is in correct format, and return OntoDoE.DesignOfExperiment instance
        try:
            doe_instance = self.sparql_client.get_doe_instance(derivation_inputs.getIris(ONTODOE_DESIGNOFEXPERIMENT)[0])
        except Exception as e:
            self.logger.error(e)
        self.logger.info("Collected inputs from the knowledge graph: ")
        self.logger.info(json.dumps(doe_instance.dict()))

        # Call function to suggest the new experiment and return an instance of dataclass OntoDoE.NewExperiment
        new_rxn_exp = suggest(doe_instance)

        # Upload the created OntoRxn:ReactionVariation triples to KG
        # Also update the triple between OntoDoE:DesignOfExperiment and OntoRxn:ReactionVariation
        g = self.sparql_client.collect_triples_for_new_experiment(doe_instance, new_rxn_exp)

        # Add the whole graph to output triples
        derivation_outputs.addGraph(g)


def suggest(doe_instance: DesignOfExperiment) -> List[ReactionExperiment]:
    """
        This method suggests the new experiment given information provided for design of experiment exercise.

        Arguments:
            doe_instance - instance of dataclass OntoDoE.DesignOfExperiment
    """

    # TODO this method calls summit doe, can be expanded in the future
    new_exp = proposeNewExperiment(doe_instance)

    return new_exp


# Show an instructional message at the DoEAgent servlet root
def default():
    """
        Instructional message at the app root.
    """
    msg  = "This is an asynchronous agent that capable of conducting Design Of Experiment (DoE).<BR>"
    msg += "For more information, please visit https://github.com/cambridge-cares/TheWorldAvatar/tree/133-dev-design-of-experiment/Agents/DoEAgent#readme<BR>"
    # TODO change above line to https://github.com/cambridge-cares/TheWorldAvatar/blob/develop/Agents/DoEAgent#readme, before merging back to develop branch
    return msg
