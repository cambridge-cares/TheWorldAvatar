import json
import time

from pyderivationagent import DerivationAgent
from pyderivationagent import DerivationInputs
from pyderivationagent import DerivationOutputs

from vtexeagent.kg_operations import *
from vtexeagent.data_model import *


class VapourtecExecutionAgent(DerivationAgent):
    def __init__(
        self,
        maximum_concurrent_experiment: int = 1,
        **kwargs
    ):
        super().__init__(**kwargs)
        self.maximum_concurrent_experiment = maximum_concurrent_experiment

        self.sparql_client = self.get_sparql_client(ChemistryAndRobotsSparqlClient)

    def agent_input_concepts(self) -> list:
        return [ONTOREACTION_REACTIONEXPERIMENT]

    def agent_output_concepts(self) -> list:
        return [ONTOHPLC_HPLCREPORT]

    def validate_inputs(self, http_request) -> bool:
        return super().validate_inputs(http_request)

    def process_request_parameters(self, derivation_inputs: DerivationInputs, derivation_outputs: DerivationOutputs):
        # Check if the input is in correct format, and return OntoReaction.ReactionExperiment/ReactionVariation instance
        list_rxn_exp_instance = self.sparql_client.getReactionExperiment(
            derivation_inputs.getIris(ONTOREACTION_REACTIONEXPERIMENT))
        if len(list_rxn_exp_instance) > 1:
            raise Exception(
                "Only one instance of OntoReaction:ReactionExperiment should be used for generating OntoLab:EquipmentSettings per ExpSetup Derivation, collected: <%s>" % (
                    ">, <".join([rxnexp.instance_iri for rxnexp in list_rxn_exp_instance]))
            )

        rxn_exp_instance = list_rxn_exp_instance[0]
        self.logger.info("Collected inputs from the knowledge graph: ")
        self.logger.info(json.dumps(rxn_exp_instance.dict()))

        # Check until it's the turn for the given reaction experiment
        rxn_exp_queue = self.sparql_client.get_prior_rxn_exp_in_queue(rxn_exp_instance.instance_iri, self.agentIRI)
        self.logger.info("ReactionExperiment <%s> has prior experiment in queue: %s" % (rxn_exp_instance.instance_iri, str(rxn_exp_queue)))
        # TODO NOTE [when run in loop] here the maximum_concurrent_experiment is configured at agent start, move to KG in the future iterations
        while len(rxn_exp_queue) > self.maximum_concurrent_experiment - 1:
            time.sleep(60)
            rxn_exp_queue = self.sparql_client.get_prior_rxn_exp_in_queue(rxn_exp_instance.instance_iri, self.agentIRI)
            self.logger.info("ReactionExperiment <%s> has prior experiment in queue: %s" % (rxn_exp_instance.instance_iri, str(rxn_exp_queue)))

        # Check until get the digital twin of the most suitable hardware
        # This function also locates the digital twin of HPLC connected to the vapourtec_rs400
        preferred_rs400, preferred_r4_reactor, associated_hplc = self.sparql_client.get_preferred_vapourtec_rs400(
            rxn_exp_instance)
        while (preferred_rs400, preferred_r4_reactor, associated_hplc) == (None, None, None):
            time.sleep(60)
            preferred_rs400, preferred_r4_reactor, associated_hplc = self.sparql_client.get_preferred_vapourtec_rs400(
                rxn_exp_instance)

        # Once selected the suitable digital twin, assign experiment to the reactor
        self.sparql_client.assign_rxn_exp_to_r4_reactor(
            rxn_exp_iri=rxn_exp_instance.instance_iri,
            r4_reactor_iri=preferred_r4_reactor.instance_iri
        )

        # Now create vapourtec derivation and hplc derivation for the reaction experiment
        vapourtec_derivation_iri = self.derivationClient.createAsyncDerivationForNewInfo(
            preferred_rs400.isManagedBy, [rxn_exp_instance.instance_iri])
        hplc_derivation_iri = self.derivationClient.createAsyncDerivationForNewInfo(
            associated_hplc.isManagedBy, [rxn_exp_instance.instance_iri, vapourtec_derivation_iri])

        # Monitor the status of the hplc_derivation_iri, until it produced outputs
        new_hplc_report = self.sparql_client.detect_new_hplc_report_from_hplc_derivation(hplc_derivation_iri)
        while not new_hplc_report:
            time.sleep(60)
            new_hplc_report = self.sparql_client.detect_new_hplc_report_from_hplc_derivation(hplc_derivation_iri)

        # Add the HPLCReport instance to the derivation_outputs
        derivation_outputs.createNewEntity(new_hplc_report, ONTOHPLC_HPLCREPORT)
        self.logger.info(f"The generated new HPLC report (raw data) for ReactionExperiment <{rxn_exp_instance.instance_iri}> can be identified as: <{new_hplc_report}>.")


# Show an instructional message at the ExecutionAgent servlet root
def default():
    """
        Instructional message at the app root.
    """
    msg = "This is an asynchronous agent that capable of executing experiment in lab equipment.<BR>"
    msg += "For more information, please visit https://github.com/cambridge-cares/TheWorldAvatar/tree/main/Agents/VapourtecExecutionAgent#readme<BR>"
    return msg
