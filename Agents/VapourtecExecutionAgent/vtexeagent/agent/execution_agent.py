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
        maximum_concurrent_experiment: int = 60,
        register_agent: bool = True,
        **kwargs
    ):
        super().__init__(**kwargs)
        self.maximum_concurrent_experiment = maximum_concurrent_experiment
        self.register_agent = register_agent

        self.sparql_client = ChemistryAndRobotsSparqlClient(
            self.kgUrl, self.kgUpdateUrl, self.kgUser, self.kgPassword,
            self.fs_url, self.fs_user, self.fs_password
        )

    def register(self):
        if self.register_agent:
            try:
                self.sparql_client.generate_ontoagent_instance(
                    self.agentIRI,
                    self.agentEndpoint,
                    [ONTOREACTION_REACTIONEXPERIMENT],
                    [ONTOHPLC_HPLCREPORT]
                )
            except Exception as e:
                self.logger.error(e, stack_info=True, exc_info=True)
                raise Exception("Agent <%s> registration failed." % self.agentIRI)

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
        rxn_exp_queue = self.sparql_client.get_prior_rxn_exp_in_queue(
            rxn_exp_instance.instance_iri)
        # TODO NOTE here the maximum_concurrent_experiment is configured at agent start, move to KG in the future iterations
        while len(rxn_exp_queue) > self.maximum_concurrent_experiment - 1:
            time.sleep(60)
            rxn_exp_queue = self.sparql_client.get_prior_rxn_exp_in_queue(
                rxn_exp_instance.instance_iri)

        # Check until get the digital twin of the most suitable hardware
        preferred_rs400, preferred_r4_reactor = self.sparql_client.get_preferred_vapourtec_rs400(
            rxn_exp_instance)
        while (preferred_rs400, preferred_r4_reactor) == (None, None):
            time.sleep(60)
            preferred_rs400, preferred_r4_reactor = self.sparql_client.get_preferred_vapourtec_rs400(
                rxn_exp_instance)

        # Once selected the suitable digital twin, assign experiment to the reactor
        self.sparql_client.assign_rxn_exp_to_r4_reactor(
            rxn_exp_iri=rxn_exp_instance.instance_iri,
            r4_reactor_iri=preferred_r4_reactor.instance_iri
        )

        # Locate the digital twin of HPLC connected to the vapourtec_rs400
        agilent_hplc = self.sparql_client.get_hplc_given_vapourtec_rs400(preferred_rs400.instance_iri)

        # Now create vapourtec derivation and agilent derivation for the reaction experiment
        if preferred_rs400.isManagedBy is not None and agilent_hplc.isManagedBy is not None:
            vapourtec_derivation_iri = self.derivationClient.createAsyncDerivationForNewInfo(
                preferred_rs400.isManagedBy, [rxn_exp_instance.instance_iri])
            agilent_derivation_iri = self.derivationClient.createAsyncDerivationForNewInfo(
                agilent_hplc.isManagedBy, [rxn_exp_instance.instance_iri, vapourtec_derivation_iri])

            # Monitor the status of the agilent_derivation_iri, until it produced outputs
            new_hplc_report = self.sparql_client.detect_new_hplc_report_from_agilent_derivation(agilent_derivation_iri)
            while not new_hplc_report:
                time.sleep(60)
                new_hplc_report = self.sparql_client.detect_new_hplc_report_from_agilent_derivation(agilent_derivation_iri)

            # Add the HPLCReport instance to the derivation_outputs
            derivation_outputs.addTriple(new_hplc_report, RDF.type.toPython(), ONTOHPLC_HPLCREPORT)
            self.logger.info(f"The generated new HPLC report (raw data) for ReactionExperiment <{rxn_exp_instance.instance_iri}> can be identified as: <{new_hplc_report}>.")

        else:
            # TODO add support in situation where digital twin are not managed by agents
            raise NotImplementedError("Not yet supported - the preferred digital twin of the hardware <%s> and <%s> are not managed by agent instances." % (
                preferred_rs400.instance_iri, agilent_hplc.instance_iri))


# Show an instructional message at the ExecutionAgent servlet root
def default():
    """
        Instructional message at the app root.
    """
    msg = "This is an asynchronous agent that capable of executing experiment in lab equipment.<BR>"
    msg += "For more information, please visit https://github.com/cambridge-cares/TheWorldAvatar/tree/134-dev-lab-equipment-digital-twin/Agents/ExecutionAgent#readme<BR>"
    # TODO change above line to https://github.com/cambridge-cares/TheWorldAvatar/blob/develop/Agents/ExecutionAgent#readme, before merging back to develop branch
    return msg
