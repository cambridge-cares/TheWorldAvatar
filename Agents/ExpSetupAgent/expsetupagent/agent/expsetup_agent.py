from dataclasses import asdict
from pathlib import Path
from typing import List
import json
import os

from pyasyncagent import AsyncAgent

from expsetupagent.kg_operations import *
from expsetupagent.conf import *

from doeagent.agent import *

class ExpSetupAgent(AsyncAgent):
    def setupJob(self, agentInputs) -> list:
        """
            This function sets up the job given the agent inputs retrieved from ExpSetup Derivation.
            Arguments:
                agentInputs - derivation inputs JSON string (in the format of python dict)
        """

        # Create sparql_client
        self.sparql_client = ExpSetupSparqlClient(
            self.kgUrl, self.kgUrl
        )
        # Check if the input is in correct format, and return OntoRxn.ReactionExperiment/ReactionVariation instance
        rxn_exp_instance = self.collectInputsInformation(agentInputs)
        self.logger.info("Collected inputs from the knowledge graph: ")
        self.logger.info(json.dumps(asdict(rxn_exp_instance)))

        # Call function to create a list of OntoLab.EquipmentSettings instances from OntoRxn:ReactionExperiment/ReactionVariation
        list_equip_settings = self.sparql_client.createEquipmentSettingsFromReactionExperiment(rxn_exp_instance)

        # Upload the created OntoLab:EquipmentSettings triples to KG
        self.sparql_client.writeEquipmentSettingsToKG(list_equip_settings)

        list_equip_settings_iri = [es.instance_iri for es in list_equip_settings]
        self.logger.info(f"The proposed new equipment settings are recorded in: <{'>, <'.join(list_equip_settings_iri)}>.")
        return list_equip_settings_iri

    def collectInputsInformation(self, agent_inputs) -> ReactionExperiment:
        """
            This function checks the agent input against the I/O signiture as declared in the ExpSetup Agent OntoAgent instance and collects information.
        """
        self.logger.info("Checking arguments...")
        exception_string = """Inputs are not provided in correct form. An example is: 
                                {
                                    "https://github.com/cambridge-cares/TheWorldAvatar/blob/develop/JPS_Ontology/ontology/ontodoe/OntoRxn.owl#ReactionExperiment": "https://www.example.com/triplestore/ontorxn/Rxn_1/ReactionExperiment_1",
                                }"""
        # If the input JSON string is missing mandatory keys, raise error with "exception_string"
        if ONTORXN_REACTIONEXPERIMENT in agent_inputs:
            try:
                # Get the information from OntoRxn:ReactionExperiment instance
                doe_sparql_client = DoESparqlClient(self.kgUrl, self.kgUrl)
                list_rxn_exp_instance = doe_sparql_client.getReactionExperiment(agent_inputs[ONTORXN_REACTIONEXPERIMENT])
                if len(list_rxn_exp_instance) > 1:
                    raise Exception(
                        "Only one instance of OntoRxn:ReactionExperiment should be used for generating OntoLab:EquipmentSettings per ExpSetup Derivation, collected: <%s>" % (">, <".join([rxnexp.instance_iri for rxnexp in list_rxn_exp_instance]))
                    )
            except ValueError:
                self.logger.error("Unable to interpret strategy ('%s') as an IRI." % agent_inputs[ONTORXN_REACTIONEXPERIMENT])
                raise Exception("Unable to interpret strategy ('%s') as an IRI." % agent_inputs[ONTORXN_REACTIONEXPERIMENT])
        else:
            self.logger.error('OntoRxn:ReactionExperiment instance might be missing. Received inputs: ' + str(agent_inputs) + exception_string)
            raise Exception('OntoRxn:ReactionExperiment instance might be missing. Received inputs: ' + str(agent_inputs) + exception_string)

        return list_rxn_exp_instance[0]

# Show an instructional message at the DoEAgent servlet root
def default():
    """
        Instructional message at the app root.
    """
    msg  = "This is an asynchronous agent that capable of setting up experiment to lab equipment digital twin.<BR>"
    msg += "For more information, please visit https://github.com/cambridge-cares/TheWorldAvatar/tree/138-dev-exp-setup-agent/Agents/ExpSetupAgent#readme<BR>"
    # TODO change above line to https://github.com/cambridge-cares/TheWorldAvatar/blob/develop/Agents/ExpSetupAgent#readme, before merging back to develop branch
    return msg
