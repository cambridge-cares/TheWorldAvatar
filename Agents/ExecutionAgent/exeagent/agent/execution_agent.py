from dataclasses import asdict
from pathlib import Path
from typing import List
import json
import time
import os

from pyasyncagent import AsyncAgent

from chemistry_and_robots.hardware import vapourtec

from exeagent.kg_operations import *
from exeagent.data_model import *
from exeagent.conf import *

class ExecutionAgent(AsyncAgent):
    def setupJob(self, agentInputs) -> list:
        """
            This function sets up the job given the agent inputs retrieved from ExpSetup Derivation.
            Arguments:
                agentInputs - derivation inputs JSON string (in the format of python dict)
        """

        # Create sparql_client
        self.sparql_client = ChemistryAndRobotsSparqlClient(
            self.kgUrl, self.kgUrl
        )
        # Check if the input is in correct format, and return OntoRxn.ReactionExperiment/ReactionVariation instance
        rxn_exp_instance = self.collectInputsInformation(agentInputs)
        self.logger.info("Collected inputs from the knowledge graph: ")
        self.logger.info(json.dumps(rxn_exp_instance.dict()))

        # Check until it's the turn for the given reaction experiment
        rxn_exp_queue = self.sparql_client.get_prior_rxn_exp_in_queue(rxn_exp_instance.instance_iri)
        while len(rxn_exp_queue) > 0:
            time.sleep(60)
            rxn_exp_queue = self.sparql_client.get_prior_rxn_exp_in_queue(rxn_exp_instance.instance_iri)

        # Check until get the digital twin of the most suitable hardware
        # yes, done TODO do we return the whole instance of vapourtec_rs400 here? which will be used for
        preferred_rs400, preferred_r4_reactor = self.sparql_client.get_preferred_vapourtec_rs400(rxn_exp_instance)
        while (preferred_rs400, preferred_r4_reactor) == (None, None):
            time.sleep(60)
            preferred_rs400, preferred_r4_reactor = self.sparql_client.get_preferred_vapourtec_rs400(rxn_exp_instance)

        # Call function to create a list of ontolab.EquipmentSettings instances from ontorxn.ReactionExperiment/ReactionVariation and ontovapourtec.VapourtecRS400 and VapourtecR4Reactor instances
        # yes, done TODO do we pass the instance of vapourtecrs400 and reaction experiment both as arguments?
        list_equip_settings = self.sparql_client.create_equip_settings_for_rs400_from_rxn_exp(rxn_exp_instance, preferred_rs400, preferred_r4_reactor)

        # Upload the create OntoLab:EquipmentSettings triples to KG
        # yes, done TODO do we need to connect the equipment settings to the lab equipment here?
        self.sparql_client.write_equip_settings_to_kg(list_equip_settings) # TODO this function should be changed, do we need to write it to the kg?
        self.logger.info(f"The proposed new equipment settings are recorded in: <{'>, <'.join([es.instance_iri for es in list_equip_settings])}>.")

        # Generate the execution CSV file and send for execution
        # yes, done? TODO pass an instance of rxn exp and list of equipment settings
        run_csv_path = vapourtec.create_exp_run_csv(rxn_exp_instance, list_equip_settings)
        # TODO this file will be uploaded to KG file server - to be downloaded by specific agent deployed together with hardware
        self.logger.info(f"The generated CSV run file path is {run_csv_path}")
        # vapourtec.connect_to_fc() # TODO
        # vapourtec.send_exp_csv_for_exe() # TODO
        return []

        # Check until a new HPLC report is generated
        # hplc_report = self.sparql_client.get_hplc_report_given_ # TODO
        # self.logger.info(f"The generated new HPLC report (raw data) is hosted at: <{hplc_report.instance_iri}>.")
        # TODO make the connection between HPLCReport and HPLCJob here?
        # TODO where do we generate the instance of HPLCJob?
        # TODO query the HPLC instance so that to make the link between the HPLCReport and HPLCJob hence ReactionExperiment?
        # <hplc> <hasJob> <hplc_job>
        # <hplc_job> <characterises> <rxnexp>
        # <hplc_job> <usesMethod> <hplc_method> # these hplc_method should pre-exist, we only need to attach to it # maybe also can check what HPLCMethod was used for previous rxnexp?
        # <hplc_job> <hasReport> <hplc_report>
        # <hplc_report> <generatedFor> <chemical_solution>
        # <chemical_solution> <fills> <vial> # here we should write the vial location to the KG
        # <vial> <hasFillLevel> <xxx> # (the rest information about vial should already be known as part of digital twin of autosampler)
        # TODO do we remove the settings for the hardware from the digital twin? --> so within each operation of execution agent, the settings got generated and deleted

        # return [hplc_report.instance_iri]

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
                list_rxn_exp_instance = self.sparql_client.getReactionExperiment(agent_inputs[ONTORXN_REACTIONEXPERIMENT])
                if len(list_rxn_exp_instance) > 1:
                    raise Exception(
                        "Only one instance of OntoRxn:ReactionExperiment should be used for generating OntoLab:EquipmentSettings per ExpSetup Derivation, collected: <%s>" % (">, <".join([rxnexp.instance_iri for rxnexp in list_rxn_exp_instance]))
                    )
            except ValueError:
                self.logger.error("Unable to interpret reaction experiment ('%s') as an IRI." % agent_inputs[ONTORXN_REACTIONEXPERIMENT])
                raise Exception("Unable to interpret reaction experiment ('%s') as an IRI." % agent_inputs[ONTORXN_REACTIONEXPERIMENT])
        else:
            self.logger.error('OntoRxn:ReactionExperiment instance might be missing. Received inputs: ' + str(agent_inputs) + exception_string)
            raise Exception('OntoRxn:ReactionExperiment instance might be missing. Received inputs: ' + str(agent_inputs) + exception_string)

        return list_rxn_exp_instance[0]

# Show an instructional message at the ExecutionAgent servlet root
def default():
    """
        Instructional message at the app root.
    """
    msg  = "This is an asynchronous agent that capable of executing experiment in lab equipment.<BR>"
    msg += "For more information, please visit https://github.com/cambridge-cares/TheWorldAvatar/tree/134-dev-lab-equipment-digital-twin/Agents/ExecutionAgent#readme<BR>"
    # TODO change above line to https://github.com/cambridge-cares/TheWorldAvatar/blob/develop/Agents/ExecutionAgent#readme, before merging back to develop branch
    return msg
