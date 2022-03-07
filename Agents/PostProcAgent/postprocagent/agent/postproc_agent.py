from pathlib import Path
from typing import List
import json
import time
import os

from pyasyncagent import AsyncAgent

from postprocagent.kg_operations import *
from postprocagent.data_model import *
from postprocagent.conf import *

from chemistry_and_robots.hardware import hplc

class PostProcAgent(AsyncAgent):
    def setupJob(self, agentInputs) -> list:
        # Create sparql_client
        self.sparql_client = ChemistryAndRobotsSparqlClient(
            self.kgUrl, self.kgUrl
        )
        # Check if the input is in correct format, and return ontohplc.HPLCReport instance
        hplc_report_instance = self.collectInputsInformation(agentInputs)
        self.logger.info("Collected inputs from the knowledge graph: ")
        self.logger.info(json.dumps(hplc_report_instance.dict()))

        # calculate yield
        
        return 

    def collectInputsInformation(self, agent_inputs) -> HPLCReport:
        """
            This function checks the agent input against the I/O signature as declared in the PostProc Agent OntoAgent instance and collects information.
        """
        self.logger.info("Checking arguments...")
        exception_string = """Inputs are not provided in correct form. An example is: 
                                {
                                    "https://github.com/cambridge-cares/TheWorldAvatar/blob/develop/JPS_Ontology/ontology/ontohplc/OntoHPLC.owl#HPLCReport": "https://www.example.com/triplestore/ontohplc/HPLC_1/HPLCReport_1",
                                }"""
        # If the input JSON string is missing mandatory keys, raise error with "exception_string"
        if ONTOHPLC_HPLCREPORT in agent_inputs:
            try:
                # Get the information from OntoHPLC:HPLCReport instance
                hplc_report_instance = self.sparql_client.process_raw_hplc_report(agent_inputs[ONTOHPLC_HPLCREPORT])
            except ValueError:
                self.logger.error("Unable to interpret hplc report ('%s') as an IRI." % agent_inputs[ONTOHPLC_HPLCREPORT])
                raise Exception("Unable to interpret hplc report ('%s') as an IRI." % agent_inputs[ONTOHPLC_HPLCREPORT])
        else:
            self.logger.error('OntoHPLC:HPLCReport instance might be missing. Received inputs: ' + str(agent_inputs) + exception_string)
            raise Exception('OntoHPLC:HPLCReport instance might be missing. Received inputs: ' + str(agent_inputs) + exception_string)

        return hplc_report_instance

def default():
    """
        Instructional message at the app root.
    """
    msg  = "This is an asynchronous agent that capable of post-processing experiment raw data generated from lab equipment.<BR>"
    msg += "For more information, please visit https://github.com/cambridge-cares/TheWorldAvatar/tree/134-dev-lab-equipment-digital-twin/Agents/PostProcAgent#readme<BR>"
    # TODO change above line to https://github.com/cambridge-cares/TheWorldAvatar/blob/develop/Agents/PostProcAgent#readme, before merging back to develop branch
    return msg
