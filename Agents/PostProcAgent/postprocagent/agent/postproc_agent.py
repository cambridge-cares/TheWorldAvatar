from pathlib import Path
from typing import List
import json
import time
import os

from pyasyncagent import AsyncAgent

from postprocagent.kg_operations import *
from postprocagent.data_model import *
import postprocagent.hypo_rxn as hypo
from postprocagent.conf import *

from chemistry_and_robots.hardware import hplc

class PostProcAgent(AsyncAgent):
    def setupJob(self, agentInputs) -> list:
        # Create sparql_client
        self.sparql_client = ChemistryAndRobotsSparqlClient(
            self.kgUrl, self.kgUrl
        )
        # Get the HPLCReport iri from the agent inputs
        hplc_report_iri = self.collectInputsInformation(agentInputs)

        # Retrieve the ReactionExperiment instance that the is HPLCReport is generated for
        rxn_exp_instance = self.sparql_client.get_rxn_exp_associated_with_hplc_report(hplc_report_iri)
        # Retrieve the InternalStandard instance that was used by the HPLCMethod
        internal_standard_instance = self.sparql_client.get_internal_standard_associated_with_hplc_report(hplc_report_iri)

        # Construct an instance of HypoReactor given the ReactionExperiment information, get the value of internal_standard_run_conc_moleperlitre
        hypo_reactor, internal_standard_run_conc_moleperlitre = hypo.construct_hypo_reactor(self.sparql_client, rxn_exp_instance, internal_standard_instance)

        # TODO Process the raw hplc report and generate an instance of HPLCReport
        hplc_report_instance = self.sparql_client.process_raw_hplc_report(hplc_report_iri=hplc_report_iri,
            internal_standard_run_conc_moleperlitre=internal_standard_run_conc_moleperlitre)

        # TODO Construct an instance of HypoEndStream given the processed HPLCReport instance and instance of HypoReactor
        hypo_end_stream = hypo.construct_hypo_end_stream(self.sparql_client, hplc_report_instance, hypo_reactor)

        # TODO Calculate each PerformanceIndicator
        pi_yield = hypo.calculate_yield(hypo_reactor, hypo_end_stream)
        pi_conversion = hypo.calculate_conversion(hypo_reactor, hypo_end_stream)
        pi_eco_score = hypo.calculate_eco_score(hypo_reactor, hypo_end_stream)
        pi_e_factor = hypo.calculate_enviromental_factor(hypo_reactor, hypo_end_stream)
        pi_spy = hypo.calculate_space_time_yield(hypo_reactor, hypo_end_stream)
        pi_cost = hypo.calculate_run_material_cost(hypo_reactor, hypo_end_stream)

        # TODO Write the generated OutputChemical triples and PerformanceIndicator triples back to KG

        # Generate a list of PerformanceIndicator iri as agent output (new derived IRI)
        lst_performance_indicator_iri = [pi_yield.instance_iri, pi_conversion.instance_iri, pi_eco_score.instance_iri,
            pi_e_factor.instance_iri, pi_spy.instance_iri, pi_cost.instance_iri]
        return lst_performance_indicator_iri

    def collectInputsInformation(self, agent_inputs) -> str:
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
            return agent_inputs[ONTOHPLC_HPLCREPORT]
        else:
            self.logger.error('OntoHPLC:HPLCReport instance might be missing. Received inputs: ' + str(agent_inputs) + exception_string)
            raise Exception('OntoHPLC:HPLCReport instance might be missing. Received inputs: ' + str(agent_inputs) + exception_string)

def default():
    """
        Instructional message at the app root.
    """
    msg  = "This is an asynchronous agent that capable of post-processing experiment raw data generated from lab equipment.<BR>"
    msg += "For more information, please visit https://github.com/cambridge-cares/TheWorldAvatar/tree/134-dev-lab-equipment-digital-twin/Agents/PostProcAgent#readme<BR>"
    # TODO change above line to https://github.com/cambridge-cares/TheWorldAvatar/blob/develop/Agents/PostProcAgent#readme, before merging back to develop branch
    return msg
