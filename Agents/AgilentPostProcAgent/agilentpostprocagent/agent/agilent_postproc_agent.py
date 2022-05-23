from pyderivationagent import DerivationAgent
from pyderivationagent import DerivationInputs
from pyderivationagent import DerivationOutputs
from pyderivationagent import FlaskConfig

from flask import Flask
from pathlib import Path

from agilentpostprocagent.kg_operations import *
from agilentpostprocagent.data_model import *
import agilentpostprocagent.hypo_rxn as hypo
from agilentpostprocagent.conf import *

class AgilentPostProcAgent(DerivationAgent):
    def process_request_parameters(self, derivation_inputs: DerivationInputs, derivation_outputs: DerivationOutputs):
        """This method takes iri of OntoHPLC:HPLCReport and generates a list of iris of OntoRxn:PerformanceIndicator."""
        # Create sparql_client
        self.sparql_client = ChemistryAndRobotsSparqlClient(
            self.kgUrl, self.kgUrl, kg_user=self.kgUser, kg_password=self.kgPassword, fs_url=self.fs_url, fs_user=self.fs_user, fs_pwd=self.fs_password
        )
        # Get the HPLCReport iri from the agent inputs (derivation_inputs)
        try:
            hplc_report_iri = derivation_inputs.getIris(ONTOHPLC_HPLCREPORT)[0]
        except Exception as e:
            self.logger.error(e)

        # Retrieve the ReactionExperiment instance that the HPLCReport is generated for
        rxn_exp_instance = self.sparql_client.get_rxn_exp_associated_with_hplc_report(hplc_report_iri)
        # Retrieve the InternalStandard instance that was used by the HPLCMethod linked to the HPLCReport via HPLCJob
        internal_standard_instance = self.sparql_client.get_internal_standard_associated_with_hplc_report(hplc_report_iri)

        # Construct an instance of HypoReactor given the ReactionExperiment information, get the value of internal_standard_run_conc_moleperlitre
        hypo_reactor, internal_standard_run_conc_moleperlitre, species_role_dct = hypo.construct_hypo_reactor(self.sparql_client, rxn_exp_instance, internal_standard_instance)

        # Process the raw hplc report and generate an instance of HPLCReport with its complete information
        hplc_report_instance = self.sparql_client.process_raw_hplc_report(hplc_report_iri=hplc_report_iri, internal_standard_species=internal_standard_instance.representsOccurenceOf,
            internal_standard_run_conc_moleperlitre=internal_standard_run_conc_moleperlitre, temp_local_folder=str(Path(__file__).absolute().parent))

        # Construct an instance of HypoEndStream given the processed HPLCReport instance and instance of HypoReactor
        hypo_end_stream = hypo.construct_hypo_end_stream(self.sparql_client, hplc_report_instance, hypo_reactor, species_role_dct)

        # Calculate each PerformanceIndicator
        lst_performance_indicator = []
        for perf_clz in [ONTORXN_YIELD, ONTORXN_CONVERSION, ONTORXN_ECOSCORE, ONTORXN_ENVIRONMENTALFACTOR, ONTORXN_SPACETIMEYIELD, ONTORXN_RUNMATERIALCOST]:
            pi = hypo.calculate_performance_indicator(
                hypo_reactor=hypo_reactor, hypo_end_stream=hypo_end_stream,
                rxn_exp_instance=rxn_exp_instance, target_clz=perf_clz, expected_amount=1
            )[0] # [0] is used here to simplify the implementation as we know there will be only one performance indicator for such clz type
            lst_performance_indicator.append(pi)

        # Collect the generated OutputChemical triples and PerformanceIndicator triples to a rdflib.Graph instance
        g = self.sparql_client.collect_triples_for_performance_indicators(lst_performance_indicator)
        g = self.sparql_client.collect_triples_for_output_chemical_of_chem_sol(hplc_report_instance.generatedFor, rxn_exp_instance.instance_iri, g)
        # self.sparql_client.write_performance_indicator_back_to_kg(lst_performance_indicator)
        # self.sparql_client.write_output_chemical_of_chem_sol_back_to_kg(hplc_report_instance.generatedFor, rxn_exp_instance.instance_iri)

        # Write all the generated triples to derivation_outputs
        derivation_outputs.addGraph(g)

    # def collectInputsInformation(self, agent_inputs) -> str:
    #     """
    #         This function checks the agent input against the I/O signature as declared in the PostProc Agent OntoAgent instance and collects information.
    #     """
    #     self.logger.info("Checking arguments...")
    #     exception_string = """Inputs are not provided in correct form. An example is: 
    #                             {
    #                                 "https://github.com/cambridge-cares/TheWorldAvatar/blob/develop/JPS_Ontology/ontology/ontohplc/OntoHPLC.owl#HPLCReport": "https://www.example.com/triplestore/ontohplc/HPLC_1/HPLCReport_1",
    #                             }"""
    #     # If the input JSON string is missing mandatory keys, raise error with "exception_string"
    #     if ONTOHPLC_HPLCREPORT in agent_inputs:
    #         return agent_inputs[ONTOHPLC_HPLCREPORT]
    #     else:
    #         self.logger.error('OntoHPLC:HPLCReport instance might be missing. Received inputs: ' + str(agent_inputs) + exception_string)
    #         raise Exception('OntoHPLC:HPLCReport instance might be missing. Received inputs: ' + str(agent_inputs) + exception_string)

def default():
    """
        Instructional message at the app root.
    """
    msg  = "This is an asynchronous agent that capable of post-processing experiment raw data generated from lab equipment.<BR>"
    msg += "For more information, please visit https://github.com/cambridge-cares/TheWorldAvatar/tree/134-dev-lab-equipment-digital-twin/Agents/PostProcAgent#readme<BR>"
    # TODO change above line to https://github.com/cambridge-cares/TheWorldAvatar/blob/develop/Agents/PostProcAgent#readme, before merging back to develop branch
    return msg
