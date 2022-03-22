from pyasyncagent import AsyncAgent, FlaskConfig
from flask import Flask
from pathlib import Path

from postprocagent.kg_operations import *
from postprocagent.data_model import *
import postprocagent.hypo_rxn as hypo
from postprocagent.conf import *

class PostProcAgent(AsyncAgent):
    def __init__(self, fs_url: str, fs_user: str, fs_pwd: str,
        agent_iri: str, time_interval: int, derivation_instance_base_url: str,
        kg_url: str, kg_user: str = None, kg_password: str = None,
        app: Flask = Flask(__name__), flask_config: FlaskConfig = FlaskConfig(), logger_name: str = "dev"
    ):
        super().__init__(agent_iri, time_interval, derivation_instance_base_url, kg_url, kg_user, kg_password, app, flask_config, logger_name)
        self.fs_url = fs_url
        self.fs_user = fs_user
        self.fs_pwd = fs_pwd

    def setupJob(self, agentInputs) -> list:
        """This method takes iri of OntoHPLC:HPLCReport and generates a list of iris of OntoRxn:PerformanceIndicator."""
        # Create sparql_client
        self.sparql_client = ChemistryAndRobotsSparqlClient(
            self.kgUrl, self.kgUrl, kg_user=self.kgUser, kg_password=self.kgPassword, fs_url=self.fs_url, fs_user=self.fs_user, fs_pwd=self.fs_pwd
        )
        # Get the HPLCReport iri from the agent inputs
        hplc_report_iri = self.collectInputsInformation(agentInputs)

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

        # Write the generated OutputChemical triples and PerformanceIndicator triples back to KG
        self.sparql_client.write_performance_indicator_back_to_kg(lst_performance_indicator)
        self.sparql_client.write_output_chemical_of_chem_sol_back_to_kg(hplc_report_instance.generatedFor, rxn_exp_instance.instance_iri)

        # Return a list of PerformanceIndicator iri as agent output (new derived IRI)
        return [pi.instance_iri for pi in lst_performance_indicator]

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
