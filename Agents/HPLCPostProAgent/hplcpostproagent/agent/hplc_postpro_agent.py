from pyderivationagent import DerivationAgent
from pyderivationagent import DerivationInputs
from pyderivationagent import DerivationOutputs
from pyderivationagent import FlaskConfig

from flask import Flask
from pathlib import Path

from hplcpostproagent.kg_operations import *
from hplcpostproagent.data_model import *
import hplcpostproagent.hypo_rxn as hypo

class HPLCPostProAgent(DerivationAgent):
    def __init__(self,
        **kwargs
    ):
        super().__init__(**kwargs)

        # Initialise the sparql_client
        self.sparql_client = self.get_sparql_client(ChemistryAndRobotsSparqlClient)

    def agent_input_concepts(self) -> list:
        return [ONTOHPLC_HPLCREPORT]

    def agent_output_concepts(self) -> list:
        return [ONTOREACTION_PERFORMANCEINDICATOR]

    def validate_inputs(self, http_request) -> bool:
        return super().validate_inputs(http_request)

    def process_request_parameters(self, derivation_inputs: DerivationInputs, derivation_outputs: DerivationOutputs):
        """This method takes iri of OntoHPLC:HPLCReport and generates a list of iris of OntoRxn:PerformanceIndicator."""
        # Get the HPLCReport iri from the agent inputs (derivation_inputs)
        hplc_report_iri = derivation_inputs.getIris(ONTOHPLC_HPLCREPORT)[0]

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
        for perf_clz in [
            ONTOREACTION_YIELD,
            ONTOREACTION_CONVERSION,
            # ONTOREACTION_ECOSCORE, # NOTE disabled as the data for this eco score is not properly available
            ONTOREACTION_ENVIRONMENTALFACTOR,
            ONTOREACTION_SPACETIMEYIELD,
            ONTOREACTION_RUNMATERIALCOST,
            ONTOREACTION_RUNMATERIALCOSTPERKILOGRAMPRODUCT,
        ]:
            pi = hypo.calculate_performance_indicator(
                hypo_reactor=hypo_reactor, hypo_end_stream=hypo_end_stream,
                rxn_exp_instance=rxn_exp_instance, target_clz=perf_clz
            )[0] # [0] is used here to simplify the implementation as we know there will be only one performance indicator for such clz type
            # TODO add support if multiple PerformanceIndicator of the same type can be computed?
            if pi is not None:
                # NOTE here the performance indicator might be None for conversion if some response factor is missing, e.g. those of starting chemicals
                lst_performance_indicator.append(pi)

        # Collect the generated OutputChemical triples, PerformanceIndicator triples and HPLCReport triples to derivation_outputs
        derivation_outputs.addGraph(self.sparql_client.collect_triples_for_performance_indicators(lst_performance_indicator, Graph()))
        derivation_outputs.addGraph(self.sparql_client.collect_triples_for_output_chemical_of_chem_sol(hplc_report_instance.generatedFor, rxn_exp_instance.instance_iri, Graph()))
        derivation_outputs.addGraph(self.sparql_client.collect_triples_for_chromatogram_point(hplc_report_instance.records, hplc_report_iri, Graph()))


def default():
    """
        Instructional message at the app root.
    """
    msg  = "This is an asynchronous agent that capable of post-processing experiment raw data generated from lab equipment.<BR>"
    msg += "For more information, please visit https://github.com/cambridge-cares/TheWorldAvatar/tree/main/Agents/HPLCPostProAgent#readme<BR>"
    return msg
