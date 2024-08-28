################################################
# Authors: Jieyang Xu (jx309@cam.ac.uk)        #    
# Date: 17 May 2023                            #
################################################
import uuid
from rdflib import Graph
import re
import numpy as np

from pyderivationagent import DerivationAgent
from pyderivationagent import DerivationInputs
from pyderivationagent import DerivationOutputs

from inequalityindexagent.datamodel.iris import *
from inequalityindexagent.kg_operations.kgclient import KGClient
from inequalityindexagent.kg_operations.tsclient import TSClient


class InequalityIndexCalculationAgent(DerivationAgent):

    def __init__(self, **kwargs):
        # Initialise DerivationAgent parent instance
        super().__init__(**kwargs)

        # Initialise the Sparql_client (with defaults specified in environment variables)
        self.sparql_client = self.get_sparql_client(KGClient)

    def agent_input_concepts(self) -> list:
        return [ONTOCAPE_UTILITYCOST, REGION_MIN_FP, REGION_MAX_FP, OFP_HOUSHOLD]
    
    def agent_output_concepts(self) -> list:
        return [REGION_INEQUALITYINDEX]
    
    def validate_inputs(self, http_request) -> bool:
        # Validate completeness of received HTTP request (i.e. non-empty HTTP request, 
        # contains derivationIRI, etc.) -> only relevant for synchronous derivation
        return super().validate_inputs(http_request)
        
    def process_request_parameters(self, derivation_inputs: DerivationInputs, 
                                   derivation_outputs: DerivationOutputs):
        
        # ------- Update assumptions provided ----------- #
        try: 
            with open('state.txt', "r") as file:
                has_function_run = file.read().strip() == "True"
        except:
            has_function_run = False
            with open('state.txt', "w") as file:
                file.write("False")  # Initialize the file with "False"
        if not has_function_run:
            # Update Unitrate provided
            self.sparql_client.update_min_max_fp_iri()
            print("Assumptions/Indecies has been updated!")
            with open('state.txt', "w") as file:
                file.write("True")

        # ----- Calculate / retrieve standard deviation ----- #
        try:
            with open('std.txt', "r") as file:
                content = file.read()
                existing_float = re.search(r'[-+]?\d*\.\d+|\d+', content)
            if existing_float:
                std = float(existing_float.group())
        except:
            with open('std.txt', "w") as file:
                existing_float = None
        if existing_float is None:
            std = self.get_standard_deviation()
            content = re.sub(r'[-+]?\d*\.\d+|\d+', str(std), content)
            with open('std.txt', 'w') as file:
                file.write(content)

        # ---------------- Get Input IRIs --------------- #
        inputs = derivation_inputs.getInputs()
        utility_cost_iri_list = inputs[ONTOCAPE_UTILITYCOST]
        min_fp_iri = inputs[REGION_MIN_FP][0]
        max_fp_iri =  inputs[REGION_MAX_FP][0]
        household_iri =  inputs[OFP_HOUSHOLD][0]
        for a in utility_cost_iri_list:
            res = self.sparql_client.get_status_of_utility_cost(a)
            if "Before" in res:
                before_utility_cost_iri = a
            elif "After" in a:
                after_utility_cost_iri = a
            else:
                self.logger.error(f"Utility Cost IRI can not be found")
                raise Exception(f"Utility Cost IRI can not be found")
        
        # ---------------- Perform Calculation ------------- #
        # Retrieve variables from the TS client
        dates = []
        delta_cost_list = []
        # Initialise TS client
        ts_client = TSClient(kg_client=self.sparql_client)

        elec_cost_iri, gas_cost_iri = self.sparql_client.get_elec_cost_gas_cost_iri(before_utility_cost_iri, 'Before')
        elec_cost_before_dict = ts_client.retrieve_data(elec_cost_iri)
        gas_cost_before_dict = ts_client.retrieve_data(gas_cost_iri)

        elec_cost_iri, gas_cost_iri = self.sparql_client.get_elec_cost_gas_cost_iri(after_utility_cost_iri, 'After')
        elec_cost_after_dict = ts_client.retrieve_data(elec_cost_iri)
        gas_cost_after_dict = ts_client.retrieve_data(gas_cost_iri)

        for key, value in elec_cost_before_dict.items():
            # Calculate change of cost
            delta_cost = elec_cost_after_dict[key] + gas_cost_after_dict[key] - (value + gas_cost_before_dict[key])
            delta_cost_list.append[delta_cost]

        total_delta_cost = sum(delta_cost_list)
        fp, region = self.sparql_client.get_fuel_poverty_proportion(household_iri)
        min_fp, max_fp = self.sparql_client.get_min_max_fp(min_fp_iri, max_fp_iri)
        inequality_index = (total_delta_cost/std) * (fp - min_fp) / (max_fp - min_fp)

        # ---------------- Populate Triples for Non-Timeseries in the KG --------------- #
        g, elec_cost_iri, gas_cost_iri = self.get_inequality_index_Graph(region)
        
        # Collect the generated triples derivation_outputs
        derivation_outputs.addGraph(g)
        
        print('Inequality Index has been updated!')


    def get_inequality_index_Graph(self, inequality_index, 
                              region):
        # Initialise results and return triples
        g = Graph()

        # Verify iri
        inequalityindex_iri = self.sparql_client.generate_inequality_index_iri(region)

        # Instantisation
        g = self.sparql_client.instantiate_inequality_index(g, inequalityindex_iri, inequality_index, region)
        
        return g
    
    def get_standard_deviation(self):

        before_utility_cost_iris_list, after_utility_cost_iris_list = self.sparql_client.get_all_utility_cost_iris()
        total_delta_cost_list = []
        # Initialise TS client
        for i in range(len(before_utility_cost_iris_list)):
            delta_cost_list = []
            before_utility_cost_iri = before_utility_cost_iris_list[i]
            after_utility_cost_iri = after_utility_cost_iris_list[i]
            ts_client = TSClient(kg_client=self.sparql_client)
            elec_cost_iri, gas_cost_iri = self.sparql_client.get_elec_cost_gas_cost_iri(before_utility_cost_iri, 'Before')
            elec_cost_before_dict = ts_client.retrieve_data(elec_cost_iri)
            gas_cost_before_dict = ts_client.retrieve_data(gas_cost_iri)

            elec_cost_iri, gas_cost_iri = self.sparql_client.get_elec_cost_gas_cost_iri(after_utility_cost_iri, 'After')
            elec_cost_after_dict = ts_client.retrieve_data(elec_cost_iri)
            gas_cost_after_dict = ts_client.retrieve_data(gas_cost_iri)

            for key, value in elec_cost_before_dict.items():
                # Calculate change of cost
                delta_cost = elec_cost_after_dict[key] + gas_cost_after_dict[key] - (value + gas_cost_before_dict[key])
                delta_cost_list.append[delta_cost]

            total_delta_cost = sum(delta_cost_list)
            total_delta_cost_list.append(total_delta_cost)
        std = np.nanstd(np.array(total_delta_cost_list))
        return std

def default():
    """
        Instructional message at the app root.
    """
    # TODO: Update path to main upon merging
    msg  = "This is an inequality index calculation agent.<BR>"
    msg += "<BR>"
    msg += "For more information, please visit https://github.com/cambridge-cares/TheWorldAvatar/tree/main/Agents/InequalityIndexCalculationAgent<BR>"
    return msg
