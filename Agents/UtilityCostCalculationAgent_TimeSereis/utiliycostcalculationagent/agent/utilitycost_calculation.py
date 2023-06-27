################################################
# Authors: Jieyang Xu (jx309@cam.ac.uk)        #    
# Date: 17 May 2023                            #
################################################
import uuid
from rdflib import Graph
import datetime

from pyderivationagent import DerivationAgent
from pyderivationagent import DerivationInputs
from pyderivationagent import DerivationOutputs

from utiliycostcalculationagent.datamodel.iris import *
from utiliycostcalculationagent.kg_operations.kgclient import KGClient
from utiliycostcalculationagent.kg_operations.tsclient import TSClient


class UtilityCostCalculationAgent(DerivationAgent):

    def __init__(self, **kwargs):
        # Initialise DerivationAgent parent instance
        super().__init__(**kwargs)

        # Initialise the Sparql_client (with defaults specified in environment variables)
        self.sparql_client = self.get_sparql_client(KGClient)

    def agent_input_concepts(self) -> list:
        return [REGION_RESULTED_ENERGYCONSUMPTION, ONTOHEATNETWORK_UNITRATE]
    
    def agent_output_concepts(self) -> list:
        return [ONTOCAPE_UTILITYCOST]
    
    def validate_inputs(self, http_request) -> bool:
        # Validate completeness of received HTTP request (i.e. non-empty HTTP request, 
        # contains derivationIRI, etc.) -> only relevant for synchronous derivation
        return super().validate_inputs(http_request)
        
    def process_request_parameters(self, derivation_inputs: DerivationInputs, 
                                   derivation_outputs: DerivationOutputs):
          
        # ---------------- Get Input IRIs --------------- #
        inputs = derivation_inputs.getInputs()
        resulted_consumption_iri = inputs[REGION_RESULTED_ENERGYCONSUMPTION]
        unit_rate_iri = inputs[ONTOHEATNETWORK_UNITRATE]
        
        
        # ---------------- Perform Calculation and populate Timeseries data ------------- #
        # Initialise variables to be added to the TS client
        dates = []
        gas_cost_list = []
        elec_cost_list = []
        # Initialise TS client
        ts_client = TSClient(kg_client=self.sparql_client)
        res = self.sparql_client.get_resulted_gas_elec_consumption_iri(resulted_consumption_iri)
        electricity_unit_cost, gas_unit_cost = self.sparql_client.get_unit_rate(unit_rate_iri)
        resulted_gas_consumption_dict = ts_client.retrieve_data(res['gas_consumption_iri'])
        resulted_elec_consumption_dict = ts_client.retrieve_data(res['elec_consumption_iri'])

        for key, value in resulted_gas_consumption_dict.items():
            # Calculate cost
            gas_cost = gas_unit_cost * value
            elec_cost = electricity_unit_cost * resulted_elec_consumption_dict[key]
            dates.append(key)
            gas_cost_list.append(gas_cost)
            elec_cost_list.append(elec_cost)

        # ---------------- Populate Triples for Non-Timeseries in the KG --------------- #
        g, elec_cost_iri, gas_cost_iri = self.get_utilitycost_Graph(res['region'])
        # Collect the generated triples derivation_outputs
        derivation_outputs.addGraph(g)

        dataIRIs = [elec_cost_iri, gas_cost_iri]
        values = [elec_cost_list, gas_cost_list]
        
        # Create Time Series
        ts_client.add_timeseries(dates, dataIRIs, values)
        
        print('Utility Cost has been updated!')


    def get_utilitycost_Graph(self,
                              region):
        # Initialise results and return triples
        g = Graph()

        # Verify iri
        utility_cost_iri, elec_cost_iri, fuel_cost_iri, gas_cost_iri, deletion = self.sparql_client.generate_utility_cost_iri(region)
        if deletion:
            # Initialise TS client
            ts_client = TSClient(kg_client=self.sparql_client)
            # Remove TimeSeries
            ts_client.delete_timeseries(elec_cost_iri)
            ts_client.delete_timeseries(gas_cost_iri)
            self.sparql_client.remove_triples_for_iri(utility_cost_iri, elec_cost_iri, fuel_cost_iri, gas_cost_iri)
        
        # Instantisation
        g = self.sparql_client.instantiate_utilitycosts(g, utility_cost_iri, elec_cost_iri, fuel_cost_iri, gas_cost_iri,region)
        
        return g, elec_cost_iri, gas_cost_iri

def default():
    """
        Instructional message at the app root.
    """
    # TODO: Update path to main upon merging
    msg  = "This is an utility cost calculation agent.<BR>"
    msg += "<BR>"
    msg += "For more information, please visit https://github.com/cambridge-cares/TheWorldAvatar/tree/main/Agents/UtilityCostCalculationAgent<BR>"
    return msg
