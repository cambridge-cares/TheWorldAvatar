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
    
    def validate_input_values(self, inputs, derivationIRI=None):
        
        # Create dict between input concepts and return values
        input_dict = {EX_BIRTHDAY:None}

        # Verify that max. one instance per concept is provided
        for i in input_dict:
            # Check whether input is available
            if inputs.get(i):
                inp = inputs.get(i)
                # Check whether only one input has been provided
                if len(inp) == 1:
                    input_dict[i] = inp[0]
                else:
                    inp_name = i[i.rfind('/')+1:]
                    self.logger.error(f"Derivation {derivationIRI}: More than one '{inp_name}' IRI provided.")
                    raise Exception(f"Derivation {derivationIRI}: More than one '{inp_name}' IRI provided.")
            else: 
                self.logger.info(f"Derivation {derivationIRI}: Insufficient set of inputs provided.")
            return input_dict[EX_BIRTHDAY]
        
    def process_request_parameters(self, derivation_inputs: DerivationInputs, 
                                   derivation_outputs: DerivationOutputs):
        inputs = derivation_inputs.getInputs()
        derivIRI = derivation_inputs.getDerivationIRI()
        resulted_consumption_iri, unit_rate_iri = self.validate_input_values(inputs=inputs,
                                                    derivationIRI=derivIRI)
        
        g = self.get_utilitycost_Graph(resulted_consumption_iri, unit_rate_iri)
        # Collect the generated triples derivation_outputs
        derivation_outputs.addGraph(g)
    
    def get_utilitycost_Graph(self,
                              resulted_consumption_iri, 
                              unit_rate_iri):
        # Initialise results and return triples
        elec_cost = None
        gas_cost = None
        g = Graph()

        res = self.sparql_client.get_resultedconsumption(resulted_consumption_iri)
        electricity_unit_cost, gas_unit_cost = self.sparql_client.get_unit_rate(unit_rate_iri)

        # Calculate cost
        gas_cost = gas_unit_cost*res['gas_resulted_consump']
        elec_cost = electricity_unit_cost*res['elec_resulted_consump']

        # Verify iri
        utility_cost_iri, elec_cost_iri, fuel_cost_iri, gas_cost_iri = self.sparql_client.generate_utility_cost_iri(res['region'],
                                                                                                    res['start'],
                                                                                                    res['end'],
                                                                                                    gas_cost,
                                                                                                    elec_cost)
        g = self.sparql_client.instantiate_utilitycosts(g, utility_cost_iri, elec_cost_iri, fuel_cost_iri, gas_cost_iri,
                                                        res['region'], res['start'],  res['end'], gas_cost, elec_cost)
        
        return g

def default():
    """
        Instructional message at the app root.
    """
    # TODO: Update path to main upon merging
    msg  = "This is an utility cost calculation agent.<BR>"
    msg += "<BR>"
    msg += "For more information, please visit https://github.com/cambridge-cares/TheWorldAvatar/tree/main/Agents/UtilityCostCalculationAgent<BR>"
    return msg
