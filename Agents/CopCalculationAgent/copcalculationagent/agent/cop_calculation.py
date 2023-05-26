from rdflib import Graph
import numpy as np
from tqdm import tqdm
import uuid

from pyderivationagent import DerivationAgent
from pyderivationagent import DerivationInputs
from pyderivationagent import DerivationOutputs

from copcalculationagent.datamodel.iris import *
from copcalculationagent.kg_operations.kgclient import KGClient, HEATPUMP_EFFICIENCY, HOTSIDE_TEMPERATURE


class COPCalculationAgent(DerivationAgent):

    def __init__(self, **kwargs):
        # Initialise DerivationAgent parent instance
        super().__init__(**kwargs)

        # Initialise the Sparql_client (with defaults specified in environment variables)
        self.sparql_client = self.get_sparql_client(KGClient)

    def agent_input_concepts(self) -> list:
        # Validate completeness of received HTTP request (i.e. non-empty HTTP request, 
        # contains derivationIRI, etc.) -> only relevant for synchronous derivation
        return [ONS_DEF_STAT, REGION_HEATPUMP_EFFICIENCY, REGION_HOTSIDE_TEMPERATURE]
    
    def agent_output_concepts(self) -> list:
        return [REGION_COP]
    
    def validate_inputs(self, http_request) -> bool:
        # Validate completeness of received HTTP request (i.e. non-empty HTTP request, 
        # contains derivationIRI, etc.) -> only relevant for synchronous derivation
        return super().validate_inputs(http_request)
    
    def validate_input_values(self, inputs, derivationIRI=None):
        
        # Create dict between input concepts and return values
        input_dict = {OM_MEASURE:None,
                      REGION_HEATPUMP_EFFICIENCY:None,
                      REGION_HOTSIDE_TEMPERATURE:None}

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
            
        return input_dict[OM_MEASURE], input_dict[REGION_HEATPUMP_EFFICIENCY], input_dict[REGION_HOTSIDE_TEMPERATURE]
        
    def process_request_parameters(self, derivation_inputs: DerivationInputs, 
                                   derivation_outputs: DerivationOutputs):
        # Update assumptions provided
        #heatpumpefficiency_iri, hotsidetemperature_iri = self.sparql_client.update_assumptions()
        
        inputs = derivation_inputs.getInputs()
        derivIRI = derivation_inputs.getDerivationIRI()

        region = inputs[ONS_DEF_STAT][0]
        res = self.sparql_client.verify_cop_iri(region)
        if res:
            return
        else:
            heatpumpefficiency_iri = inputs[REGION_HEATPUMP_EFFICIENCY][0]
            hotsidetemperature_iri = inputs[REGION_HOTSIDE_TEMPERATURE][0]

            temperature_iri_list = self.sparql_client.retrieve_temperature_iri(region)
            # temperature_iri, heatpumpefficiency_iri, hotsidetemperature_iri = self.validate_input_values(inputs=inputs,
            #                                              derivationIRI=derivIRI)
            for i in tqdm(range(len(temperature_iri_list))):
                temperature_iri = temperature_iri_list[i]
                g = self.getCOPGraph(temperature_iri, heatpumpefficiency_iri, hotsidetemperature_iri)

                # Collect the generated triples derivation_outputs
                derivation_outputs.addGraph(g)

        print('COP has been updated!')

    def getCOPGraph(self, temperature_iri, heatpumpefficiency_iri, hotsidetemperature_iri):
        
        # Initialise COP and return triples
        cop_mean = None
        g = Graph()
        res = self.sparql_client.get_temperature(temperature_iri)
        heatpumpefficiency = self.sparql_client.get_heatpumpefficiency(heatpumpefficiency_iri)
        hotsidetemperature= self.sparql_client.get_hotsidetemperature(hotsidetemperature_iri)
      
        cop_max = self.calculateCOP(res['maxtemperature'], heatpumpefficiency, hotsidetemperature)
        cop_mean = self.calculateCOP(res['meantemperature'], heatpumpefficiency, hotsidetemperature)
        cop_min = self.calculateCOP(res['mintemperature'], heatpumpefficiency, hotsidetemperature)
        
        if cop_mean:
            cop_iri = REGION + "COP_" + str(uuid.uuid4())

        g = self.sparql_client.instantiate_COP(g, cop_iri, res['region'], res['start'], res['end'], cop_max, cop_mean, cop_min)
        

        return g
    
    def calculateCOP(self, temperature, heatpumpefficiency, hotsidetemperature):
        '''
        Based on a given temperature to calculate the COP
        Note: COP = (hp_efficiency * T_H) / (T_H - T_C), where the input temperature is represented as T_C
        T_H, hp_efficiency are hypothesd as constant, which have default value as 318.15 and 0.35
        respectfully. I suggest to check if that default value is up to date or if that hypothesis is 
        valid in your case
        '''
        COP = heatpumpefficiency * hotsidetemperature / (hotsidetemperature -273.15 - temperature)
        
        COP = np.round(COP,3)
        
        return COP


def default():
    """
        Instructional message at the app root.
    """
    # TODO: Update path to main upon merging
    msg  = "This is an COPCalculation agent.<BR>"
    msg += "<BR>"
    msg += "For more information, please visit https://github.com/cambridge-cares/TheWorldAvatar/tree/main/Agents/CopCalculationAgent<BR>"
    return msg

