from rdflib import Graph
import numpy as np

from pyderivationagent import DerivationAgent
from pyderivationagent import DerivationInputs
from pyderivationagent import DerivationOutputs

from copcalculationagent.datamodel.iris import *
from copcalculationagent.kg_operations.kgclient import KGClient
from copcalculationagent.kg_operations.tsclient import TSClient

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
    
    def process_request_parameters(self, derivation_inputs: DerivationInputs, 
                                   derivation_outputs: DerivationOutputs):
        # Update assumptions provided
        try: 
            with open('state.txt', "r") as file:
                has_function_run = file.read().strip() == "True"
        except:
            has_function_run = False
            with open('state.txt', "w") as file:
                file.write("False")  # Initialize the file with "False"
                print("Assumptions has been updated!")

        if not has_function_run:
            self.sparql_client.update_assumptions()
            with open('state.txt', "w") as file:
                file.write("True")
        
        inputs = derivation_inputs.getInputs()

        region = inputs[ONS_DEF_STAT][0]
        heatpumpefficiency_iri = inputs[REGION_HEATPUMP_EFFICIENCY][0]
        hotsidetemperature_iri = inputs[REGION_HOTSIDE_TEMPERATURE][0]

        g, mean_cop_iri, max_cop_iri, min_cop_iri = self.getCOPGraph(region)
        # Collect the generated triples derivation_outputs
        derivation_outputs.addGraph(g)
        
        # Initialze variables to be added to the TS client
        dates = []
        meanvalues = []
        minvalues = []
        maxvalues = []
        # Initialise TS client
        ts_client = TSClient(kg_client=self.sparql_client)

        # Perform SPARQL update for non-time series related triples (i.e. without TimeSeriesClient)
        temperature_iri_list = self.sparql_client.retrieve_temperature_iri(region)
        # Calculate COP and append to lists
        for i in range(len(temperature_iri_list)):
            temperature_iri = temperature_iri_list[i]
            res = self.sparql_client.get_temperature(temperature_iri)
            cop_max, cop_mean, cop_min = self.bulk_calculateCOP(temperature_iri, heatpumpefficiency_iri, hotsidetemperature_iri)
            dates.append(res["start"])
            maxvalues.append(cop_max)
            meanvalues.append(cop_mean)
            minvalues.append(cop_min)
        dataIRIs = [min_cop_iri,mean_cop_iri,max_cop_iri]
        values = [minvalues, meanvalues, maxvalues]
        
        # Create Time Series
        ts_client.add_timeseries(dates, dataIRIs, values)
        
        print('COP has been updated!')

    def getCOPGraph(self, region):
        
        # Initialise COP and return triples
        g = Graph()
        mean_cop_iri, max_cop_iri, min_cop_iri, deletion = self.sparql_client.verify_cop_iri(region)
        
        if deletion:
            # Initialise TS client
            ts_client = TSClient(kg_client=self.sparql_client)
            # Remove TimeSeries
            ts_client.delete_timeseries(mean_cop_iri)
            ts_client.delete_timeseries(max_cop_iri)
            ts_client.delete_timeseries(min_cop_iri)
            self.sparql_client.clear_cop_iris_triples(mean_cop_iri, max_cop_iri, min_cop_iri)
        
        # Perform SPARQL update for non-time series related triples (i.e. without TimeSeriesClient)
        g = self.sparql_client.instantiate_COP(g, mean_cop_iri, max_cop_iri, min_cop_iri, region)
        
        return g, mean_cop_iri, max_cop_iri, min_cop_iri
    
    def bulk_calculateCOP(self, temperature_iri, heatpumpefficiency_iri, hotsidetemperature_iri):
        
        res = self.sparql_client.get_temperature(temperature_iri)
        heatpumpefficiency = self.sparql_client.get_heatpumpefficiency(heatpumpefficiency_iri)
        hotsidetemperature= self.sparql_client.get_hotsidetemperature(hotsidetemperature_iri)

        cop_max = self.calculateCOP(res['maxtemperature'], heatpumpefficiency, hotsidetemperature)
        cop_mean = self.calculateCOP(res['meantemperature'], heatpumpefficiency, hotsidetemperature)
        cop_min = self.calculateCOP(res['mintemperature'], heatpumpefficiency, hotsidetemperature)

        return cop_max, cop_mean, cop_min

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

