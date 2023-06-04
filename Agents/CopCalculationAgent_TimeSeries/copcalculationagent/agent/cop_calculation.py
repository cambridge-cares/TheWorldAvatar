from rdflib import Graph
import numpy as np
from tqdm import tqdm
import uuid

from pyderivationagent import DerivationAgent
from pyderivationagent import DerivationInputs
from pyderivationagent import DerivationOutputs

from copcalculationagent.datamodel.iris import *
from copcalculationagent.kg_operations.kgclient import KGClient, HEATPUMP_EFFICIENCY, HOTSIDE_TEMPERATURE
from copcalculationagent.kg_operations.tsclient import TSClient
from copcalculationagent.errorhandling.exceptions import TSException
from copcalculationagent.datamodel.data import DATACLASS, TIME_FORMAT

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
        heatpumpefficiency_iri, hotsidetemperature_iri = self.sparql_client.update_assumptions()
        
        inputs = derivation_inputs.getInputs()
        derivIRI = derivation_inputs.getDerivationIRI()

        region = inputs[ONS_DEF_STAT][0]
        heatpumpefficiency_iri = inputs[REGION_HEATPUMP_EFFICIENCY][0]
        hotsidetemperature_iri = inputs[REGION_HOTSIDE_TEMPERATURE][0]

        # Perform SPARQL update for non-time series related triples (i.e. without TimeSeriesClient)
        temperature_iri_list = self.sparql_client.retrieve_temperature_iri(region)
        g, mean_cop_iri, max_cop_iri, min_cop_iri = self.getCOPGraph(temperature_iri_list[0])
        # Collect the generated triples derivation_outputs
        derivation_outputs.addGraph(g)
        
        # Initialze variables to be added to the TS client
        dates = []
        meanvalues = []
        minvalues = []
        maxvalues = []
        # Initialise TS client
        ts_client = TSClient(kg_client=self.sparql_client)

        # Calculate COP and append to lists
        for i in tqdm(range(len(temperature_iri_list))):
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
        self.create_timeseries(self, ts_client, dates, dataIRIs, values)
        
        print('COP has been updated!')

    def create_timeseries(self, ts_client, dates, dataIRIs, values):
        try:
            # Create time series from test data                        
            ts = TSClient.create_timeseries(dates, dataIRIs, values)
            with ts_client.connect() as conn:
                # Initialise time series in Blazegraph and PostgreSQL
                ts_client.tsclient.initTimeSeries(dataIRIs, [DATACLASS]*len(dataIRIs), TIME_FORMAT, conn)
                # Add test time series data
                ts_client.tsclient.addTimeSeriesData(ts, conn)

        except Exception as ex:
            self.logger.error('Error wrapping COP data time series')
            raise TSException('Error wrapping COP data time series') from ex
        
    def getCOPGraph(self, temperature_iri):
        
        # Initialise COP and return triples
        g = Graph()
        res = self.sparql_client.get_temperature(temperature_iri)
        mean_cop_iri, max_cop_iri, min_cop_iri = self.sparql_client.verify_cop_iri(res['region'])

        # Perform SPARQL update for non-time series related triples (i.e. without TimeSeriesClient)
        g = self.sparql_client.instantiate_COP(g, mean_cop_iri, max_cop_iri, min_cop_iri, res['region'])
        
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

