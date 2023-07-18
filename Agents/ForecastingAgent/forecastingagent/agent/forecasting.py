################################################
# Authors: Markus Hofmeister (mh807@cam.ac.uk) #    
# Date: 17 Jul 2023                            #
################################################

# The purpose of this module is to provide the forecasting agent class based on 
# the pyderivationagent.DerivationAgent class, i.e., implementing the forecasting
# agent as derivation agent using synchronous derivation with time series

import uuid
import pandas as pd

from pyderivationagent import DerivationAgent
from pyderivationagent import DerivationInputs
from pyderivationagent import DerivationOutputs

from forecastingagent.datamodel.iris import *
#from forecastingagent.datamodel.data_mapping import TIME_FORMAT_LONG, TIME_FORMAT_SHORT
from forecastingagent.errorhandling.exceptions import TSException
from forecastingagent.kgutils.kgclient import KGClient
#from forecastingagent.kg_operations.tsclient import TSClient


class ForecastingAgent(DerivationAgent):

    def __init__(self, **kwargs):
        # Set flag whether to overwrite existing forecast or not (i.e., create new one)
        self.fc_overwrite = kwargs.pop('overwrite_fc')
        # Initialise DerivationAgent parent instance
        super().__init__(**kwargs)

        # Initialise the Sparql_client (with defaults specified in environment variables)
        self.sparql_client = self.get_sparql_client(KGClient)
        

    def agent_input_concepts(self) -> list:
        # Please note: Declared inputs/outputs need proper instantiation incl. 
        #              RDF TYPE declarations in the KG for the derivation to work
        return [TS_FORECASTINGMODEL, TIME_INTERVAL, TIME_DURATION, TS_FREQUENCY,
                OWL_THING]


    def agent_output_concepts(self) -> list:
        # Output concept (i.e., result) of the Derivation
        return [TS_FORECAST]


    def validate_inputs(self, http_request) -> bool:
        # Validate completeness of received HTTP request (i.e. non-empty HTTP request, 
        # contains derivationIRI, etc.) -> only relevant for synchronous derivation
        return super().validate_inputs(http_request)
    

    def validate_input_values(self, inputs, derivationIRI=None):
        """
        Check whether received input instances/values are suitable to derive forecast.
        Throw exception if data is not suitable.

        Arguments:
            inputs {dict} -- Dictionary of inputs with input concepts as keys and values as list
            derivationIRI {str} -- IRI of the derivation instance (optional)

        Returns:
            postcode_iri {str}, ppi_iri {str}, tx_records {list}
        """
        return True


    def process_request_parameters(self, derivation_inputs: DerivationInputs, 
                                   derivation_outputs: DerivationOutputs):
        """
            This method takes 
                1 IRI of OntoTimeSeries:ForecastingModel (model to be used for forecasting)
                1 IRI of OntoTimeSeries:Frequency (frequency of ts to be forecasted)
                1 IRI of Time:TimeInterval (time interval to be forecasted)
                1 IRI of Time:TimeDuration (data length prior to forecast interval
                                            to be used for training/data scaling)
                1 IRI of Owl:Thing (the concept holding the ts to be forecasted)
            and generates
                1 IRI of OntoTimeSeries:Forecast (incl. further relationships 
                                                  detailing forecast instance)
            
            Please Note: Output triples will only be generated when creating new info, i.e.,
                         instantiating new derivations. Otherwise, no outputs are generated
                         for derivations with time series
        """
        print("Forecasting task received.")


def default():
    """
    Instructional message at the agent root.
    """
    msg = '<B>Forecasting agent</B>:<BR><BR>'
    msg += 'The Forecasting Agent can predict instantiated time series and instantiate the forecasted series in the KG.<BR>'
    msg += "It is implemented as derivation agent using derivations with time series"
    msg += "<BR><BR>"
    msg += 'For further details please see the <a href="https://github.com/cambridge-cares/TheWorldAvatar/tree/main/Agents/ForecastingAgent/">Forecasting Agent README</a>.'
    return msg