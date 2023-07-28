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
                OM_QUANTITY, OWL_THING]


    def agent_output_concepts(self) -> list:
        # Output concept (i.e., result) of the Derivation
        return [TS_FORECAST]


    def validate_inputs(self, http_request) -> bool:
        # Validate completeness of received HTTP request (i.e. non-empty HTTP request, 
        # contains derivationIRI, etc.) -> only relevant for synchronous derivation
        return super().validate_inputs(http_request)
    

    def validate_input_values(self, inputs, derivationIRI=None):
        """
        Check whether received input instances are suitable to derive forecast.
        Throw exception if data is not suitable.

        Arguments:
            inputs {dict} -- Dictionary of inputs with input concepts as keys and values as list
            derivationIRI {str} -- IRI of the derivation instance (optional)

        Returns:
            dictionary of input IRIs as key-value pairs (no lists) with following keys: 
            'iri_to_forecast' as well as full concept IRIs for ts:ForecastingModel, 
            ts:Frequency, time:Interval, and time:Duration
        """

        def _find_unique_owl_thing_iris(dictionary):
            d = dictionary.copy()
            # Extract all owl:Thing IRIs
            owl_thing_iris = set(d[OWL_THING])
            d.pop(OWL_THING)
            # Flatten all IRIs for other rdf types and convert to set
            all_other_iris = set(sum(d.values(), []))
            unique_owl_thing = owl_thing_iris - all_other_iris
            # Return list of unique owl:Thing IRIs (empty list if none found)
            return list(unique_owl_thing)
        
        # Create dict between input concepts and return values
        input_iris = {
            TS_FORECASTINGMODEL: None,
            TIME_INTERVAL: None,
            TS_FREQUENCY: None,
            TIME_DURATION: None}

        # Verify that exactly one instance per concept is provided
        for i in input_iris:
            # Check whether input is available
            if not inputs.get(i):
                inp_name = i[i.rfind('/')+1:]
                inp_name = inp_name[inp_name.rfind('#')+1:]
                self.logger.error(f"Derivation {derivationIRI}: No '{inp_name}' IRI provided.")
                raise TypeError(f"Derivation {derivationIRI}: No '{inp_name}' IRI provided.")
            else:
                inp = inputs.get(i)
                # Check whether only one input has been provided
                if len(inp) == 1:
                    input_iris[i] = inp[0]
                else:
                    inp_name = i[i.rfind('/')+1:]
                    inp_name = inp_name[inp_name.rfind('#')+1:]
                    self.logger.error(f"Derivation {derivationIRI}: More than one '{inp_name}' IRI provided.")
                    raise TypeError(f"Derivation {derivationIRI}: More than one '{inp_name}' IRI provided.")

        # Retrieve IRI to forecast
        # Prio 1) If an om:Quantity is provided, use this one
        # Prio 2) If not, try to extract the appropriate owl:Thing
        if inputs.get(OM_QUANTITY):
            inp = inputs.get(OM_QUANTITY)
            # Check whether only one input has been provided
            if len(inp) == 1:
                input_iris['iri_to_forecast'] = inp[0]
            else:
                self.logger.error(f"Derivation {derivationIRI}: More than one 'om:Quantity' IRI provided to forecast.")
                raise TypeError(f"Derivation {derivationIRI}: More than one 'om:Quantity' IRI provided to forecast.")
        elif inputs.get(OWL_THING):
            msg = f"Derivation {derivationIRI}: No 'om:Quantity' IRI provided to forecast. "
            msg += "Trying to retrieve 'owl:Thing' IRI to forecast."
            self.logger.warning(msg)
            # Extract unique owl:Thing IRI
            inp = _find_unique_owl_thing_iris(inputs)
            # Check whether only one input has been provided
            if len(inp) == 1:
                input_iris['iri_to_forecast'] = inp[0]
            elif len(inp) == 0:
                self.logger.error(f"Derivation {derivationIRI}: Neither 'om:Quantity' nor 'owl:Thing' IRI provided to forecast.")
                raise TypeError(f"Derivation {derivationIRI}: Neither 'om:Quantity' nor 'owl:Thing' IRI provided to forecast.")
            else:
                self.logger.error(f"Derivation {derivationIRI}: No unique 'owl:Thing' IRI provided to forecast.")
                raise TypeError(f"Derivation {derivationIRI}: No unique 'owl:Thing' IRI provided to forecast.")

        return input_iris


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

        # Get input IRIs from the agent inputs (derivation_inputs)
        # (returns dict of inputs with input concepts as keys and values as list)
        inputs = derivation_inputs.getInputs()
        derivIRI = derivation_inputs.getDerivationIRI()
        input_iris = self.validate_input_values(inputs=inputs, derivationIRI=derivIRI)


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