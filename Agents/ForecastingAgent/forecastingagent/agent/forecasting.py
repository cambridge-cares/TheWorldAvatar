################################################
# Authors: Markus Hofmeister (mh807@cam.ac.uk) #    
# Date: 17 Jul 2023                            #
################################################

# The purpose of this module is to provide the ForecastingAgent class based on 
# the pyderivationagent.DerivationAgent class, i.e., implementing the forecasting
# agent as derivation agent using synchronous derivation with time series

import pandas as pd
from darts import TimeSeries
from flask import request, jsonify
from rdflib import Graph, URIRef

from pyderivationagent import DerivationAgent
from pyderivationagent import DerivationInputs
from pyderivationagent import DerivationOutputs

from forecastingagent.datamodel.iris import *
from forecastingagent.agent.forcasting_config import *
from forecastingagent.agent.forecasting_tasks import forecast, calculate_error
from forecastingagent.errorhandling.exceptions import InvalidInput
from forecastingagent.kgutils.kgclient import KGClient
from forecastingagent.kgutils.tsclient import TSClient
from forecastingagent.utils.env_configs import DB_USER, DB_PASSWORD


class ForecastingAgent(DerivationAgent):

    def __init__(self, **kwargs):
        # Set flag whether to overwrite existing forecast or not (i.e., create new one)
        self.fc_overwrite = kwargs.pop('overwrite_fc')
        # Specify default rounding behavior for forecast values 
        # (i.e. number of target decimal places; no rounding if not specified)
        self.fc_round = kwargs.pop('round_fc')
        # Initialise DerivationAgent parent instance
        super().__init__(**kwargs)

        # Initialise the Sparql_client (with defaults specified in environment variables)
        self.sparql_client = self.get_sparql_client(KGClient)
        

    def agent_input_concepts(self) -> list:
        # Please note: Declared inputs/outputs need proper instantiation incl. 
        #              RDF TYPE declarations in the KG for the derivation to work
        # NOTE: The instance to forecast must either be of type om:Quantity (priority 1) 
        #       or owl:Thing (priority 2). Although om:Quantity will likely cover most of
        #       the current use cases, the support for owl:Thing ensures that also concepts
        #       from other ontologies can be forecasted. For detailed discussion also see:
        # https://github.com/cambridge-cares/TheWorldAvatar/pull/866#discussion_r1311530447
        # https://github.com/cambridge-cares/TheWorldAvatar/pull/866#issuecomment-1713483223
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
        # NOTE: When forecasting subclasses of om:Quantity, then "IRI to forecast" 
        #       is handled differently by the Derived Information Framework: 
        #       1) for createNewInfo, the IRI will be returned as instance of om:Quantity
        #       2) for derivation updates, the IRI will be returned as instance of the
        #          the actual subclass type
        #       Hence, the following code distinguishes between the two cases
        #
        # 1) For createSyncDerivationForNewInfo
        #       Prio 1) If (subclass of) an om:Quantity is provided, use this one
        #       Prio 2) If not, try to extract the appropriate owl:Thing
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
        
        # 2) For existing derivations
        else:
            # If neither om:Quantity nor owl:Thing is marked up directly as input
            # (e.g., in cases where a subclass of om:Quantity is forecasted), try to
            # extract the appropriate IRI
            type_to_forecast = [k for k in inputs if k not in input_iris.keys()]
            if len(type_to_forecast) != 1:
                self.logger.error(f"Derivation {derivationIRI}: Rdf:type to forecast could not be determined.")
                raise TypeError(f"Derivation {derivationIRI}: Rdf:type to forecast could not be determined.")
            else:
                inp = inputs.get(type_to_forecast[0])
                # Check whether only one input has been provided
                if len(inp) == 1:
                    input_iris['iri_to_forecast'] = inp[0]
                else:
                    self.logger.error(f"Derivation {derivationIRI}: IRI to forecast could not be determined.")
                    raise TypeError(f"Derivation {derivationIRI}: IRI to forecast could not be determined.")

        return input_iris


    def process_request_parameters(self, derivation_inputs: DerivationInputs, 
                                   derivation_outputs: DerivationOutputs):
        """
        This method takes 
            1 IRI of OntoTimeSeries:ForecastingModel (model to be used for forecasting)
            1 IRI of OntoTimeSeries:Frequency (frequency of forecasted time series)
            1 IRI of Time:TimeInterval (time interval to be forecasted)
            1 IRI of Time:TimeDuration (data length prior to forecast interval
                                        to be used for training/data scaling)
            1 IRI of OM:Quantity or owl:Thing (the concept associated with the 
                                               time series to be forecasted)
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
        # Get inputs as unique key-value pairs and extract IRI to forecast
        input_iris = self.validate_input_values(inputs=inputs, derivationIRI=derivIRI)

        # Query relevant forecasting inputs from KG
        # 1) Time series data to forecast (incl. time series RDB link and format)
        ts = self.sparql_client.get_time_series_details(input_iris['iri_to_forecast'])
        # 2) Forecasting model
        fcmodel = self.sparql_client.get_fcmodel_details(input_iris[TS_FORECASTINGMODEL])
        # 3) Forecast interval and frequency
        interval = self.sparql_client.get_interval_details(input_iris[TIME_INTERVAL])
        frequency = self.sparql_client.get_duration_details(input_iris[TS_FREQUENCY])
        # 4) Data length for training and data scaling
        data_hist = self.sparql_client.get_duration_details(input_iris[TIME_DURATION])

        # Create forecasting configuration dict
        cfg = create_forecast_configuration(model=fcmodel, ts_details=ts, ts_frequency=frequency, 
                                            hist_duration=data_hist, fc_interval=interval)
        # Add IRI to forecast to config (i.e., IRI which will get ts:hasForecast relationship)
        cfg['iri_to_forecast'] = input_iris['iri_to_forecast']

        # Create forecast
        rdb_url, time_format = get_rdb_endpoint(ts)
        ts_client = TSClient(kg_client=self.sparql_client, rdb_url=rdb_url, 
                             rdb_user=DB_USER, rdb_password=DB_PASSWORD)
        fc_ts = forecast(config=cfg, kgClient=self.sparql_client, tsClient=ts_client, 
                         db_url=rdb_url, time_format=time_format)
        # Extract times and values from darts TimeSeries
        times = [str(x) for x in fc_ts.time_index.tz_localize('UTC').strftime(time_format)]
        values = fc_ts.values().squeeze().tolist()
        if self.fc_round:
            # Round data values if set in environment variables
            values = [round(x, self.fc_round) for x in values]

        # Instantiate new forecast in KG and RDB (if requested or not yet existing)
        if not self.fc_overwrite or not bool(cfg.get('fc_iri')):
            # Retrieve potential existing forecast IRI (None if not existing)
            old_fc_iri = cfg.get('fc_iri')
            # Initialise forecast in KG
            fc_iri = self.sparql_client.instantiate_forecast(forecast=fc_ts, config=cfg,
                                                             create_new=True)
            # Create new forecast instance and add ts data in RDB
            ts_client.init_timeseries(dataIRI=fc_iri, times=times, values=values, 
                                      ts_type=cfg['ts_data_type'], time_format=time_format)
            if old_fc_iri:
                # In case new forecast has been created for existing derivation,
                # disconnect old forecast instance from derivation and connect with new one
                self.sparql_client.reconnect_forecast_with_derivation(old_fc_iri, fc_iri)

            # Add output graph to ensure complete derivation markup
            # --> this part of the code is only relevant when called via 
            # 'createSyncDerivationForNewInfo' and its only purpose is to ensure
            #  that forecast instance is marked up as "belongsTo" the derivation
            g = Graph()
            g.add((URIRef(fc_iri), URIRef(RDF_TYPE), URIRef(TS_FORECAST)))   
            derivation_outputs.addGraph(g)

        else:
            # Only update forecast metadata in KG
            self.sparql_client.instantiate_forecast(forecast=fc_ts, config=cfg)
            # Update forecast time series data in RDB
            # NOTE: Entire previous forecast data is replaced, i.e., NOT just appending
            #       new data and potentially overwriting existing data
            ts_client.replace_ts_data(dataIRI=cfg['fc_iri'], times=times, values=values)
            #ts_client.add_ts_data(dataIRI=cfg['fc_iri'], times=times, values=values)
        
        created_at = pd.to_datetime('now', utc=True)
        logger.info(f'Created forecast at: {created_at}')

        # NOTE: DerivationWithTimeSeries does not return any output triples, 
        #       as all updates to the time series are expected to be conducted
        #       within the agent logic 
        

    def evaluate_forecast_error(self):
        """
        This method evaluates multiple error metrics between two time series,
        mainly to assess forecasting errors. 
        
        The method expects a HTTP POST JSON body with the following parameters:
        { "query": {
            "dataIRI_target": "https://...",
            "dataIRI_fc" : "https://..."
            }
        }
        """
        
        #
        # 1) Check HTTP request parameters
        #
        # Get received 'query' JSON object which holds all request parameters
        try:
            try:
                query = request.json['query']
            except Exception as ex:
                msg = "No 'query' node provided in HTTP request: " + str(ex)
                logger.error(msg, ex)
                raise InvalidInput(msg) from ex
            
            inputs = {'target': 'dataIRI_target', 'forecast': 'dataIRI_fc'}
            # Extract time series data IRIs to evaluate
            try:
                for k, v in inputs.items():
                    inputs[k] = str(query[v])
            except Exception as ex:
                msg = 'Unable to extract time series data IRIs to evaluate: ' + str(ex)
                logger.error(msg)
                raise InvalidInput(msg) from ex

            # 2) Retrieve time series data
            ts_client = TSClient(kg_client=self.sparql_client)
            for k, v in inputs.items():
                try:
                    # Retrieve time series data using TimeSeriesClient
                    times, values = ts_client.retrieve_timeseries(v)
                    # Create darts TimeSeries objects
                    t = pd.DatetimeIndex(times)
                    inputs[k] = TimeSeries.from_times_and_values(times=t, values=values)
                except Exception as ex:
                    msg = 'Unable to retrieve time series data: ' + str(ex)
                    logger.error(msg)
                    raise InvalidInput(msg) from ex

            # 3) Estimate forecast error (based on longest overlapping time series)
            # NOTE: Different time series lengths are not an issue as long as they
            #       have at least one overlapping time point -> error metrics are
            #       calculated based on overlapping time series
            response = calculate_error(**inputs)
            if self.fc_round:
                response = {key: round(value, self.fc_round) for key, value in response.items()}
            return jsonify(response), 200

        except Exception as ex:
            msg = 'Error evaluation failed: ' + str(ex)
            logger.error(msg, ex)
            return jsonify({'msg': msg}), 500


def default():
    """
    Instructional message at the agent root.
    """

    msg = '<B>Forecasting agent</B>:<BR><BR>'
    msg += 'The Forecasting Agent can predict instantiated time series and instantiate the forecast in the KG.<BR>'
    msg += "The agent is implemented as derivation agent using ontoderivation:DerivationWithTimeSeries"
    msg += "<BR><BR>"
    msg += 'For further details please see the <a href="https://github.com/cambridge-cares/TheWorldAvatar/tree/main/Agents/ForecastingAgent/">Forecasting Agent README</a>.'
    return msg
