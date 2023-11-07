################################################
# Authors: Markus Hofmeister (mh807@cam.ac.uk) #    
# Date: 25 Sep 2023                            #
################################################

# The purpose of this module is to provide the DHOptimisationAgent class based on 
# the pyderivationagent.DerivationAgent class, i.e., implementing the district 
# heating generation optimisation agent as derivation agent using synchronous 
# derivation with time series

from datetime import datetime
from rdflib import Graph, URIRef

from pyderivationagent import DerivationAgent
from pyderivationagent import DerivationInputs
from pyderivationagent import DerivationOutputs

from dhoptimisation.utils import *
from dhoptimisation.datamodel.iris import *
from dhoptimisation.agent.config import *
from dhoptimisation.agent.generator_models import *
from dhoptimisation.agent.optimisation_tasks import *
from dhoptimisation.kgutils.kgclient import KGClient
from dhoptimisation.kgutils.tsclient import TSClient
from dhoptimisation.utils.env_configs import DB_USER, DB_PASSWORD


class DHOptimisationAgent(DerivationAgent):

    def __init__(self, **kwargs):
        # Initialise DerivationAgent parent instance
        super().__init__(**kwargs)

        # Initialise the Sparql_client (with defaults specified in environment variables)
        self.sparql_client = self.get_sparql_client(KGClient)
        

    def agent_input_concepts(self) -> list:
        # Please note: Declared inputs/outputs need proper instantiation incl. 
        #              RDF TYPE declarations in the KG for the derivation to work
        return [TS_FORECAST, TIME_INTERVAL]


    def agent_output_concepts(self) -> list:
        # Output concepts (i.e., results) of the Derivation
        return [OHN_PROVIDED_HEAT_AMOUNT, OHN_CONSUMED_GAS_AMOUNT,
                OHN_GENERATED_HEAT_AMOUNT]


    def validate_inputs(self, http_request) -> bool:
        # Validate completeness of received HTTP request (i.e. non-empty HTTP request, 
        # contains derivationIRI, etc.) -> only relevant for synchronous derivation
        return super().validate_inputs(http_request)


    def validate_input_values(self, inputs, derivationIRI=None):
        """
        Check whether received input instances are suitable to optimise heat generation.
        Throw exception if data is not suitable.

        Arguments:
            inputs {dict} -- Dictionary of inputs with input concepts as keys and values as list
            derivationIRI {str} -- IRI of the derivation instance (optional)

        Returns:
            opti_inputs {dict} -- optimisation model inputs with keys 'interval',
                                  'q_demand', 't_flow_efw', 't_return_efw', 't_flow_mu',
                                  't_return_mu' and corresponding IRI values as string
            t1 {str} -- optimisation start datetime as string
            ts_client {TSClient} -- configures time series client object
            time_format {str} -- Python compliant time format
        """
        
        # Initialise dict of return values
        opti_inputs = {}
        
        # 1) Verify that exactly one (optimisation) time:Interval instance is provided
        # Check whether input is available
        if not inputs.get(TIME_INTERVAL):
            raise_error(TypeError, f"Derivation {derivationIRI}: No 'time:Interval' IRI provided.")
        else:
            inp = inputs.get(TIME_INTERVAL)
            # Check whether only one input has been provided
            if len(inp) == 1:
                opti_inputs['interval'] = inp[0]
            else:
                raise_error(TypeError, f"Derivation {derivationIRI}: More than one 'time:Interval' IRI provided.")
       
        # 2) Verify that exactly one forecast for heat demand and each grid temperature
        #    is provided, i.e., map forecast instances to corresponding input parameters
        if not inputs.get(TS_FORECAST):
            raise_error(TypeError, f"Derivation {derivationIRI}: No 'ts:Forecast' IRI provided.")
        else:
            fc_mapping = self.sparql_client.get_input_types_from_forecast_iris(inputs[TS_FORECAST])
            # Throw exception in case any could not be retrieved (i.e., is None)
            for key, value in fc_mapping.items():
                if value is None:
                    raise_error(ValueError, f"Derivation {derivationIRI}: No forecast provided for '{key}'.")
        
        # 3) Verify that all forecast time series cover required optimisation interval
        # Retrieve optimisation interval bounds
        interval = self.sparql_client.get_interval_details(opti_inputs['interval'])
        # Retrieve relevant time series settings from KG
        # NOTE: This assumes that all time series have same rdb url and time format
        ts_settings = self.sparql_client.get_input_forecast_details(fc_mapping['q_demand'])
        rdb_url, time_format = get_rdb_endpoint(ts_settings)
        ts_client = TSClient(kg_client=self.sparql_client, rdb_url=rdb_url, 
                             rdb_user=DB_USER, rdb_password=DB_PASSWORD)
        # Create datetime strings from interval timestamps
        t1 = datetime.utcfromtimestamp(interval['start_unix']).strftime(time_format)
        t2 = datetime.utcfromtimestamp(interval['end_unix']).strftime(time_format)
        for key, value in fc_mapping.items():
            times, _ = ts_client.retrieve_timeseries(value)
            # Verify that retrieved times contain interval bounds
            if not (t1 in times) or not (t2 in times):
                raise_error(ValueError, f"Derivation {derivationIRI}: Time series data for " \
                                       +f"'{key}' does not cover full optimisation interval.")
        
        # Otherwise append mapped IRIs to return dict
        opti_inputs.update(fc_mapping)

        return opti_inputs, t1, ts_client, time_format


    def process_request_parameters(self, derivation_inputs: DerivationInputs, 
                                   derivation_outputs: DerivationOutputs):
        """
        This method takes 
            multiple ts:Forecast instances, representing 1 forecasted heat demand
              and 4 grid temperatures (i.e, flow and return temperature at municipal
              utility and energy from waste plant)
            1 time:Interval instance, representing the optimisation horizon
        and generates
            1 ohn:ProvidedHeatAmount instance representing the amount of heat provided
              by the energy from waste plant
            4 ohn:GeneratedHeatAmount instances representing the amount of heat generated
              by three conventional gas boilers and the CHP gas turbine
            4 ohn:ConsumedGasAmount instances representing the amount of gas consumed
              by three conventional gas boilers and the CHP gas turbine

        NOTE: This is a minimal design in the sense than many more input parameters 
              required for the optimisation are queried from the KG (and instantiated
              back into the KG); however, not all are marked as inputs/outputs of the
              derivation and only the required subset to create the target derivation
              chain for the use case (forecast -> optimise -> emission estimation -> aermod)
              are actually considered
        """

        #TODO: Decide on whether to round or not

        # Get input IRIs from the agent inputs (derivation_inputs)
        # (returns dict of inputs with input concepts as keys and values as list)
        inputs = derivation_inputs.getInputs()
        derivIRI = derivation_inputs.getDerivationIRI()

        # Get validated optimisation model inputs
        opti_inputs, opti_start_dt, ts_client, time_format = \
            self.validate_input_values(inputs=inputs, derivationIRI=derivIRI)

        # Get gas consumption and electricity co-generation models
        consumption_models, cogen_models = get_generator_models(self.sparql_client,
                                                                ts_client, opti_start_dt)

        # Optimise heat generation
        #TODO: mocked for now; to be properly implemented
        # 3) Get potentially already instantiated optimisation output instances, i.e.,
        #    ProvidedHeat and ConsumedGas Amounts, which ts would just get updated
        #    (checks for actual forecast instances)
        outputs = self.sparql_client.get_optimisation_outputs(opti_inputs['q_demand'])
       
        # Mock optimisation data
        # 1) retrieve 1 input time series
        times, values = ts_client.retrieve_timeseries(opti_inputs['q_demand'])
        # 2) initialise output forecast for "random" values
        import random
        provided_heat = [round(random.uniform(2.0, 11.0),1) for _ in times]
        consumed_gas = [round(random.uniform(1.0, 6.0),1) for _ in times]

        # Instantiate new optimisation outputs in KG and RDB (if not yet existing)
        if not outputs:
            # Initialise return Graph
            g = Graph()
            providers = self.sparql_client.get_heat_providers()
            # efw plant
            efw_outputs = self.sparql_client.get_efw_output_iris(providers['efw_plant'][0])
            g, efw_ts = self.sparql_client.instantiate_new_outputs(g, efw_outputs)
            # gas boiler
            boiler_outputs = self.sparql_client.get_heatgenerator_output_iris(providers['boilers'][0])
            g, boiler_ts = self.sparql_client.instantiate_new_outputs(g, boiler_outputs)
            
            # Initialise time series
            ts_client.init_timeseries(dataIRI=efw_ts[OHN_PROVIDED_HEAT_AMOUNT], 
                                      times=times, values=provided_heat,
                                      ts_type=DOUBLE,
                                      time_format=time_format)
            ts_client.init_timeseries(dataIRI=boiler_ts[OHN_CONSUMED_GAS_AMOUNT], 
                                      times=times, values=consumed_gas, 
                                      ts_type=DOUBLE,
                                      time_format=time_format)

            # Add output graph to ensure complete derivation markup
            # --> this part of the code is only relevant when called via 
            # 'createSyncDerivationForNewInfo' and its only purpose is to ensure
            #  that forecast instance is marked up as "belongsTo" the derivation
            derivation_outputs.addGraph(g)

        else:
            # Only update optimisation time series data in RDB
            # NOTE: Entire previous optimisation data is replaced, i.e., NOT just 
            #       appending new data and potentially overwriting existing data
            # efw plant
            data_IRI, _ = self.sparql_client.get_associated_dataIRI(instance_iri=outputs[OHN_PROVIDED_HEAT_AMOUNT][0],
                                                                    unit=None, forecast=True)
            ts_client.replace_ts_data(dataIRI=data_IRI, 
                                      times=times, values=provided_heat)
            # gas boiler
            data_IRI, _ = self.sparql_client.get_associated_dataIRI(instance_iri=outputs[OHN_CONSUMED_GAS_AMOUNT][0],
                                                                    unit=None, forecast=True)
            ts_client.replace_ts_data(dataIRI=data_IRI, 
                                      times=times, values=consumed_gas)
        
        created_at = pd.to_datetime('now', utc=True)
        logger.info(f'Created generation optimisation at: {created_at}')

        # NOTE: DerivationWithTimeSeries does not return any output triples, 
        #       as all updates to the time series are expected to be conducted
        #       within the agent logic 
        

def default():
    """
    Instructional message at the agent root.
    """

    msg = '<B>District Heating Optimisation Agent</B>:<BR><BR>'
    msg += 'This district heating optimisation agent is used to optimise the total heat generation cost '
    msg += 'for the Pirmasens municipal utility company by solving the economic dispatch problem including '
    msg += 'an energy-from-waste plant, a combined heat and power gas turbine, and a set of conventional '
    msg += 'gas boilers in an MPC-like fashion. <BR>'
    msg += "The agent is implemented as derivation agent using ontoderivation:DerivationWithTimeSeries"
    msg += "<BR><BR>"
    msg += 'For further details please see the <a href="https://github.com/cambridge-cares/TheWorldAvatar/tree/main/Agents/DistrictHeatingOptimisationAgent/">District Heating Optimisation Agent README</a>.'
    return msg
