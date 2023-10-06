################################################
# Authors: Markus Hofmeister (mh807@cam.ac.uk) #    
# Date: 25 Sep 2023                            #
################################################

# The purpose of this module is to provide the DHOptimisationAgent class based on 
# the pyderivationagent.DerivationAgent class, i.e., implementing the district 
# heating generation optimisation agent as derivation agent using synchronous 
# derivation with time series

from rdflib import Graph, URIRef

from pyderivationagent import DerivationAgent
from pyderivationagent import DerivationInputs
from pyderivationagent import DerivationOutputs

from dhoptimisation.datamodel.iris import *
from dhoptimisation.agent.config import *
from dhoptimisation.kgutils.kgclient import KGClient
from dhoptimisation.kgutils.tsclient import TSClient
from dhoptimisation.agent.optimisation_tasks import *
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
            dictionary of ...
        """
        
        # Extract required optimisation inputs from derivation markup (i.e., map
        # retrieved derivation inputs to corresponding model input parameters)
        input_iris = {
        }

        # Map forecast instances to corresponding input parameters using further KG data

        return input_iris


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
              by the conventional gas boilers and the CHP gas turbine
            4 ohn:ConsumedGasAmount instances representing the amount of gas consumed
              by the conventional gas boilers and the CHP gas turbine

        NOTE: This is a minimal design in the sense than many more input parameters 
              required for the optimisation are queried from the KG (and instantiated
              back into the KG); however, not all are marked as inputs/outputs of the
              derivation and only the required subset to create the target derivation
              chain for the use case (forecast -> otpimise -> emission estimation -> aermod)
              are actually considered
        """

        #TODO: Decide on whether to round or not

        # Get input IRIs from the agent inputs (derivation_inputs)
        # (returns dict of inputs with input concepts as keys and values as list)
        inputs = derivation_inputs.getInputs()
        derivIRI = derivation_inputs.getDerivationIRI()

        # Get validated optimisation model inputs
        #TODO: skipped for now, to be properly implemented
        #input_iris = self.validate_input_values(inputs=inputs, derivationIRI=derivIRI)
        input_iris = inputs

        # Optimise heat generation
        #TODO: mocked for now; to be properly implemented
        # 1) Get optimisation interval bounds
        interval = self.sparql_client.get_interval_details(input_iris[TIME_INTERVAL])
        # 2) Get relevant time series settings from KG
        fc_details = self.sparql_client.get_input_forecast_details(input_iris[TS_FORECAST][0])
        # 3) Get potentially already instantiated optimisation output instances, i.e.,
        #    ProvidedHeat and ConsumedGas Amounts, which ts would just get updated
        outputs = self.sparql_client.get_optimisation_outputs(input_iris[TS_FORECAST][0])

        # Create optimisation
        rdb_url, time_format = get_rdb_endpoint(fc_details)
        ts_client = TSClient(kg_client=self.sparql_client, rdb_url=rdb_url, 
                             rdb_user=DB_USER, rdb_password=DB_PASSWORD)

        # Instantiate new optimisation outputs in KG and RDB (if not yet existing)
        if not outputs:
            # Mock
            # 1) retrieve 1 input time series
            # 2) initialise output forecast for same/scaled values
            # # Initialise forecast in KG
            # fc_iri = self.sparql_client.instantiate_forecast(forecast=fc_ts, config=cfg,
            #                                                  create_new=True)
            # # Create new forecast instance and add ts data in RDB
            # ts_client.init_timeseries(dataIRI=fc_iri, times=times, values=values, 
            #                           ts_type=cfg['ts_data_type'], time_format=time_format)

            # Add output graph to ensure complete derivation markup
            # --> this part of the code is only relevant when called via 
            # 'createSyncDerivationForNewInfo' and its only purpose is to ensure
            #  that forecast instance is marked up as "belongsTo" the derivation
            g = Graph()
            g.add((URIRef(fc_iri), URIRef(RDF_TYPE), URIRef(TS_FORECAST)))   
            derivation_outputs.addGraph(g)

        else:
            # Only update optimisation time series data in RDB
            # NOTE: Entire previous optimisation data is replaced, i.e., NOT just 
            #       appending new data and potentially overwriting existing data
            ts_client.replace_ts_data(dataIRI=outputs[0], times=times, values=values)
            #ts_client.add_ts_data(dataIRI=cfg['fc_iri'], times=times, values=values)
        
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
