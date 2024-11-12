################################################
# Authors: Markus Hofmeister (mh807@cam.ac.uk) #    
# Date: 25 Sep 2023                            #
################################################

# The purpose of this module is to provide the DHOptimisationAgent class based on 
# the pyderivationagent.DerivationAgent class, i.e., implementing the district 
# heating generation optimisation agent as derivation agent using synchronous 
# derivation with time series

from rdflib import Graph
from datetime import datetime
from flask import request, jsonify

from pyderivationagent import DerivationAgent
from pyderivationagent import DerivationInputs
from pyderivationagent import DerivationOutputs

from dhoptimisation.utils import *
from dhoptimisation.datamodel.iris import *
from dhoptimisation.agent.config import *
from dhoptimisation.agent.generator_models import *
from dhoptimisation.agent.optimisation_setup import *
from dhoptimisation.agent.optimisation_tasks import *
from dhoptimisation.agent.postprocessing import *
from dhoptimisation.kgutils.kgclient import KGClient
from dhoptimisation.kgutils.tsclient import TSClient
from dhoptimisation.utils.env_configs import DB_USER, DB_PASSWORD


class DHOptimisationAgent(DerivationAgent):

    def __init__(self, **kwargs):
        # Initialise DerivationAgent parent instance
        super().__init__(**kwargs)

        # Initialise the Sparql_client (with defaults specified in environment variables)
        self.sparql_client = self.get_sparql_client(KGClient)
        
        # Initialise previous state object
        # (i.e., to ensure link between related subsequent optimisation runs)
        self.previous_state = PreviousSystemState()
        

    def agent_input_concepts(self) -> list:
        # Please note: Declared inputs/outputs need proper instantiation incl. 
        #              RDF TYPE declarations in the KG for the derivation to work
        return [TS_FORECAST, TIME_INTERVAL]


    def agent_output_concepts(self) -> list:
        # Output concepts (i.e., results) of the Derivation
        return [OHN_PROVIDED_HEAT_AMOUNT, OHN_CONSUMED_GAS_AMOUNT,
                OHN_GENERATED_HEAT_AMOUNT,
                # less relevant, but also optimisation outputs
                OHN_COGEN_ELECTRICITY_AMOUNT, OHN_AVAILABILITY]


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
            t2 {str} -- optimisation end datetime as string
            ts_client {TSClient} -- configures time series client object
            time_format {str} -- Python compliant time format
        """
        
        print(f'Validating inputs for derivation {derivationIRI} ...')
        
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
        
        print('Inputs successfully validated.')

        return opti_inputs, t1, t2, ts_client, time_format


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

        # Get input IRIs from the agent inputs (derivation_inputs)
        # (returns dict of inputs with input concepts as keys and values as list)
        inputs = derivation_inputs.getInputs()
        derivIRI = derivation_inputs.getDerivationIRI()

        # Validate suitability of provided derivation inputs
        opti_inputs, opti_start_dt, opti_end_dt, ts_client, time_format = \
            self.validate_input_values(inputs=inputs, derivationIRI=derivIRI)

        # Get gas consumption and electricity co-generation models
        consumption_models, cogen_models = get_generator_models(self.sparql_client,
                                                                ts_client, opti_start_dt)

        ###  Optimise heat generation  ###
        # 1) Get potentially already instantiated optimisation output instances, i.e.,
        #    ProvidedHeat, ConsumedGas and GeneratedHeat Amounts, for which time series
        #    would just get updated (checks for actual forecast instances)
        print('Retrieving already instantiated optimisation output IRIs ...')
        rdf_types = self.agent_output_concepts()
        outputs = self.sparql_client.get_existing_optimisation_outputs(opti_inputs['q_demand'],
                                                                       rdf_types)
        
        # 2) Create optimisation input objects from KG data
        print('Creating optimisation input objects ...')
        # Query further inputs from KG and construct optimisation model setup dictionary
        setup_dict, index = define_optimisation_setup(self.sparql_client, ts_client,
                                                 consumption_models, cogen_models,
                                                 opti_inputs, opti_start_dt, opti_end_dt,
                                                 time_format)        
        # Create MarketPrices and MunicipalUtility objects for optimisation
        prices, swps = create_optimisation_setup(setup_dict)
        print('Optimisation input objects successfully created.')
        
        # 3) Optimize heat generation modes
        print('Optimising heat generation ...')
        clear_outputs = False
        logger.info(f'Current optimisation start time step: {opti_start_dt}')
        logger.info(f'Previous optimisation start time step: {self.previous_state.start_dt}')
        # Reset gas turbine etc. states for non-related optimisation runs
        if (datetime.strptime(opti_start_dt, time_format) - timedelta(hours=1)).strftime(time_format) != self.previous_state.start_dt:
            # Otherwise: If the previous optimisation interval covered [t1, t2] and
            # the current request covers [t1+1h, t3] -> runs are considered related
            # and the optimised state for t1 will be used as starting conditions
            print('Requested optimisation interval not related to previous optimisation. ' +
                  'Resetting system state prior to optimisation ...')
            self.previous_state.reset_system_state()
            clear_outputs = True
        # Run generation optimisation for entire optimisation horizon
        optimised, _, _ = generation_optimization(swps, prices, index, self.previous_state)
        print('Heat generation successfully optimised.')
        
        # 4) Instantiate (relevant) optimisation outputs
        # Extract timestamps for optimisation outputs to instantiate
        times = [datetime.strftime(dt, time_format) for dt in index]
        providers = swps.boilers + swps.gas_turbines + swps.contracts
        
        print('Instantiating/updating optimisation results ...')
        if not outputs:
            # Instantiate new optimisation outputs for all heat providers/generators
            # in KG and RDB (if not yet existing)
           
            # Initialise return Graph            
            g = Graph()
            
            for pro in providers:
                logger.info('Adding new optimisation outputs to return graph ...')
                # Get provider IRI and rdf type
                iri = pro.iri
                rdftype = self.sparql_client.get_rdftype(iri)
                
                # Retrieve optimisation output concept IRIs
                if rdftype == OHN_INCINERATIONPLANT:
                    outputs = self.sparql_client.get_efw_output_iris(iri)
                elif rdftype in [OHN_HEATBOILER, OHN_GASTURBINE]:
                    outputs = self.sparql_client.get_heatgenerator_output_iris(iri, 
                                gt=(rdftype == OHN_GASTURBINE))

                # Add new forecast IRIs for optimisation output data to return graph
                # NOTE: All optimisation outputs are instantiated as forecasts
                g, fc_iris = self.sparql_client.instantiate_new_outputs(g, outputs)
                
                logger.info('Instantiating associated output time series ...')
                # Extract optimisation output time series data
                ts_data = extract_output_timeseries(optimised, rdftype, pro)                
                # Initialise optimisation output time series
                for data_iri, data_values in ts_data.items():
                    if data_values:
                        ts_client.init_timeseries(dataIRI=fc_iris[data_iri], times=times, 
                                    values=data_values, time_format=time_format,
                                    ts_type=(DOUBLE if data_iri != OHN_AVAILABILITY else BOOLEAN))
                  
            # Add output graph to ensure complete derivation markup
            # --> this part of the code is only relevant when called via 
            # 'createSyncDerivationForNewInfo' and its only purpose is to ensure
            #  that forecast instance is marked up as "belongsTo" the derivation
            derivation_outputs.addGraph(g)

        else:
            # Only update optimisation time series data in RDB
            # NOTE: Entire previous optimisation data is replaced, i.e., NOT just 
            #       appending new data and potentially overwriting existing data

            logger.info('Overwriting previous optimisation time series data ...')
            for pro_iri, outp_dict in outputs.items():
                # Get corresponding provider object and rdf type
                pro = [p for p in providers if p.iri == pro_iri][0]
                rdftype = self.sparql_client.get_rdftype(pro_iri)

                # Extract optimisation output time series data
                ts_data = extract_output_timeseries(optimised, rdftype, pro)
                
                for out_type, iri in outp_dict.items():
                    # Skip irrelevant rdf types for current provider/generator
                    if iri is None:
                        continue
                    # Retrieve corresponding dataIRI
                    data_IRI, _ = self.sparql_client.get_associated_dataIRI(instance_iri=iri,
                                                                            forecast=True)
                    # Overwrite existing data with new optimisation outputs
                    ts_client.replace_ts_data(dataIRI=data_IRI, times=times, 
                                              values=ts_data.get(out_type))
        
        # Update instantiated current tier unit price (based on new incremental heat sourcing)
        for c in swps.contracts:
            logger.info(f'Updating current heat unit price for contract \"{c.name}\"')
            # Assess updated cumulative annual sourcing amount
            total = c.q_hist.sum(skipna=True)
            # Add optimised sourcing amount for current (first) time step
            total += optimised[c.name+'_q'].iloc[0]
            # Query applicable price tier for updated annual amount
            # NOTE: Returns None for amounts exceeding annual max supply -> no update anymore
            tier = self.sparql_client.get_price_tier_iri(c.iri, total)
            if tier:
                # Assign (potentially updated) price tier as current price
                self.sparql_client.update_current_heat_unit_price(c.iri, tier)
        print('Optimisation results successfully instantiated/updated.')
        
        print('Instantiating/updating total heat generation cost ...')
        # Extract DataFrame of optimised time series
        cols = [p.name+'_q' for p in providers] + ['Q_demand', 'Min_cost']
        generation_opt = optimised[cols].copy()
        generation_opt.rename(columns=lambda x: x.rstrip('_q'), inplace=True)
        # Create DataFrame of historical generation and cost
        generation_hist = get_historical_generation(providers, swps, prices,
                            self.sparql_client, ts_client, opti_start_dt, opti_end_dt)
        # Instantiate/update total generation cost
        instantiate_generation_cost(times, generation_hist, generation_opt,
                                    self.sparql_client, ts_client)
        print('Total heat generation cost successfully instantiated/updated.')
        
        # 5) Compare generation and cost (historical vs. optimised)
        print('Comparing historical vs. optimised generation (composition and cost) ...')
        if clear_outputs:
            # Delete previous output figures from non-related optimisation runs
            clear_repository()
        logger.info('Writing to consolidated output csv ...')
        create_optimised_csv_output(generation_opt)
        # Create plots
        self.logger.info('Creating output plots ...')
        # 1) plot comparison of heat generation/sourcing composition
        plot_entire_heat_generation(generation_hist, generation_opt, prices.el_spot)
        # 2) plot cost of non-optimized vs. optimised generation
        plot_generation_cost(generation_hist, generation_opt)
        # 3) plot forecast analysis for heat demand
        plot_forecast_quality(generation_hist['Q_demand'], generation_opt['Q_demand'])
        print('Comparison of optimised and historical generation completed.')

        # NOTE: DerivationWithTimeSeries does not return any output triples, 
        #       as all updates to the time series are expected to be conducted
        #       within the agent logic
        created_at = pd.to_datetime('now', utc=True)
        print(f'Created generation optimisation at: {created_at}')


    def compare_generation_cost(self):
        """
        This method evaluates the total cost benefit from the optimised and instantiated
        time interval, i.e., generation cost difference between historical generation
        and forecast/optimised generation for entire municipal utility
        
        The method expects an empty HTTP GET request.
        """
        
        try:
            # 1) Verify that empty body has been received
            if request.args:
                raise_error(ValueError, 'This method expects an empty GET request.')

            # 2) Compare generation cost for entire interval covered by optimisation
            # Retrieve relevant time series data IRIs
            cost = self.sparql_client.get_total_generation_cost()
            ts_client = TSClient(kg_client=self.sparql_client)
            
            # Retrieve entire optimised time series
            fc = ts_client.retrieve_timeseries_as_dataframe(cost.get('fc_data_iri'), 'fc')
            t1 = fc.index[0].strftime(TIME_FORMAT)
            t2 = fc.index[-1].strftime(TIME_FORMAT)
            # Retrieve historical data within same bounds
            hist = ts_client.retrieve_timeseries_as_dataframe(cost.get('hist_data_iri'), 
                                                'hist', lowerbound=t1, upperbound=t2)
            # Assess cost savings
            df = pd.concat([fc, hist], axis=1)
            df['saving'] = df['hist'] - df['fc']
            # Optimisation interval in days
            days = (df.index[-1] - df.index[0]).total_seconds()/(24*60*60)
            # Initialise return json
            response = {
                'Optimisation interval': 
                    {'Start': t1, 'End': t2 },
                'Heat generation cost (EUR)': 
                    {'Historical generation': round(df['hist'].sum(), 2),
                     'Optimised generation': round(df['fc'].sum(), 2),
                     'Total savings': round(df['saving'].sum(), 2),
                     'Average savings per day': round(df['saving'].sum() / days, 2),
                    }
            }

            return jsonify(response), 200

        except Exception as ex:
            msg = 'Cost saving evaluation failed: ' + str(ex)
            logger.error(msg)
            return jsonify({'msg': msg}), 500


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
