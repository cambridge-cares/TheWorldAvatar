################################################
# Authors: Markus Hofmeister (mh807@cam.ac.uk) #    
# Date: 11 Jul 2022                            #
################################################

# Create Flask application and define HTTP route to initiate district heating
# optimisation by derivation agents and trigger subsequent runs by updating inputs

import uuid
from celery import Celery
from datetime import datetime, timedelta
from flask import Flask, request, jsonify

from py4jps import agentlogging
from pyderivationagent import PyDerivationClient

from agent.datamodel import *
from agent.kgutils.kgclient import KGClient
from agent.utils.agent_configs import QUERY_ENDPOINT, UPDATE_ENDPOINT, \
                                      FORECASTING_AGENT, DH_OPTIMISATION_AGENT, \
                                      EMISSION_ESTIMATION_AGENT

# Initialise logger instance (ensure consistent logger level`)
logger = agentlogging.get_logger('prod')


# Create and start Flask app
app = Flask(__name__)

# Import triples from 'resources' folder upon app startup
logger.info("Importing triples from 'resources/triples' folder...")
kg_client = KGClient(QUERY_ENDPOINT, UPDATE_ENDPOINT)
kg_client.initialise_namespace('./resources/triples')
logger.info("Successfully imported triples.")

# Add covariate relationships to uploaded forecasting model instances
# TODO: Add covariate relationships for grid temperatures if necessary
logger.info("Adding covariate relationships to relevant forecasting models...")
if kg_client.check_if_triple_exist(fc_model_heat_demand, RDF_TYPE, TS_FORECASTINGMODEL):
    # Get ambient air temperature and public holiday covariate IRIs
    # NOTE: assumes exactly one instance of each type in the KG, i.e.,
    #       ideally instantiate district heating in separate namespace
    temp, holi = kg_client.get_heat_demand_covariates()
    # Add covariate relationships
    kg_client.instantiate_covariate_relationships(fc_model_heat_demand, [temp, holi])
    logger.info("Covariate relationships successfully added.")
else:
    msg = 'Heat demand forecasting model instance not found in KG. '
    msg += 'Please ensure it is instantiated and properly referenced in iris.py'
    logger.error(msg)
    raise ValueError(msg)


# Launch celery task queue
celery = Celery(app.name, broker='redis://localhost:6379/0')


@app.route('/triggerOptimisation', methods=['POST'])
def trigger_optimisation():
    # Check if previous optimisation task is still running
    if is_processing_task_running():
            return jsonify(message='Previous request is not finished yet. Please try again later.'), 423
    
    # Otherwise, initiate optimisation task
    try:
        # Verify received HTTP request parameters
        params = request.get_json()
        params = validate_input_params(params)

        # Queue the optimisation task
        #NOTE: For debugging please switch (un-)commented in next two lines
        #trigger_optimisation_task(params)
        task_id = trigger_optimisation_task.apply_async(args=[params])

        return jsonify(message=f'District heating optimisation task started with ID: {task_id}'), 200
    
    except Exception:
        # Log the exception
        logger.error("An error occurred during optimisation.", exc_info=True)

        # Return an error response
        return jsonify(message='An error occurred during optimisation. See agent log for details.'), 500


#NOTE: For debugging please comment next line
@celery.task
def trigger_optimisation_task(params):
    try:
        #TODO: Sequence to be refactored/streamlined
        # Initialise sparql and derivation clients
        kg_client = KGClient(query_endpoint=QUERY_ENDPOINT, update_endpoint=UPDATE_ENDPOINT)
        derivation_client = PyDerivationClient(
            derivation_instance_base_url=DERIVATION_INSTANCE_BASE_URL,
            query_endpoint=QUERY_ENDPOINT, update_endpoint=UPDATE_ENDPOINT)
        
        # Check for already instantiated chain of derivations (to be reused)
        # Heat demand and grid temp forecast derivation IRIs; returns empty list if not exist
        fc_deriv_iris = kg_client.get_forecast_derivations()
        # Downstream optimisation and emission derivation IRIs; returns None if not exist
        opti_deriv_iri = [] if not fc_deriv_iris else \
                            kg_client.get_downstream_derivation(fc_deriv_iris[0])
        if len(opti_deriv_iri) > 1:
            msg = f'More than 1 Generation optimisation derivation retrieved: {", ".join(opti_deriv_iri)}'
            logger.error(msg)
            raise ValueError(msg)
        em_deriv_iri = [] if not opti_deriv_iri else \
                        kg_client.get_downstream_derivation(opti_deriv_iri[0])
        if len(em_deriv_iri) > 2:
            msg = f'More than 2 Emission estimation derivation retrieved: {", ".join(em_deriv_iri)}'
            logger.error(msg)
            raise ValueError(msg)
        derivs = [d for d in (opti_deriv_iri, em_deriv_iri) if d is not None]
        try:
            # Retrieve unique time inputs (optimisation interval, simulation time)
            # attached to retrieved derivations; throws Exception if any does not exists
            sim_t, opti_int, opti_t1, opti_t2 = kg_client.get_pure_trigger_inputs(derivs)
            logger.info('Derivation chain already instantiated.')
            new_derivation = False
        except:
            new_derivation = True
            logger.info('Instantiating new chain of derivations...')
            # Create IRIs for time inputs to instantiate
            # Simulation time
            sim_t = KB + 'SimulationTime_' + str(uuid.uuid4())
            # Optimisation interval
            opti_int = KB + 'OptimisationInterval_' + str(uuid.uuid4())
            opti_t1 = KB + 'OptimisationStartInstant_' + str(uuid.uuid4())
            opti_t2 = KB + 'OptimisationEndInstant_' + str(uuid.uuid4())
            # Heat demand & grid temperature data length
            heat_length = KB + 'HeatDemandDuration_' + str(uuid.uuid4())
            tmp_length = KB + 'GridTemperatureDuration_' + str(uuid.uuid4())
            # Forecast frequency
            freq = KB + 'Frequency_' + str(uuid.uuid4())

        # Trigger derivation update for each time step to optimise
        for run in range(params['numberOfTimeSteps']):
            if run == 0:
                # Initialise optimisation/forecast interval
                opti_dt = params['optHorizon']*params['timeDelta_unix']
                t1 = params['start']                
                t2 = t1 + opti_dt
                if new_derivation:
                    logger.info('Instantiating optimisation time etc. pure inputs...')
                    # Instantiate required time instances to initiate optimisation cascades
                    kg_client.instantiate_time_instant(sim_t, t1, instance_type=OD_SIMULATION_TIME) 
                    kg_client.instantiate_time_interval(opti_int, opti_t1, opti_t2, t1, t2)
                    # Instantiate required durations
                    kg_client.instantiate_time_duration(heat_length, params['timeDelta'], 
                                                        params['heatDemandDataLength'])
                    kg_client.instantiate_time_duration(tmp_length, params['timeDelta'], 
                                                        params['gridTemperatureDataLength'])
                    # Instantiate required frequency for forecasting agent
                    kg_client.instantiate_time_duration(freq, params['timeDelta'], 
                                                        value=1, rdf_type=TS_FREQUENCY)
                    # Add time stamps to pure inputs
                    derivation_client.addTimeInstanceCurrentTimestamp(
                        [sim_t, opti_int, heat_length, tmp_length, freq])
                    logger.info('Instantiation of optimisation time etc. successfully finished.')
                else:
                    # Update time instances (pre-existing from previous optimisation)
                    kg_client.update_time_instant(sim_t, t1)
                    kg_client.update_time_instant(opti_t1, t1)
                    kg_client.update_time_instant(opti_t2, t2)
                
                ###   Instantiate derivation markups   ###
                # 1) Forecast derivations
                #    1) Heat demand
                if not fc_deriv_iris:
                    heat_demand = kg_client.get_heat_demand()
                    inputs_demand = [heat_demand, fc_model_heat_demand, opti_int, freq, heat_length]
                    deriv = derivation_client.createSyncDerivationForNewInfo(FORECASTING_AGENT, 
                                    inputs_demand, ONTODERIVATION_DERIVATIONWITHTIMESERIES)
                    logger.info(f"Heat demand forecast derivation successfully instantiated: {deriv.getIri()}")
                    # Add to list of all forecast derivation IRIs
                    fc_deriv_iris.append(deriv.getIri())
                    #    2) Grid temperatures
                    grid_temps = kg_client.get_grid_temperatures()
                    deriv_base = [fc_model_grid_temperature, opti_int, freq, tmp_length]
                    inputs_temps = [deriv_base + [t] for t in grid_temps]
                    for i in inputs_temps:
                        deriv = derivation_client.createSyncDerivationForNewInfo(FORECASTING_AGENT, 
                                    i, ONTODERIVATION_DERIVATIONWITHTIMESERIES)
                        logger.info(f"Grid temperature forecast derivation successfully instantiated: {deriv.getIri()}")
                        fc_deriv_iris.append(deriv.getIri())
                else:
                    for d in fc_deriv_iris:
                        derivation_client.unifiedUpdateDerivation(d)
                        logger.info(f"Forecast derivation instance successfully updated: {d}")

                # 2) Optimisation derivation
                if not opti_deriv_iri:
                    # NOTE: Instantiated using "createSyncDerivationForNewInfo" for same
                    #       reason as above, i.e., ensure initial generation of output triples
                    # Get all forecast derivation outputs
                    fc_outputs = kg_client.get_derivation_outputs(fc_deriv_iris)
                    # Extract all created forecast instances and create list of optimisation inputs
                    inputs_opti = list(fc_outputs[TS_FORECAST]) + [opti_int]
                    deriv = derivation_client.createSyncDerivationForNewInfo(DH_OPTIMISATION_AGENT, 
                                    inputs_opti, ONTODERIVATION_DERIVATIONWITHTIMESERIES)
                    opti_deriv_iri.append(deriv.getIri())
                    logger.info(f"Generation optimisation derivation successfully instantiated: {opti_deriv_iri[0]}")
                else:
                    derivation_client.unifiedUpdateDerivation(opti_deriv_iri[0])
                    logger.info(f"Generation optimisation derivation instance successfully updated: {opti_deriv_iri[0]}")

                # 3) Emission estimation derivations
                if not em_deriv_iri:
                    # Query Point Sources associated with emissions (instances need to have
                    # a disp:hasOntoCityGMLCityObject relationship attached for Aermod to work)

                    # Get all optimisation derivation outputs
                    opti_outputs = kg_client.get_derivation_outputs([opti_deriv_iri])
                    # Extract all created forecast instances and create list of optimisation inputs
                    #    1) EfW emissions (ProvidedHeatAmount)
                    inputs_efw_em = list(opti_outputs[OHN_PROVIDED_HEAT_AMOUNT]) + [sim_t, point_source_efw]
                    deriv = derivation_client.createSyncDerivationForNewInfo(EMISSION_ESTIMATION_AGENT, 
                                                inputs_efw_em, ONTODERIVATION_DERIVATION)
                    logger.info(f"EfW emission estimation derivation successfully instantiated: {deriv.getIri()}")
                    #    2) heating plant emissions (ConsumedGasAmount)
                    inputs_mu_em = list(opti_outputs[OHN_CONSUMED_GAS_AMOUNT]) + [sim_t, point_source_mu]
                    deriv = derivation_client.createSyncDerivationForNewInfo(EMISSION_ESTIMATION_AGENT, 
                                                inputs_mu_em, ONTODERIVATION_DERIVATION)
                    logger.info(f"Municipal utility emission estimation derivation successfully instantiated: {deriv.getIri()}")
                else:
                    for d in em_deriv_iri:
                        derivation_client.unifiedUpdateDerivation(d)
                        logger.info(f"Emission estimation derivation instance successfully updated: {d}")

                # 4) Initialise Aermod dispersion derivation markup (i.e., for 
                #    existing SimulationTime instance)
                #TODO: Likely by sending POST request to some agent in Aermod suite,
                #      which shall return dispersion derivation iri
                #dis_deriv_iri=

            else:
                t1 += params['timeDelta_unix']
                t2 += params['timeDelta_unix']
                # Update required time instances to trigger next optimisation run
                kg_client.update_time_instant(sim_t, t1)
                kg_client.update_time_instant(opti_t1, t1)
                kg_client.update_time_instant(opti_t2, t2)
                # Update time stamps of pure inputs
                derivation_client.updateTimestamps([sim_t, opti_int])

            # Request derivation update via Aermod Agent
            #TODO: to be verified
            #derivation_client.unifiedUpdateDerivation(dis_deriv_iri)
            # NOTE: Aermod Agent queries emission derivations via StaticPointSources 
            # and requests update; all other derivation updates are handled by DIF 
            # directly as derivationscare directly linked via I/O relations in KG            

            # Print progress (to ensure output to console even for async tasks)
            print(f"Optimisation run {run+1}/{params['numberOfTimeSteps']} completed.")
            print(f"Current optimisation time: {t1}")

        print("Optimisation completed successfully.")

    except Exception:
        # Log the exception
        logger.error("An error occurred during optimisation.", exc_info=True)


def is_processing_task_running():
    # Return True if any 'perform_optimisation_task' is currently running
    inspect = celery.control.inspect()
    active_tasks = inspect.active()
    if any(active_tasks.values()):
        active_task_names = [v[0].get('name') for v in active_tasks.values()]
        return any('trigger_optimisation_task' in item for item in active_task_names)
    return False


def validate_input_params(params_dict):
    """
    Validate suitability of received HTTP request parameters (converted to dict)
    """

    def _validate_int_parameter(param_name, param_value):
        if not isinstance(param_value, int) or param_value <= 0:
            raise ValueError(f"{param_name} should be an integer greater than 0")

    # Check if all required keys are present
    required_keys = ['start', 'optHorizon', 'numberOfTimeSteps', 'timeDelta', 
                     'heatDemandDataLength', 'gridTemperatureDataLength']
    for key in required_keys:
        if key not in params_dict:
            raise ValueError(f"Missing required parameter: {key}")

    # Validate startDateTime
    start_date_time = params_dict['start']
    try:
        datetime.strptime(start_date_time, '%Y-%m-%dT%H:%M:%SZ')
    except ValueError:
        raise ValueError("Invalid start dateTime format. Expected format: YYYY-MM-DDThh:mm:ssZ")

    # Convert startDateTime to unix timestamp if needed
    unix_timestamp = datetime.timestamp(datetime.strptime(start_date_time, '%Y-%m-%dT%H:%M:%SZ'))
    params_dict['start'] = int(unix_timestamp)

    # Validate integer parameters
    _validate_int_parameter('optHorizon', params_dict['optHorizon'])
    _validate_int_parameter('numberOfTimeSteps', params_dict['numberOfTimeSteps'])
    _validate_int_parameter('heatDemandDataLength', params_dict['heatDemandDataLength'])
    _validate_int_parameter('gridTemperatureDataLength', params_dict['gridTemperatureDataLength'])

    # Validate frequency and map to constants
    valid_frequencies = {
        "day": (TIME_UNIT_DAY, int(timedelta(days=1).total_seconds())),
        "hour": (TIME_UNIT_HOUR, int(timedelta(hours=1).total_seconds())),
        "minute": (TIME_UNIT_MINUTE, int(timedelta(minutes=1).total_seconds())),
        "second": (TIME_UNIT_SECOND, int(timedelta(seconds=1).total_seconds()))
    }

    frequency = params_dict['timeDelta']
    if frequency not in valid_frequencies:
        raise ValueError("Invalid timeDelta. Valid options: 'day', 'hour', 'minute', 'second'")
    params_dict['timeDelta'] = valid_frequencies[frequency][0]
    params_dict['timeDelta_unix'] = valid_frequencies[frequency][1]

    return params_dict


if __name__ == "__main__":
    # Start the app
    app.run(host='localhost', port="5000")
    logger.info('App started')