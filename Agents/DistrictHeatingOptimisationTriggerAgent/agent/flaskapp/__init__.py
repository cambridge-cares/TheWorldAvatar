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
from agent.utils.connection_configs import QUERY_ENDPOINT, UPDATE_ENDPOINT

# Initialise logger instance (ensure consistent logger level`)
logger = agentlogging.get_logger('prod')


# Create Flask app and incorporate Celery task queue
app = Flask(__name__)
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
        task_id = trigger_optimisation_task.apply_async(args=[params])

        return jsonify(message=f'District heating optimisation task started with ID: {task_id}'), 200
    
    except Exception:
        # Log the exception
        logger.error("An error occurred during optimisation.", exc_info=True)

        # Return an error response
        return jsonify(message='An error occurred during optimisation. See agent log for details.'), 500


@celery.task
def trigger_optimisation_task(params):
    try:
        # Initialise KG and derivation clients
        kg_client = KGClient(query_endpoint=QUERY_ENDPOINT, update_endpoint=UPDATE_ENDPOINT)
        derivation_client = PyDerivationClient(
            derivation_instance_base_url=DERIVATION_INSTANCE_BASE_URL,
            query_endpoint=QUERY_ENDPOINT, update_endpoint=UPDATE_ENDPOINT)

        # Create Instance IRIs 
        # Simulation time
        sim_t = KB + 'SimulationTimeInstant_' + str(uuid.uuid4())
        # Optimisation interval
        opti_int = KB + 'OptimisationInterval_' + str(uuid.uuid4())
        opti_t1 = KB + 'OptimisationTimeInstant_' + str(uuid.uuid4())
        opti_t2 = KB + 'OptimisationTimeInstant_' + str(uuid.uuid4())
        opti_dt = params['numberOfTimeSteps']*params['timeDelta']
        # Heat demand data length
        heat_int = KB + 'HeatDemandInterval_' + str(uuid.uuid4())
        heat_t1 = KB + 'HeatDemandTimeInstant_' + str(uuid.uuid4())
        heat_t2 = KB + 'HeatDemandTimeInstant_' + str(uuid.uuid4())
        heat_dt = params['heatDemandDataLength']*params['timeDelta']
        # Grid temperature data length
        tmp_int = KB + 'GridTemperatureInterval_' + str(uuid.uuid4())
        tmp_t1 = KB + 'GridTemperatureTimeInstant_' + str(uuid.uuid4())
        tmp_t2 = KB + 'GridTemperatureTimeInstant_' + str(uuid.uuid4())
        tmp_dt = params['gridTemperatureDataLength']*params['timeDelta']

        for run in range(params['numberOfTimeSteps']):
            if run == 0:
                t1 = params['start']
                t2 = t1 + opti_dt
                # Instantiate required time instances to initiate optimisation cascades
                kg_client.instantiate_time_instance(sim_t, t1, instance_type=OD_SIMULATION_TIME) 
                kg_client.instantiate_time_interval(opti_int, opti_t1, opti_t2, t1, t2)
                # Instantiate required time instances for Forecasting Agent input
                kg_client.instantiate_time_interval(heat_int, heat_t1, heat_t2, t1-heat_dt, t1)
                kg_client.instantiate_time_interval(tmp_int, tmp_t1, tmp_t2, t1-tmp_dt, t1)

                # Instantiate derivation markups
                #TODO: to be implemented

                # Add time stamps to pure inputs
                derivation_client.addTimeInstanceCurrentTimestamp([sim_t, opti_int, heat_int, tmp_int])

            else:
                t1 += params['timeDelta']
                t2 += params['timeDelta']
                # Update required time instances to trigger next optimisation run
                kg_client.update_time_instance(sim_t, t1)
                kg_client.update_time_instance(opti_t1, t1)
                kg_client.update_time_instance(opti_t2, t2)
                # Update data histories for Forecasting Agent input
                kg_client.update_time_instance(heat_t1, t1-heat_dt)
                kg_client.update_time_instance(heat_t2, t1)
                kg_client.update_time_instance(tmp_t1, t1-tmp_dt)
                kg_client.update_time_instance(tmp_t2, t1)

                # Update time stamps of pure inputs
                derivation_client.updateTimestamps([sim_t, opti_int, heat_int, tmp_int])

                # Request derivation update from Aermod Agent
                #TODO: to be implemented

            # Print progress (to ensure output to console even for async tasks)
            print(f"Optimisation run {run+1}/{params['numberOfTimeSteps']} completed.")
            print(f"Current time: {t1}")

    except Exception:
        # Log the exception
        logger.error("An error occurred during optimisation.", exc_info=True)

        # Return an error response
        return jsonify(message='An error occurred during optimisation. See agent log for details.'), 500


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

    # Check if all required keys are present
    required_keys = ['start', 'numberOfTimeSteps', 'timeDelta', 'heatDemandDataLength', 'gridTemperatureDataLength']
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

    # Validate numberOfTimeSteps
    num_time_steps = params_dict['numberOfTimeSteps']
    if not isinstance(num_time_steps, int) or num_time_steps <= 0:
        raise ValueError("numberOfTimeSteps should be an integer greater than 0")

    # Validate frequency and map to constants
    frequency = params_dict['timeDelta']
    valid_frequencies = {
        'day': int(timedelta(days=1).total_seconds()),
        'hour': int(timedelta(hours=1).total_seconds()),
        'minute': int(timedelta(minutes=1).total_seconds()),
        'second': int(timedelta(seconds=1).total_seconds())
    }
    if frequency not in valid_frequencies:
        raise ValueError("Invalid timeDelta. Valid options: 'day', 'hour', 'minute', 'second'")
    params_dict['timeDelta'] = valid_frequencies[frequency]

    # Validate heatDemandDataLength
    heat_data_length = params_dict['heatDemandDataLength']
    if not isinstance(heat_data_length, int) or heat_data_length <= 0:
        raise ValueError("heatDemandDataLength should be an integer greater than 0")

    # Validate gridTemperatureDataLength
    grid_data_length = params_dict['gridTemperatureDataLength']
    if not isinstance(grid_data_length, int) or grid_data_length <= 0:
        raise ValueError("gridTemperatureDataLength should be an integer greater than 0")

    return params_dict


if __name__ == "__main__":
    # Start the app
    app.run(host='localhost', port="5000")
    logger.info('App started')