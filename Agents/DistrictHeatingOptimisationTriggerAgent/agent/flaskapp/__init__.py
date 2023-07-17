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
        sim_t = KB + 'SimulationTime_' + str(uuid.uuid4())
        # Optimisation interval
        opti_int = KB + 'OptimisationInterval_' + str(uuid.uuid4())
        opti_t1 = KB + 'OptimisationStartInstant_' + str(uuid.uuid4())
        opti_t2 = KB + 'OptimisationEndInstant_' + str(uuid.uuid4())
        opti_dt = params['mpcHorizon']*params['timeDelta_unix']
        # Heat demand & grid temperature data length
        heat_length = KB + 'HeatDemandDuration_' + str(uuid.uuid4())
        tmp_length = KB + 'GridTemperatureDuration_' + str(uuid.uuid4())
        # Forecast frequency
        freq = KB + 'Frequency_' + str(uuid.uuid4())

        for run in range(params['numberOfTimeSteps']):
            if run == 0:
                t1 = params['start']
                t2 = t1 + opti_dt
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

                # Instantiate derivation markups
                #TODO: to be implemented

                # Add time stamps to pure inputs
                derivation_client.addTimeInstanceCurrentTimestamp(
                    [sim_t, opti_int, heat_length, tmp_length, freq])

            else:
                t1 += params['timeDelta_unix']
                t2 += params['timeDelta_unix']
                # Update required time instances to trigger next optimisation run
                kg_client.update_time_instant(sim_t, t1)
                kg_client.update_time_instant(opti_t1, t1)
                kg_client.update_time_instant(opti_t2, t2)

                # Update time stamps of pure inputs
                derivation_client.updateTimestamps([sim_t, opti_int])

                # Request derivation update from Aermod Agent
                #TODO: to be implemented

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
    required_keys = ['start', 'mpcHorizon', 'numberOfTimeSteps', 'timeDelta', 
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
    _validate_int_parameter('mpcHorizon', params_dict['mpcHorizon'])
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