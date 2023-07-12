# Create Flask application and define HTTP route

import re
import uuid
from datetime import datetime, timedelta
from flask import Flask, request, jsonify
import threading

from py4jps import agentlogging

from agent.datamodel import *
from agent.kgutils.kgclient import KGClient
from agent.utils.connection_configs import QUERY_ENDPOINT, UPDATE_ENDPOINT

# Initialise logger instance (ensure consistent logger level`)
logger = agentlogging.get_logger('prod')


def create_app(test_config=None):
    """
    Create and configure an instance of the Flask application.
    """
    app = Flask(__name__, instance_relative_config=True)
    app.config['JSON_SORT_KEYS'] = False
    if test_config is not None:
        # Load the test config if passed in
        app.config.update(test_config)

    # Ensure that only one request is processed at a time
    lock = threading.Lock()
    global processing
    processing = False

    @app.route('/triggerOptimisation', methods=['POST'])
    def trigger_optimisation():

        global processing

        if processing:
            # Return HTTP 423 (Locked) if previous request is still being processed
            return jsonify(message='Previous request is not finished yet. Please try again later.'), 423

        # Start processing the current request
        with lock:
            processing = True

            # Verify received HTTP request parameters
            params = request.get_json()
            params = validate_input_params(params)

            # Initialise KG client
            kg_client = KGClient(query_endpoint=QUERY_ENDPOINT,
                                 update_endpoint=UPDATE_ENDPOINT)

            # Create Instance IRIs 
            # Optimisation interval
            opti_int = KB + 'OptimisationInterval_' + str(uuid.uuid4())
            opti_t1 = KB + 'OptimisationTimeInstant_' + str(uuid.uuid4())
            opti_t2 = KB + 'OptimisationTimeInstant_' + str(uuid.uuid4())
            # Heat demand data length
            heat_int = KB + 'HeatDemandInterval_' + str(uuid.uuid4())
            heat_t1 = KB + 'HeatDemandTimeInstant_' + str(uuid.uuid4())
            heat_t2 = KB + 'HeatDemandTimeInstant_' + str(uuid.uuid4())
            # Grid temperature data length
            tmp_int = KB + 'GridTemperatureInterval_' + str(uuid.uuid4())
            tmp_t1 = KB + 'GridTemperatureTimeInstant_' + str(uuid.uuid4())
            tmp_t2 = KB + 'GridTemperatureTimeInstant_' + str(uuid.uuid4())
            # Simulation time
            sim_t = KB + 'SimulationTimeInstant_' + str(uuid.uuid4())

            for run in range(params['numberOfTimeSteps']):
                if run == 0:
                    t1 = params['start']
                    t2 = t1 + params['timeDelta']
                    # Instantiate required time instances to initiate optimisation cascades
                    kg_client.instantiate_time_instance(sim_t, t1)

                    # Instantiate derivation markups
                    #TODO: to be implemented
                else:
                    t1 += params['timeDelta']
                    t2 += params['timeDelta']
                    # Update required time instances to initiate optimisation cascades
                    kg_client.update_time_instance(sim_t, t1)

                    # Request derivation update from Aermod Agent
                    #TODO: to be implemented

            # Once processing is complete, update the flag
            processing = False

        return jsonify(message='District heating optimisation runs successfully completed.'), 200

    return app


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
    app = create_app()
    app.run(host='localhost', port="5000")
    logger.info('App started')