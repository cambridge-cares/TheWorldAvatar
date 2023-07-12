# Create Flask application and define HTTP route

import re
from datetime import datetime
from flask import Flask, request, jsonify
import threading

from py4jps import agentlogging

from agent.datamodel import *

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

    # Ensure that only one request is processed at a time and a lock message provided otherwise
    lock = threading.Lock()
    global processing
    processing = False

    @app.route('/triggerOptimisation', methods=['POST'])
    def trigger_optimisation():

        global processing

        if processing:
            return jsonify(message='Previous request is not finished yet. Please try again later.'), 423

        # Start processing the current request
        with lock:
            processing = True

            # Verify received HTTP request parameters
            params = request.get_json()
            params = validate_input_params(params)

            # Once processing is complete, update the flag
            processing = False

        return jsonify(message='Request processed successfully')


    return app


def validate_input_params(params_dict):
    """
    Validate suitability of received HTTP request parameters (converted to dict)
    """
    # Check if all required keys are present
    required_keys = ['startDateTime', 'numberOfTimeSteps', 'frequency', 'heatDemandDataLength', 'gridTemperatureDataLength']
    for key in required_keys:
        if key not in params_dict:
            raise ValueError(f"Missing required parameter: {key}")

    # Validate startDateTime
    start_date_time = params_dict['startDateTime']
    try:
        datetime.strptime(start_date_time, '%Y-%m-%dT%H:%M:%SZ')
    except ValueError:
        raise ValueError("Invalid startDateTime format. Expected format: YYYY-MM-DDThh:mm:ssZ")

    # Convert startDateTime to unix timestamp if needed
    unix_timestamp = datetime.timestamp(datetime.strptime(start_date_time, '%Y-%m-%dT%H:%M:%SZ'))
    params_dict['startDateTime'] = int(unix_timestamp)

    # Validate numberOfTimeSteps
    num_time_steps = params_dict['numberOfTimeSteps']
    if not isinstance(num_time_steps, int) or num_time_steps <= 0:
        raise ValueError("numberOfTimeSteps should be an integer greater than 0")

    # Validate frequency and map to constants
    frequency = params_dict['frequency']
    valid_frequencies = {
        'day': TIME_DAY,
        'hour': TIME_HOUR,
        'minute': TIME_MINUTE,
        'second': TIME_SECOND
    }
    if frequency not in valid_frequencies:
        raise ValueError("Invalid frequency. Valid options: 'day', 'hour', 'minute', 'second'")
    params_dict['frequency'] = valid_frequencies[frequency]

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