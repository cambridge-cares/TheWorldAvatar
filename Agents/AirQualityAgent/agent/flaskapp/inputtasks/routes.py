################################################
# Authors: Markus Hofmeister (mh807@cam.ac.uk) #    
# Date: 08 Apr 2022                            #
################################################

from flask import Blueprint, request, jsonify

from agent.datainstantiation.stations import instantiate_all_stations, \
                                             instantiate_mocked_kingslynn_stations
from agent.datainstantiation.readings import instantiate_all_station_readings, \
                                             add_all_readings_timeseries, \
                                             update_all_stations


# Initialise logger
from py4jps import agentlogging
logger = agentlogging.get_logger("prod")

inputtasks_bp = Blueprint(
    'inputtasks_bp', __name__
)

# Define route for API request to instantiate all stations
@inputtasks_bp.route('/airqualityagent/instantiate/stations', methods=['GET'])
def api_instantiate_all_stations():
    # Check arguments (query parameters)
    if len(request.args) > 0:
        logger.warning("Query parameters provided, although not required. \
                        Provided arguments will be neglected.")
    try:
        # Instantiate stations
        response = instantiate_all_stations()
        print(f"Number of instantiated stations: {response}")
        return jsonify({"Instantiated stations": response}), 200

    except Exception as ex:
        logger.error('Instantiation failed: ' + str(ex))
        return jsonify({'msg': 'Instantiation failed: ' + str(ex)}), 500


# Define route for API request to instantiate all readings
# (i.e. observations for all prior instantiated stations)
@inputtasks_bp.route('/airqualityagent/instantiate/readings', methods=['GET'])
def api_instantiate_all_readings():
    # Check arguments (query parameters)
    if len(request.args) > 0:
        logger.warning("Query parameters provided, although not required. \
                        Provided arguments will be neglected.")
    try:
        # Instantiate stations
        response = instantiate_all_station_readings()
        print(f"Number of instantiated readings: {response}")
        return jsonify({"Instantiated readings": response}), 200

    except Exception as ex:
        logger.error('Instantiation failed: ' + str(ex))
        return jsonify({'msg': 'Instantiation failed: ' + str(ex)}), 500


# Define route for API request to add latest time series readings for all 
# instantiated time series 
@inputtasks_bp.route('/airqualityagent/update/timeseries', methods=['GET'])
def api_add_all_readings_timeseries():
    # Check arguments (query parameters)
    if len(request.args) > 0:
        logger.warning("Query parameters provided, although not required. \
                        Provided arguments will be neglected.")
    try:
        # Instantiate stations
        response = add_all_readings_timeseries()
        print(f"Number of updated time series readings: {response}")
        return jsonify({"Updated time series readings": response}), 200

    except Exception as ex:
        logger.error('Time series addition failed: ' + str(ex))
        return jsonify({'msg': 'Time series addition failed: ' + str(ex)}), 500


# Define route for API request to update all stations and readings (i.e. instantiate
# missing stations), and add latest time series readings for all time series
@inputtasks_bp.route('/airqualityagent/update/all', methods=['GET'])
def api_update_all_stations():
    # Check arguments (query parameters)
    if len(request.args) > 0:
        logger.warning("Query parameters provided, although not required. \
                        Provided arguments will be neglected.")
    try:
        # Instantiate stations
        response = update_all_stations()
        return_dict = {
            'Number of instantiated stations': response[0],
            'Number of instantiated readings': response[1],
            'Number of updated time series readings (i.e. dataIRIs)': response[2]
        }
        for key, value in return_dict.items():
            print(key, ' : ', value)
        return jsonify(return_dict), 200

    except Exception as ex:
        logger.error('Update failed: ' + str(ex))
        return jsonify({'msg': 'Update failed: ' + str(ex)}), 500
    

# Define route for API request to instantiate mocked/virtual station in King's Lynn
@inputtasks_bp.route('/airqualityagent/instantiate/mocked', methods=['GET'])
def api_instantiate_mocked_stations():
    # Check arguments (query parameters)
    if len(request.args) > 0:
        logger.warning("Query parameters provided, although not required. \
                        Provided arguments will be neglected.")
    try:
        response = instantiate_mocked_kingslynn_stations()
        print(f"Number of mocked stations: {response}")
        return jsonify({"Instantiated mocked stations": response}), 200

    except Exception as ex:
        logger.error('Instantiation failed: ' + str(ex))
        return jsonify({'msg': 'Instantiation failed: ' + str(ex)}), 500
