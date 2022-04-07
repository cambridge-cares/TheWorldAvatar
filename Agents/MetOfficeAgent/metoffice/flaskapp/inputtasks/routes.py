from flask import Blueprint, request, jsonify, make_response

#import agentlogging
from metoffice.datainstantiation.stations import instantiate_all_stations
from metoffice.datainstantiation.readings import instantiate_all_station_readings
from metoffice.datainstantiation.readings import add_all_readings_timeseries


# # Initialise logger
# logger = agentlogging.get_logger("dev")

inputtasks_bp = Blueprint(
    'inputtasks_bp', __name__
)

# Define route for API request to instantiate all stations
@inputtasks_bp.route('/api/metofficeagent/instantiate/stations', methods=['GET'])
def api_instantiate_all_stations():
    # Check arguments (query parameters)
    if len(request.args) > 0:
        print("Query parameters provided, although not required. " \
              + "Provided arguments will be neglected.")
        # logger.warning("Query parameters provided, although not required. \
        #                 Provided arguments will be neglected.")
    try:
        # Instantiate stations
        response = instantiate_all_stations()
        print(f"Number of instantiated stations: {response}")
        return jsonify({"stations": response})

    except Exception as ex:
        print(ex)
        return jsonify({"status": '500', 'errormsg': 'Instantiation failed'})


# Define route for API request to instantiate all readings
# (i.e. observations and forecasts for all prior instantiated stations)
@inputtasks_bp.route('/api/metofficeagent/instantiate/readings', methods=['GET'])
def api_instantiate_all_readings():
    # Check arguments (query parameters)
    if len(request.args) > 0:
        print("Query parameters provided, although not required. " \
              + "Provided arguments will be neglected.")
        # logger.warning("Query parameters provided, although not required. \
        #                 Provided arguments will be neglected.")
    try:
        # Instantiate stations
        response = instantiate_all_station_readings()
        print(f"Number of instantiated readings: {response}")
        return jsonify({"readings": response})

    except Exception as ex:
        print(ex)
        return jsonify({"status": '500', 'errormsg': 'Instantiation failed'})


# Define route for API request to add latest time series readings for all 
# instantiated time series 
@inputtasks_bp.route('/api/metofficeagent/timeseries/adddata', methods=['GET'])
def api_add_all_readings_timeseries():
    # Check arguments (query parameters)
    if len(request.args) > 0:
        print("Query parameters provided, although not required. " \
              + "Provided arguments will be neglected.")
        # logger.warning("Query parameters provided, although not required. \
        #                 Provided arguments will be neglected.")
    try:
        # Instantiate stations
        response = add_all_readings_timeseries()
        print(f"Number of updated time series readings: {response}")
        return jsonify({"readings": response})

    except Exception as ex:
        print(ex)
        return jsonify({"status": '500', 'errormsg': 'Time series addition failed'})
