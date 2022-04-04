from flask import Blueprint, request, jsonify, make_response

#import agentlogging
from metoffice.datainstantiation.stations import instantiate_all_stations


# # Initialise logger
# logger = agentlogging.get_logger("dev")

inputtasks_bp = Blueprint(
    'inputtasks_bp', __name__
)

# Define a route for API request to instantiate all stations
@inputtasks_bp.route('/api/metofficeagent/instantiate/stations', methods=['GET'])
def api():
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
