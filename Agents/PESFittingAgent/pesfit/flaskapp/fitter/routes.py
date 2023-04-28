from flask import Blueprint, request, jsonify, make_response
from pesfit.app import pesfit_wrapper

fitter_bp = Blueprint(
    'fitter_bp', __name__
)

# Define a route for API requests
@fitter_bp.route('/api/pesfitter', methods=['GET'])
def api():
    # Check arguments (query parameters)
    print(request.args)
    inputs= {}
    ontopesscan_IRI = request.args['ontopesscan_IRI']
    #TO BE IMPLEMENTED

    try:
        pesfit_wrapper(inputs)

    except Exception as ex:
        print(ex)
        return jsonify({"status": '500', 'errormsg': 'Invalid request'})