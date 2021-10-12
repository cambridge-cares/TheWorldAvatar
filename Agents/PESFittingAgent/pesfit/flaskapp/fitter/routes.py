from flask import Blueprint, request, jsonify, make_response
from pesfit.kgoperations.getkgdata import get_kg_data

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
    opes_inputs = get_kg_data(ontopesscan_IRI)

    print(opes_inputs)

    try:
        # Run the model
        response = 'to be implemented'
        return jsonify({"result": response})

    except Exception as ex:
        print(ex)
        return jsonify({"status": '500', 'errormsg': 'Invalid request'})