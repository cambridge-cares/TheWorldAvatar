from flask import Blueprint, request, jsonify, make_response
from pubchemagent.app import query_with_inchi

query_bp = Blueprint(
    'query_bp', __name__
)

# Define a route for API requests
@query_bp.route('/api/pubchemagent/query', methods=['GET'])
def api():
    # Check arguments (query parameters)
    print(request.args)
    inchi_string = request.args['inchi']
    
    try:
        # Run the model
        data, source = query_with_inchi(inchi_string)
        print(source, '\n')
        print(data)
        return jsonify({"result": data})

    except Exception as ex:
        print(ex)
        return jsonify({"status": '500', 'errormsg': 'Invalid request'})