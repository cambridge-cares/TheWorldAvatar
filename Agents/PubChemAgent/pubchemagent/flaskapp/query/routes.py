from flask import Blueprint, request, jsonify, make_response
from pubchemagent.app import species_instantiation, element_instantiation

query_bp = Blueprint(
    'query_bp', __name__
)

# Define a route for API requests
@query_bp.route('/query/species', methods=['GET'])
def api():
    # Check arguments (query parameters)
    print(request.args)
    inchi_string = request.args['inchi']
    inchi_string=inchi_string.replace('%2b','+').replace('%3b',';').replace('%28','(').replace('%29',')').replace('%2e','.').replace('%2c',',')
    print(inchi_string)
    
    try:
        # Run the model
        species_instantiation(inchi_string)
        msg  = "Species " + inchi_string + " have been instantiated"
        return msg

    except Exception as ex:
        print(ex)
        return jsonify({"status": '500', 'errormsg': 'Invalid request'})


# Define a route for API requests
@query_bp.route('/query/elements')
def apielements():
    
    try: 
        for el in range(1,119):
            print('Element ' + str(el))
            element_instantiation(el)
        msg  = "Elements have been instantiated"
        return msg

    except Exception as ex:
        print(ex)
        return jsonify({"status": '500', 'errormsg': 'Invalid request'})