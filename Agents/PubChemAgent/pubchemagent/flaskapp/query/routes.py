from flask import Blueprint, request, jsonify, make_response
from pubchemagent.app import species_instantiation, element_instantiation

query_bp = Blueprint(
    'query_bp', __name__
)

# Define a route for API requests
@query_bp.route('/query/species', methods=['GET'])
def api():
    inchi_raw = request.args.get('inchi', '')
    print("Received InChI:", inchi_raw)

    if not inchi_raw:
        return jsonify({
            "status": "400",
            "errormsg": "Missing required parameter: inchi"
        }), 400

    # --- Require URL-formatted (URL-encoded) InChI ---
    # Detect illegal character: space.
    illegal_chars = [' ']

    if any(ch in inchi_raw for ch in illegal_chars):
        return jsonify({
            "status": "400",
            "errormsg": (
                "Invalid InChI: the InChI string must be URL-encoded. "
                "For example, '+' must be encoded as '%2B'. "
                "Please send a URL-formatted InChI string."
            )
        }), 400

    try:
        result = species_instantiation(inchi_raw)
        return result

    except Exception as ex:
        print("❌ Error:", ex)
        return jsonify({"status": "500", "errormsg": "Internal server error"}), 500


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