from flask import Blueprint, request, jsonify, make_response
from stdc.kgoperations.getkgdata import get_ontocompchem_data, \
                                        get_ontospecies_data
from stdc.app import runThermoCalculator

calculator_bp = Blueprint(
    'calculator_bp', __name__
)

# Define a route for API requests
@calculator_bp.route('/api/thermoagent/calculate', methods=['GET'])
def api():
    # Check arguments (query parameters)
    print(request.args)
    inputs= {}
    ontocompchem_IRI = request.args['ontocompchem_IRI']
    ontospecies_IRI = request.args['ontospecies_IRI']
    oc_inputs = get_ontocompchem_data(ontocompchem_IRI, ontospecies_IRI)
    os_inputs = get_ontospecies_data(ontospecies_IRI)
    inputs = {**oc_inputs,**os_inputs}

    if 'temperature' in request.args:
        inputs['temperature'] = request.args['temperature']
    if 'pressure' in request.args:
        inputs['pressure'] = request.args['pressure']
    print(inputs)

    try:
        # Run the model
        ThermoData = runThermoCalculator(inputs)
        print(ThermoData)
        return jsonify({"result": ThermoData})

    except Exception as ex:
        print(ex)
        return jsonify({"status": '500', 'errormsg': 'Invalid request'})