from flask import Flask, request, jsonify
from stdc.kgoperations.getkgdata import get_ontocompchem_data, \
                                        get_ontospecies_data
from stdc.app import runThermoCalculator


def create_app(test_config=None):
    """Create and configure an instance of the Flask application."""
    app = Flask(__name__, instance_relative_config=True)
    app.config['JSON_SORT_KEYS'] = False
    if test_config is not None:
        # load the test config if passed in
        app.config.update(test_config)


    # Show an instructional message at the app root
    @app.route('/')
    def default():
        msg  = "To see the result of an API call, enter a URL of the form:<BR>"
        msg += "&nbsp&nbsp /api/thermoagent/calculate?ontocompchem_IRI=oc_IRI&ontospecies_IRI=os_IRI"
        return msg

    # Define a route for API requests
    @app.route('/api/thermoagent/calculate', methods=['GET'])
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

    return app