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

    with app.app_context():
        # Import parts of our application
        import stdc.flaskapp.home.routes as home
        import stdc.flaskapp.calculator.routes as calculator

        # Register Blueprints
        app.register_blueprint(home.home_bp)
        app.register_blueprint(calculator.calculator_bp)

    return app