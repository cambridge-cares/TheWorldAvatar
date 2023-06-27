# Create Flask application and import (i.e. register) HTTP routes

from flask import Flask

from agent.utils.stack_configs import UPDATE_ENDPOINT
from agent.kgutils.initialise_kg import create_blazegraph_namespace


def create_app(test_config=None):
    """
    Create and configure an instance of the Flask application.
    """

    app = Flask(__name__, instance_relative_config=True)
    app.config['JSON_SORT_KEYS'] = False
    if test_config is not None:
        # Load the test config if passed in
        app.config.update(test_config)

    with app.app_context():
        # Import parts of application
        import agent.flaskapp.home.routes as home
        import agent.flaskapp.inputtasks.routes as inputtasks
        import agent.flaskapp.outputtasks.routes as outputtasks

        # Register Blueprints
        app.register_blueprint(home.home_bp)
        app.register_blueprint(inputtasks.inputtasks_bp)
        
        # Data retrieval only kept as "backup" to create geojson files on demand
        # Regular visualisation now handled via Postgis/Geoserver
        app.register_blueprint(outputtasks.outputtasks_bp)

    # Create Blazegraph namespace if not exists (on app startup)
    create_blazegraph_namespace(endpoint=UPDATE_ENDPOINT)

    return app
