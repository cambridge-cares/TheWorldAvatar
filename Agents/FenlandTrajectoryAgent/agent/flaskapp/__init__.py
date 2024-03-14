from flask import Flask
# Import blueprints
from agent.flaskapp.gpstasks.routes import gps_instantiation_bp
from agent.flaskapp.home.routes import gps_bp
# Import utilities for RDF store and database initialization
from agent.kgutils.initialise_kg import create_blazegraph_namespace
from agent.kgutils.utils import *

def create_app(test_config=None):
    """
    Create and configure an instance of the Flask application.
    """
    app = Flask(__name__, instance_relative_config=True)
    app.config['JSON_SORT_KEYS'] = False

    # Optionally load test configuration if passed in
    if test_config is not None:
        app.config.update(test_config)

    # Register blueprints for application components
    app.register_blueprint(gps_instantiation_bp, url_prefix='/gpstasks')
    app.register_blueprint(gps_bp, url_prefix='/')

    # Perform initial setup actions
    with app.app_context():
        # Initialize PostgreSQL database and Blazegraph namespace for RDF data
        # Adjust these function calls based on your actual utility functions and requirements
        create_postgres_db()
        create_blazegraph_namespace(endpoint=UPDATE_ENDPOINT)

        # Additional setup actions can be added here, such as uploading ontologies
        # if needed, uncomment and adjust the following line:
        # upload_ontology()

    return app
