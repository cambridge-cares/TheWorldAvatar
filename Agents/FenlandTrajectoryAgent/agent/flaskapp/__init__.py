from flask import Flask
# Import blueprints
from agent.flaskapp.gpstasks.routes import gps_instantiation_bp
from agent.flaskapp.home.routes import gps_bp
from agent.flaskapp.exposure.route import exposure_bp
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
    app.register_blueprint(gps_instantiation_bp, url_prefix='')
    app.register_blueprint(gps_bp, url_prefix='')
    app.register_blueprint(exposure_bp, url_prefix='')


    # Perform initial setup actions within the app context
    with app.app_context():
        # Initialize PostgreSQL database and Blazegraph namespace for RDF data
        try:
            create_postgres_db()
            create_blazegraph_namespace(endpoint=SPARQL_UPDATE_ENDPOINT)
            # Optional: Upload ontologies or perform additional setup
            # upload_ontology()
        except Exception as e:
            app.logger.error(f"Failed to initialize database or RDF store: {e}")

    return app
