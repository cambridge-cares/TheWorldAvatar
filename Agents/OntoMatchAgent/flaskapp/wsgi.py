from flask import Flask
import os
import sys
sys.path.append(os.getcwd())
def create_app(test_config=None):
    """Create and configure an instance of the Flask application."""
    app = Flask(__name__, instance_relative_config=True)
    app.config['JSON_SORT_KEYS'] = False
    if test_config is not None:
        # load the test config if passed in
        app.config.update(test_config)

    with app.app_context():
        # Import parts of our application
        import geonames.routes as geonames
        import enrichment.routes as enrichment
        import blackboard.routes as blackboard
        import matchmanager.routes as matchmanager
        import coordinator.routes as coordinator

        # Register Blueprints
        app.register_blueprint(geonames.geonames_bp)
        app.register_blueprint(enrichment.enrichment_bp)
        app.register_blueprint(blackboard.blackboard_bp)
        app.register_blueprint(matchmanager.matchmanager_bp)
        app.register_blueprint(coordinator.coordinator_bp)

    return app
    
app = create_app()

if __name__ == "__main__":
    app.run(host='localhost',port=5000)