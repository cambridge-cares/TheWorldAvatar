from flask import Flask

def create_app(test_config=None):
    """Create and configure an instance of the Flask application."""
    app = Flask(__name__, instance_relative_config=True)
    app.config['JSON_SORT_KEYS'] = False
    if test_config is not None:
        # load the test config if passed in
        app.config.update(test_config)

    with app.app_context():
        # Import parts of our application
        import flaskapp.geonames.routes as geonames
        import flaskapp.enrichment.routes as enrichment

        # Register Blueprints
        app.register_blueprint(geonames.geonames_bp)
        app.register_blueprint(enrichment.enrichment_bp)

    return app