################################################
# Authors: Markus Hofmeister (mh807@cam.ac.uk) #    
# Date: 22 Feb 2023                            #
################################################

# Create Flask application and integrate Celery task queue
# Celery task queue can be used to start long running tasks (i.e. data assimilation)
# as background tasks so that the main App remains responsive, for details see:
# DOC: https://flask.palletsprojects.com/en/2.2.x/patterns/celery/
# EXAMPLE: https://github.com/pallets/flask/tree/main/examples/celery

from flask import Flask
from celery import Celery, Task

from agent.kgutils.initialise_kb import create_blazegraph_namespace, upload_ontology
from agent.utils.stack_configs import UPDATE_ENDPOINT


def create_app(test_config=None) -> Flask:
    """
    Create and configure an instance of the Flask application
    (using Application Factory pattern)
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

        # Register Blueprints
        app.register_blueprint(home.home_bp)
        app.register_blueprint(inputtasks.inputtasks_bp)

    # Integrate Celery with Flask, using Redis for communication. Enable result backend,
    # but ignore results by default to store results only for relevant tasks
    app.config.from_mapping(
        CELERY=dict(
            broker_url="redis://localhost",
            result_backend="redis://localhost",
            task_ignore_result=True,
        ),
    )
    # Set app.extensions["celery"] to the Celery app object
    app.config.from_prefixed_env()
    celery_init_app(app)


    # Create Blazegraph namespace if not exists (on app startup)
    create_blazegraph_namespace(endpoint=UPDATE_ENDPOINT, quads=False, geospatial=False)
    # Upload ontology and units to Blazegraph
    upload_ontology()

    return app


def celery_init_app(app: Flask) -> Celery:
    """
    Create Celery app object with configuration as specified in Flask configuration. 
    The Celery app is set as the default, so that it is seen during each request.
    """
    class FlaskTask(Task):
        def __call__(self, *args: object, **kwargs: object) -> object:
            with app.app_context():
                return self.run(*args, **kwargs)

    celery_app = Celery(app.name, task_cls=FlaskTask)
    celery_app.config_from_object(app.config["CELERY"])
    celery_app.set_default()
    app.extensions["celery"] = celery_app
    return celery_app
