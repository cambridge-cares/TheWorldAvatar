# Flask app

## Local setup 

1. Create a virtual environment for dependency management (e.g. venv, conda) and activate it.

1. Install dependencies `pip install -r requirements.txt`.

1. Start the development server in debug mode (app is automatically reloaded upon code changes) `uvicorn main:app --port 5000 --reload --log-config=log_conf.yaml`. The app will be running at `localhost:5000`.

## Tests

Install test dependencies with `pip install -r requirements-test.txt`.

To run test, execute `pytest`.