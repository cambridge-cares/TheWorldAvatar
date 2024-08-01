# Flask app

## Local setup 

1. Create a virtual environment for dependency management and activate it.

   ```
   python3 -m venv .venv
   source .venv/bin/activate
   ```

1. Install the application `pip install -e .`.

1. Start the development server in debug mode (app is automatically reloaded upon code changes) `flask --app marie run --debug`. The app will be running at `localhost:5000`.

## Test

```
pip install '.[test]'
pytest
```

Run with coverage report:

```
coverage run -m pytest
coverage report
coverage html  # open htmlcov/index.html in a browser
```
