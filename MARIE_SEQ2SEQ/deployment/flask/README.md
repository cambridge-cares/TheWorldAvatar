# Flask app

## Development

Create a virtual environment for dependency management and activate it.

```
python3 -m venv .venv
. .venv/bin/activate
```

Install the application

```
pip install -e .
```

Start the development server at `localhost:5000` in debug mode (app is automatically reloaded upon code changes)
```
flask --app marie run --debug
```

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

## Deployment

See [Deployment for Marie](../README.md#deployment-for-marie).