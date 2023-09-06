# Flask app

## Development

Create a virtual environment for dependency management and install dependencies.

```
python -m venv flask_venv
source flask_venv/bin/activate
pip install -r requirements.txt
```

Start the development server at `localhost:5000`.

```
python app.py
```

To enter debug mode (app is automatically reloaded upon code changes)
```
flask --app app.py --debug run
```

## Deployment

See [Deployment for Marie](../README.md#deployment-for-marie).