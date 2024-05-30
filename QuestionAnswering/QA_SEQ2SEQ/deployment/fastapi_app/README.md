# FastAPI app

## Local setup 

1. Create a virtual environment for dependency management (e.g. venv, conda) and activate it.

1. Install dependencies `pip install -r requirements.txt`.

1. The following environment variables must be set:
   - `QA_SUPERDOMAIN`: either `chemistry` for Marie or `cities` for Zaha,
   - `TRITON_ENDPOINT`: endpoint to Triton server where the translation model runs,
   - `KG_ENDPOINT_{domain}`: endpoint to KG server for the relevant domains,
   - `OPENAI_API_ENDPOINT`: endpoint to OpenAI's API (can be either `https://api.openai.com/v1` or a custom endpoint),
   - `OPENAI_API_KEY`: API key for OpenAI's API access,
   - `OPENAI_MODEL`: model to generate the chatbot's response,
   - `OPENAI_INPUT_LIMIT`: the maximum number of tokens that will be sent to the the OpenAI's API for chatbot generation.

1. Start the development server in debug mode (app is automatically reloaded upon code changes) `uvicorn main:app --port 5000 --reload --log-config=log_conf.yaml`. The app will be running at `localhost:5000`.

## Tests

Install test dependencies with `pip install -r requirements-test.txt`.

To run test, execute `pytest`.