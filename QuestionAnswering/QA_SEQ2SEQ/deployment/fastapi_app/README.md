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
   - `KG_URL_CHEMISTRY`: endpoint to Blazegraph server for chemistry
   - `BG_USER`: username to access Blazegraph server for chemistry
   - `BG_PASSWORD`: password to access Blazegraph server for chemistry
   - `URL_PREFIX`: prefix path for Dash app (visualisation of numerical properties), required when the app is accessed via a proxy server that redirects requests from a path prefix

1. Start the development server in debug mode (app is automatically reloaded upon code changes) `uvicorn main:app --port 5000 --reload --log-config=log_conf.yaml`. The app will be running at `localhost:5000`.

## Usage

### Backend

Visit `localhost:5000/docs` to see detailed API documentation. Below is an overview.

- GET '/': to retrieve the HTML page for the app's 
- POST '/translate': to translate a natural language question into a SPARQL query
- POST '/sparql': to execute a SPARQL query at a specified namespace
- POST '/chat': to obtain a human-like response
- POST '/adv_search': to search for zeolites based on their properties and metadata
- POST '/get-iri-details': to obtain all data associated with a species or zeolite

### Frontend

Here are a set of features implemented. Please visit `localhost:5000` in a browser to interact with them. 

- Search by natural language
- (Advanced Search) Search for zeolite frameworks and zeolitic materials based on their properties and metadata
- (Explore) Visualise the correlation between two numerical properties of species belonging to a chemical class or among zeolites
