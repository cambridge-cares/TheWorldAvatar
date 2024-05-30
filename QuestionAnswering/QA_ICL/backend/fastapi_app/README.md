# QA Application Backend

## Installation

### Prerequisites

#### Environment 

- python>=3.8
- [conda](https://conda.io/projects/conda/en/latest/user-guide/install/index.html)
  
#### Required services 

- A Redis server (see [Run Redis Stack in Docker](https://redis.io/docs/latest/operate/oss_and_stack/install/install-stack/docker/))
- A text embedding service exposed via either OpenAI compatible or NVIDIA's Triton API. For running a local Triton server in Docker, see [triton_inference_server](../triton_inference_server/).
- A chat completion service exposed via OpenAI compatible API.
- (Optional) LocationIQ geocoding service. 
  - Context: Some of Zaha's features require geocoding i.e. the determination of geographical coordinates based on a location name. These include functionalities such as finding pollutant concentrations or the nearest carpark given a location name. By default the app will preferentially use the [API key-free geocoding service by Nominatim](https://nominatim.org/release-docs/latest/api/Search/). However, Nominatim does impose a rate limit (read more about [Nominatim's usage policy](https://operations.osmfoundation.org/policies/nominatim/)). 
  - To avoid having geocoding requests denied, the app will also make requests to [LocationIQ](https://locationiq.com/), which requires an API key. At the time of writing, LocationIQ does offer a free plan for API access.
  - KIV: [set up a local Nominatim instance](https://nominatim.org/release-docs/latest/admin/Installation/) to remove dependency on external geocoding service.

#### Required data

- [data/lexicon](data/lexicon/): to be populated with JSON files containing lexicon data. Each file is a list of lexicon objects.
- [data/schema](data/schema/): to be populated with JSON files containing simplified graph data. Each file is a list of simplified graph schema objects.
- [data/examples](data/examples/): to be populated with JSON files containing examples of converting natural language questions into structured data requests. Each file is a list of example object.

All data schemas are defined in [../../data_generation/README.md](../../data_generation/README.md).
  
### Steps

1. Create conda environment and activate it.
   ```{bash}
   conda create --name qa_backend python==3.10
   conda activate qa_backend
   ```

1. Install dependencies.
   ```{bash}
   pip install -r requirements.txt
   ```

1. Set the following environment variables
   
   Common environment variables:

   - `REDIS_HOST`: host name of Redis server.
   - `TEXT_EMBDDING_SERVER`: either `openai` if connecting to an OpenAI compatible server, or `triton` if connecting to Triton server.
   - `TEXT_EMBEDDING_URL`: url of text embedding server.
   - `TRANSLATOR_OPENAI_BASE_URL` (optional): base URL of OpenAI compatible server used by internal translation service (i.e. the conversion of natural language questions into structured data requests), if different from OpenAI's default.
   - `TRANSLATOR_OPENAI_API_KEY` (optional): API key to authorise requests to OpenAI server used by internal translation service; defaults to `OPENAI_API_KEY` if not set.
   - `TRANSLATOR_OPENAI_MODEL`: name of model at OpenAI server called by internal translation service.
   - `CHAT_OPENAI_BASE_URL` (optional): base URL of OpenAI compatible server used by internal chat service (i.e. the formulation of human-like response from structured data), if different from OpenAI's default.
   - `CHAT_OPENAI_API_KEY` (optional): API key to authorise requests to OpenAI server used by internal chat service; defaults to `OPENAI_API_KEY` if not set.
   - `CHAT_OPENAI_MODEL`: name of model at OpenAI server called by internal chat service.

   Marie-specific environment variables:
   - `KG_ENDPOINT_ONTOSPECIES`, `KG_ENDPOINT_ONTOKIN`, `KG_ENDPOINT_ONTOCOMPCHEM`, `KG_ENDPOINT_ONTOZEOLITE`: SPARQL endpoints of OntoSpecies, OntoKin, OntoCompChem, OntoZeolite KGs.
   
   Zaha-specific environment variables:
   - `KG_ENDPOINT_SG_ONTOP`, `KG_ENDPOINT_SG_DISPERSION`, `KG_ENDPOINT_SG_PLOT`, `KG_ENDPOINT_SG_COMPANY`, `KG_ENDPOINT_SG_CARPARK`: public SPARQL endpoints of Singapore's stack Ontop, dispersion, plot, company, carpark KGs
   - `SG_STACK_INTERNAL_ONTOP_ENDPOINT`, `SG_STACK_INTERNAL_CARPARK_ENDPOINT`: SPARQL endpoints of Singapore's stack Ontop and carpark KGs within its internal network.
   - `ENDPOINT_FEATURE_INFO_AGENT`: endpoint of the feature info agent of Singapore's stack
   - `ENDPOINT_POLLUTANT_CONCENTRATIONS`: endpoint to retrieve pollutant concentrations
   - `LOCATION_IQ_API_KEY`: API key to make requests to LocationIQ server


1. Ingest lexicon, simplified graph schema, and example data into Redis server. 
   <!-- TODO: allow user to pass in endpoints for Redis and text embedding servers when executing the script. -->
   ```
   sh ingest.sh
   ```

2. Start the development server in debug mode (app is automatically reloaded upon code changes) `uvicorn main:app --port 5000 --reload --log-config=log_conf.yaml`. The app will be running at `localhost:5000`.

## Usage

- Zaha's frontend can be accessed at `localhost:5000` in the browser.
- For the specifications of backend APIs, visit `localhost:5000/docs`.

<!-- ## Tests

Install test dependencies with `pip install -r requirements-test.txt`.

Run only unit tests: `pytest `

`docker run -d --name redis-stack -p 6379:6379 -p 8001:8001 redis/redis-stack:latest`
`docker exec -it redis-stack redis-cli` -->