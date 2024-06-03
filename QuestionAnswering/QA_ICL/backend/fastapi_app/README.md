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

1. Create an `app.local.yaml` file using the following template.
   ```{yaml}
   translator:
     api_key: <openai_api_key_for_translation_service>
   chat:
     api_key: <openai_api_key_for_chat_service>
   location_iq:
     api_key: <location_iq_api_key_for_geocoding_service>
   ```

1. Ingest lexicon, simplified graph schema, and example data into Redis server. 
   Execute `python ingest.py --help` to see a list of all available command line options.
   Example execution:
   ```
   python ingest.py --redis_host localhost --text_embedding_backend triton --text_embedding_url localhost:8001 --drop_indx --invalidate_cache
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