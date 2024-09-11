# QA Application Backend

## Initial setup

### Required services

- A Redis server (see [Run Redis Stack in Docker](https://redis.io/docs/latest/operate/oss_and_stack/install/install-stack/docker/))
- A text embedding service exposed via either OpenAI-compatible or NVIDIA's Triton API. For running a local Triton server in Docker, see [triton_inference_server](../triton_inference_server/).
- A chat completion service exposed via OpenAI-compatible API.
- (Optional) LocationIQ geocoding service. 
  - Context: Some of Zaha's features require geocoding i.e. the determination of geographical coordinates based on a location name. These include functionalities such as finding pollutant concentrations or the nearest carpark given a location name. By default the app will preferentially use the [API key-free geocoding service by Nominatim](https://nominatim.org/release-docs/latest/api/Search/). However, Nominatim does impose a rate limit (read more about [Nominatim's usage policy](https://operations.osmfoundation.org/policies/nominatim/)). 
  - To avoid having geocoding requests denied, the app will also make requests to [LocationIQ](https://locationiq.com/), which requires an API key. At the time of writing, LocationIQ does offer a free plan for API access.
  - KIV: [set up a local Nominatim instance](https://nominatim.org/release-docs/latest/admin/Installation/) to remove dependency on external geocoding service.


### Required resources

Resources required by this app are:

- [data/lexicon](data/lexicon/): to be populated with JSON files containing lexicon data. Each file is an array of lexicon objects.
- [data/schema/properties](data/schema/properties): to be populated with JSON files containing simplified graph data. Each file is an array of graph schema objects.
- [data/examples](data/examples/): to be populated with JSON files containing examples of converting natural language questions into structured data requests. Each file is an array of example objects.
- [data/qtRecog_examples](data/qtRecog_examples/): to be populated with JSON files containing examples of recognising physical quantities from natural language texts. Each file is an array of example objects.

All JSON schemas are defined in [../../data_generation/README.md](../../data_generation/README.md#schema-definitions).

These data need to be ingested into the Redis server before the app starts accepting requests from users. The module [`ingest_data`](./ingest_data/) provides the utilities to run automated ingestion. Execute `python -m ingest_data --help` to see a list of all available command-line options.
   
Example execution:
```
python -m ingest_data --redis_host localhost --text_embedding_backend triton --text_embedding_url localhost:8001 --drop_index --invalidate_cache
```

Each resource can also be ingested separately.
- To ingest entity lexicons, `python -m ingest_data.entities ...`
- To ingest examples for translating input questions into data requests, `python -m ingest_data.nlq2datareq_examples ...`
- To ingest KG relations, `python -m ingest_data.properties ...`

### Configurable parameters

All configurable parameters for the app are indicated in [`app.yaml`](./app.yaml), which have been pre-populated with default values. Environment-specific parameters are also pre-filled in [`app.dev.yaml`](./app.dev.yaml) and [`app.prod.yaml`](./app.prod.yaml), either of which is automatically selected based on the value `dev` or `prod` of the `APP_ENV` environment variable.

API keys for external services need to be set in the `app.local.yaml` file, which has to be created by the user. Following is a minimal template. Additional fields can be set to override default settings.
```{yaml}
translator:
   api_key: <openai_api_key_for_translation_service>
chat:
   api_key: <openai_api_key_for_chat_service>
location_iq:
   api_key: <location_iq_api_key_for_geocoding_service>
ontomops_fileserver:
   username: <ontomops_fileserver_username>
   password: <ontomops_fileserver_password>
```

Precedence: `app.local.yaml` > `app.{APP_ENV}.yaml` > `app.yaml`.

## Native installation

### Prerequisites

- (recommended) Linux enviroment
- python>=3.8
- [conda](https://conda.io/projects/conda/en/latest/user-guide/install/index.html)

  
### Steps

1. Create conda environment and activate it.
   ```{bash}
   conda create --name qa_backend python==3.10
   conda activate qa_backend
   ```

2. Install dependencies.
   ```{bash}
   pip install -r requirements.txt
   ```

3. Ingest required resources into Redis server, as per the section ['Required resources'](#required-resources).

4. Start the server:
   - In debug mode (app is automatically reloaded upon code changes), `uvicorn main:app --reload --log-config=log_conf.yaml`.
   - In production mode, `uvicorn main:app --host=0.0.0.0 --log-config log_conf.yaml --workers 4`.
   - The app will be available at `localhost:8000`. 
   - To expose the app at a different port, use the command line argument `--port {port}` e.g. `uvicorn main:app --reload --log-config=log_conf.yaml --port 5000`.

5. Whenever any resource needs to be updated, re-run the ingestion script for that specific resource with the argument `--drop_index --invalidate_cache` to (1) trigger Redis to flush the old index and create a new one, and (2) re-create on-disk cache for the processed resource. KIV: allow user to add new resource entries without processing everything again and recreatng the index.

## Dockerised installation

1. Build the image, `docker build -t fastapi_app .`.
1. Spin up the container as follows. The app will be available at `localhost:5000` on Docker host.
   ```{bash}
   docker run --name fastapi_app \
      -e APP_ENV=prod \
      -v "$(pwd)/data:/code/data" \
      -p 5000:8000 \
      fastapi_app:latest
   ```
   Notes:
   - `-p 5000:8000` instructs Docker to map port 5000 on Docker host to port 8000 in the container, which is the default FastAPI port. 
   - `-v "$(pwd)/data:/code/data"` mounts the `data` directory in the host machine into the container.
1. To ingest the required resources, one may open a `bash` terminal in the container by executing `docker exec -it fastapi_app bash` and then run the ingestion command as introduced in section ['Required resources'](#required-resources).

## Usage

### Backend

Visit `/docs` to see detailed API documentation. This Swagger UI will show you all available endpoints and allow you to test them directly from the browser.

Overview:

- `POST` `/qa`: given a natural language question, returns the structured data that meet the information need.
- `POST` `/chat`: given the `id` of the QA request, returns a text stream for natural language response.
- `GET` `/ontospecies/species`: given constraints such as chemical classes and physical properties, returns all species that fulfil the requirements.
- `GET` `/ontozeolite/zeolite-frameworks`: given criteria such as XRD peaks and unit cell dimensions, returns all zeolite frameworks that fulfil the requirements.
- `GET` `/ontozeolite/zeolitic-materials`: given criteria such as framework code and an author's family name, returns all zeolitic materials that fulfil the requirements.

### Frontend (to be deprecated)

Zaha's frontend can be accessed at `localhost:5000` in the browser.
