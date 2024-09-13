# QA Application Backend

## Initial setup

### Required services

- A Redis server (see [Run Redis Stack in Docker](https://redis.io/docs/latest/operate/oss_and_stack/install/install-stack/docker/))
- A text embedding service exposed via either OpenAI-compatible or NVIDIA's Triton API. For running a local Triton server in Docker, see [triton_inference_server](../triton_inference_server/).
- A chat completion service exposed via OpenAI-compatible API.
- (Optional) LocationIQ geocoding service. 
  - Context: Some of Zaha's features require geocoding i.e. the determination of geographical coordinates based on a location name. These include functionalities such as finding pollutant concentrations or the nearest carpark given a location name. By default the app will preferentially use the [API key-free geocoding service by Nominatim](https://nominatim.org/release-docs/latest/api/Search/). However, Nominatim does impose a rate limit (read more about [Nominatim's usage policy](https://operations.osmfoundation.org/policies/nominatim/)). 
  - To avoid having geocoding requests denied, the app will also make requests to [LocationIQ](https://locationiq.com/), which requires an API key. At the time of writing, LocationIQ does offer a free plan for API access.
  - KIV: [set up a local Nominatim instance](https://nominatim.org/release-docs/latest/admin/Installation/) to remove dependency on external geocoding services.

### Required datasets

Datasets required by this app are:

- [`data/lexicon`](data/lexicon/): JSON files for lexicons of entities that require linking with Redis. Each file is an array of `Lexicon` objects.
- [`data/schema/properties`](data/schema/properties): JSON files for information on KG predicates. Each file is an array of `GraphItemType` objects.
- [`data/nlq2datareq_examples`](data/nlq2datareq_examples/): JSON files for semantic parsing examples. Each file is an array of `Nlq2DataReqExample` objects.
- [`data/qtRecog_examples`](data/qtRecog_examples/): JSON files for quantity recognition examples. Each file is an array of `QtRecogExample` objects.

All JSON schemas are defined in [../../data_generation/README.md](../../data_generation/README.md#schema-definitions).

To facilitate efficient on-demand retrieval, the datasets need to be ingested into the Redis server before the app starts accepting requests from users. The module [`ingest_data`](./ingest_data/) provides the utilities to run automated ingestion. Execute `python -m ingest_data --help` to see a list of all available command-line options.

Example execution:
```
python -m ingest_data --redis_host localhost --text_embedding_backend triton --text_embedding_url localhost:8001 --drop_index --invalidate_cache
```

Each dataset can also be ingested separately.
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

1. Follow the points in the section [Initial setup](initial-setup) to ensure that the required services and resources are properly configured.

1. Create conda environment and activate it.
   ```{bash}
   conda create --name qa_backend python==3.10
   conda activate qa_backend
   ```

1. Install dependencies.
   ```{bash}
   pip install -r requirements.txt
   ```

1. Ingest required resources into Redis server, as per the section ['Required resources'](#required-resources).

1. Start the server:
   - In debug mode (app is automatically reloaded upon code changes), `uvicorn main:app --reload --log-config=log_conf.yaml`.
   - In production mode, `uvicorn main:app --host=0.0.0.0 --log-config log_conf.yaml --workers 4`.
   - The app will be available at `localhost:8000`.  
   <!-- TODO: add a health check endpoint -->
   - To expose the app at a different port, use the command line argument `--port {port}` e.g. `uvicorn main:app --reload --log-config=log_conf.yaml --port 5000`.

1. Whenever any dataset needs to be updated, re-run the ingestion script for that specific dataset with the argument `--drop_index --invalidate_cache` to (1) trigger Redis to flush the old index and create a new one, and (2) re-create on-disc cache for the processed datasets. KIV: allow user to add new resource entries without processing everything again and recreatng the index.

## Docker installation

1. Follow the points in the section [Initial setup](initial-setup) to ensure that the required services and resources are properly configured.
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


## Developer's documentation

### Application breakdown

```mermaid
classDiagram
   class DataSupporter {
      query(query)
   }
   DataSupporter ..> NlqRewriter
   DataSupporter ..> Nlq2DataReqContextRetriever
   DataSupporter ..> Nlq2DataReqLLMCaller
   DataSupporter ..> CentralEntityLinker
   DataSupporter ..> DataReqExecutor
   DataSupporter ..> QARequestArtifactStore
   DataSupporter ..> VisualisationDataStore

   NlqRewriter ..> QtNormaliser

   class QtNormaliser {
      openai_client
      normalise(text)
   }
   QtNormaliser ..> QtRecogExampleStore

   class QtRecogExampleStore {
      examples
      retrieve()
   }

   class Nlq2DataReqContextRetriever {
      example_num
      relation_num
      retrieve(nlq)
   }
   Nlq2DataReqContextRetriever ..> Nlq2DataReqExampleStore
   Nlq2DataReqContextRetriever ..> SchemaStore

   class Nlq2DataReqExampleStore {
      retrieve_examples(nlq, k)
   }
   Nlq2DataReqExampleStore ..> Redis
   Nlq2DataReqExampleStore ..> IEmbedder

   <<interface>> IEmbedder
   IEmbedder: __call__(documents)

   class SchemaStore {
      retrieve_relations(nlq, k)
   }
   SchemaStore ..> Redis
   SchemaStore ..> IEmbedder

   class Nlq2DataReqLLMCaller {
      openai_client
      forward(nlq, translation_context)
   }

   class CentralEntityLinker {
      vss_threshold
      cls2elconfig
      link_exact(surface_form, cls)
      link_semantic(surface_form, cls, k)
      link_fuzzy(surface_form, cls, k)
   }
   CentralEntityLinker ..> Redis
   CentralEntityLinker ..> IEmbedder
   CentralEntityLinker "1" --o "many" LinkEntity

   <<interface>> LinkEntity
   LinkEntity: __call__(text)

   class DataReqExecutor {
      exec(req_form, ...)
   }
   DataReqExecutor ..> SparqlDataReqExecutor
   DataReqExecutor ..> FuncDataReqExecutor
   DataReqExecutor ..> FallbackDataReqExecutor

   class SparqlDataReqExecutor {
      ns2kg
      exec(req_form, ...)
   }
   SparqlDataReqExecutor ..> SparqlQueryProcessor
   SparqlDataReqExecutor ..> SparqlResponseTransformer

   class SparqlQueryProcessor {
      process(sparql, ...)
   }

   class SparqlResponseTransformer {
      transform(vars, bindings, ...)
   }

   class FuncDataReqExecutor {
      name2func
      exec(req_form, ...)
   }

   class FallbackDataReqExecutor {
      exec(...)
   }

   class QARequestArtifactStore {
      save(artifact)
      load(id)
   }
   QARequestArtifactStore ..> Redis

   class VisualisationDataStore {
      get(cls, iris)
   }
```

### /qa endpoint handling

```mermaid
sequenceDiagram
   participant client as API consumer
   participant controller as DataSupporter
   participant nlq_rewriter as NlqRewriter
   participant context_retriever as Nlq2DataReqContextRetriver
   participant llm_caller as Nlq2DataReqLLMCaller
   participant entity_store as CentralEntityLinker
   participant executor as DataReqExecutor
   participant vis_data_store as VisualisationDataStore

   client->>controller: Submits natural language<br/>query to /qa endpoint
   activate controller
   
   controller->>nlq_rewriter: Requests query rewrite
   activate nlq_rewriter
   nlq_rewriter->>controller: Returns rewritten query
   deactivate nlq_rewriter

   controller->>context_retriever: Requests relevant semantic parsing<br/>examples and KG schema elements
   activate context_retriever
   context_retriever->>controller: Returns requested resources
   deactivate context_retriever

   controller->>llm_caller: Feeds input query, semantic parsing examples, and<br/>KG schema elements into LLM to perform semantic parsing
   activate llm_caller
   llm_caller->>controller: Returns parsed data request
   deactivate llm_caller

   controller->>entity_store: Requests entity linking for ungrounded nodes
   activate entity_store
   entity_store->>controller: Returns IRIs for each ungrounded node
   deactivate entity_store

   controller->>executor: Forwards data request to execution engine
   activate executor
   executor->>controller: Returns execution results
   deactivate executor

   controller->>vis_data_store: Requests data for visualisation
   activate vis_data_store
   vis_data_store->>controller: Returns data for visualisation
   deactivate vis_data_store

   controller->>client: Returns data for answering<br/>and visualisation
   deactivate controller
```
