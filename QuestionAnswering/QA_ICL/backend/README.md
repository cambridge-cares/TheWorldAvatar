# Marie's Backend

## Table of Contents

- [Marie's Backend](#maries-backend)
  - [Table of Contents](#table-of-contents)
  - [Overview](#overview)
  - [Architecture](#architecture)
  - [Deployment](#deployment)
    - [Prerequisites](#prerequisites)
    - [Steps](#steps)
  - [Usage](#usage)


## Overview

Marie's Backend is designed to handle natural language queries for chemistry data and provide search capabilities for chemical species and zeolites.

## Architecture

The backend consists of two main components:

- [`fastapi_app`](./fastapi_app/) exposes APIs for:
  - querying chemistry data via natural language
  - searching for chemical species and zeolites based on specific filtering criteria
- [`triton_inference_server`](./triton_inference_server/) serves the Sentence-BERT model for generating text embeddings.


## Deployment

This guide is for deploying the backend services using Docker containers in a production environment.

### Prerequisites

- Docker (Docker Engine, Docker CLI, and Docker Compose)

### Steps

1. Prepare the Triton Inference Server:
   Deposit the text embedding model weights for the Sentence-BERT model in `triton_inference_server/model_repository/mpnet/1/`. 
   For more details, see [Triton Server README](./triton_inference_server/README.md#required-resources).

2. Prepare the `fastapi_app`:
   
   - Deposit the [required data](./fastapi_app/README.md#required-resources) in the following locations:
     - Lexicon files: `fastapi_app/data/lexicon/`
     -  Schema property files: `fastapi_app/data/schema/properties/`
     -  Semantic parsing example files: `fastapi_app/data/nlq2datareq_examples/`
     -  Quantity recognition example files: `fastapi_app/data/qtRecog_examples/`
  
    - Configure parameters by creating `fastapi_app/app.yaml` according to [Configurable parameters](./fastapi_app/README.md#configurable-parameters) section.

3. Execute the deployment script:
   
   ```bash
   sh deploy.sh
   ```

   This script executes two commands:

   1. Spin up the services defined in `docker-compose.yaml`.
   2. Ingest all datasets located under [`fastapi/data/`](fastapi_app/data/) into Redis.

   After successful deployment, the application will be available at `localhost:5000`.

Note: For development mode deployment of the FastAPI app, please refer to FastAPI App [README](./fastapi_app/README.md).

## Usage

Visit `localhost:5000/docs` to see detailed API documentation. This interactive Swagger UI provides detailed information about available endpoints, request/response formats, and allows you to test the API directly from your browser.
