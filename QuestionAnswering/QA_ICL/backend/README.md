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

### Prerequisites

- Docker (Docker Engine, Docker CLI, and Docker Compose)

### Steps

1. Prepare the `fastapi_app`:
   
   Deposit the required data and configure parameters as specified in:

    - [Required resources](./fastapi_app/README.md#required-resources)
    - [Configurable parameters](./fastapi_app/README.md#configurable-parameters).

1. Prepare the `triton_inference_server`:
   
   Deposit text embedding model weights as described in [Required resources](./triton_inference_server/README.md#required-resources).

1. Execute the deployment script:
   
   ```bash
   sh deploy.sh
   ```

   This script executes two commands:

   1. Spin up the services defined in `docker-compose.yaml`.
   2. Ingest all datasets located under [`fastapi/data/`](fastapi_app/data/) into Redis.

   After successful deployment, the application will be available at `localhost:5000`.


## Usage

Visit `localhost:5000/docs` to see detailed API documentation. This interactive Swagger UI provides detailed information about available endpoints, request/response formats, and allows you to test the API directly from your browser.
