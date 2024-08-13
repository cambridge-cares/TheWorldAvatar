# Marie Backend

## Architecture

- [`fastapi_app`](./fastapi_app/) exposes APIs for:
  - querying chemistry data via natural language
  - searching for chemical species and zeolites based on some filtering criteria
- [`triton_inference_server`](./triton_inference_server/) serves Sentence-BERT model.

## Deployment with Docker (recommended)

### Prerequisites

- Docker

### Steps

1. Populate the required data and config parameters for the `fastapi_app` according to its README sections ['Required resources'](./fastapi_app/README.md#required-resources) and ['Configurable parameters'](./fastapi_app/README.md#configurable-parameters).
2. Execute the deployment script.
   ```{bash}
   sh deploy.sh
   ```

The app will be available at `localhost:5000`. See `fastapi_app`'s section [Usage](./fastapi_app/README.md#usage) for how to interact with the app.

## Manual setup

Follow the instructions in [triton_inference_server/README.md](triton_inference_server/README.md) and [fastapi_app/README.md](fastapi_app/README.md).