# Deployment for Question-Answering Applications

## Deployment with Docker Compose (recommended)

1. Fill in the environment variable values in [fastapi-variables.env](fastapi-variables.env). Their descriptions can be found in [fastapi_app/README.md](fastapi_app/README.md#steps).
2. Populate the required data for the application as per the [Required data section](fastapi_app/README.md#required-data).
3. Spin up the services with Docker Compose.
   ```{bash}
   docker compose -f "deployment/docker-compose.yaml" up -d --build
   ```

Zaha frontend can then be accessed at `localhost:5000` in the browser. For the specifications of backend APIs, visit `localhost:5000/docs`.

## Manual setup of services

Follow the instructions in [triton_inference_server/README.md](triton_inference_server/README.md) and [fastapi_app/README.md](fastapi_app/README.md).