# Deployment for Marie

## Architecture

Based on the principle of separation of concerns, the application architecture comprises two main components:

- Flask app: serves as the point of entry to Marie via a browser-based UI
- Triton server: exposes the translation model

Additionally, the Flask app has a dependency on the knowledge graph server that hosts the OntoSpecies triples.

## Local deployment (Docker)

1. Update the environment variables in `triton-variables.env`.

1. Spin up the services ` docker compose -f "docker-compose.local.yaml" up -d --build`.

1. Check that the containers are running. The app will be running at `localhost:5000`.

   Note: The triton translation server might take some time to initialise, which might cause the flask app to automatically shut down when it pings the triton server and the latter is not ready yet. Therefore, please restart the flask app if necessary. 

## Production deployment (on Digital Ocean)

On the server's machine, ensure that

- [Docker](https://www.digitalocean.com/community/tutorials/how-to-install-and-use-docker-on-ubuntu-20-04) and [Docker Compose](https://www.digitalocean.com/community/tutorials/how-to-install-and-use-docker-compose-on-ubuntu-20-04) are installed `docker --version && docker-compose --version`, and
- The firewalls' rules have been configured to expose port 5000.

Afterwards, clone the repo (spare checkout recommended) and follow the [local deployment steps](#local-deployment-docker).

### [DEPRECATED] Deployment with docker images from registry

1. Log in to CARES' Github Docker registry `docker login ghcr.io/cambridge-cares`.

1. Create a docker compose file named `docker-compose.yaml` and 
copy the content of [`docker-compose.prod.yaml`](./docker-compose.prod.yaml) over.

1. Spin up the services `docker compose up -d --build`.

1. Check that the containers are running with `docker-compose ps`. The app will be running at `<public-ip-address>:5000`.
   
   Note: The triton translation server might take some time to initialise, which might cause the flask app to automatically shut down when it pings the triton server and the latter is not ready yet. Therefore, please restart the flask app if necessary. 
