# Deployment

## Architecture

Based on the principle of separation of concerns, the application architecture comprises two main components:

- `fastapi_app`: acts as the point of entry to Marie via a browser-based UI and exposes the APIs for
  - natural language-to-SPARQL translation and domain classification
  - KG execution
  - generation of human-like responses to user queries
  - query for zeolites based on their properties and provenance
  - visualise correlations between two numerical properties
- `triton` server: serves the seq2seq and embedding models that undergird the inference processes. 

## Setup

### Prerequisites

- Docker

### Running

First, update the environment variables in the files `*.env`. Then, build and run the docker images using the following command.

```docker compose -f "docker-compose.local.yaml" up -d --build```