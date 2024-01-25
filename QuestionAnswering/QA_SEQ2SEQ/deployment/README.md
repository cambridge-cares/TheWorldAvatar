# Deployment

## Architecture

Based on the principle of separation of concerns, the application architecture comprises two main components:

- `Flask` app: acts as the point of entry to Marie via a browser-based UI and exposes the APIs for (1) natural language-to-SPARQL translation and domain classification, (2) KG execution, and (3) generation of human-like responses to user queries.
- `Triton` server: serves the seq2seq and embedding models that undergird the inference processes. 
- `llama-cpp-python` server: serves an instruction-tuned LLM for generating chatbot response. 

## Setup

### Prerequisites

- Docker

### Running

First, update the environment variables in the files `*.env`. Then, build and run the docker images using the following command.

```docker compose -f "docker-compose.local.yaml" up -d --build```