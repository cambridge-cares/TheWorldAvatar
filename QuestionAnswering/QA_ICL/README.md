# Question-Answering System for The World Avatar

## Table of Contents
- [Question-Answering System for The World Avatar](#question-answering-system-for-the-world-avatar)
  - [Table of Contents](#table-of-contents)
  - [Introduction](#introduction)
    - [Key Features](#key-features)
  - [Architecture overview](#architecture-overview)
  - [Key Components](#key-components)
  - [Project Structure](#project-structure)
  - [Getting Started](#getting-started)
    - [Prerequisities](#prerequisities)
    - [Installation](#installation)
    - [API Documentation](#api-documentation)
    - [Usage](#usage)

## Introduction

The Question-Answering System for The World Avatar involves retrieving data from RDF graphs and other data sources such as HTTP endpoints. To do so, input questions need to be converted into SPARQL queries and HTTP requests, whose execution would yield the desired data. The conversion of natural language queries to data requests is facilitated by in-context learning (ICL), which entails engineering a text prompt for Large Language Models (LLMs) to automatically perform the transformation. The prompt may include context information such as parsing examples and the structure of target predictions.

### Key Features
- Utilises in-context learning for query transformation
- Supports multiple data sources including RDF graphs and HTTP endpoints
- Employs LLMs for natural language understanding
- Provides a user-friendly frontend interface
- Offers efficient backend processing with caching and inference capabilities
- Includes tools for data generation and preparation

## Architecture overview

The architecture of the project consists of a frontend (user interface), a backend (handling queries, model inference, and Knowledge Graph access), and additional data generation modules. These components communicate through RESTful APIs and other protocols. 

```mermaid
graph TD
    subgraph Frontend
    A[Next.js app]
    end

    subgraph Backend
    C[FastAPI app]
    E[(Redis)]
    F[Triton inference server]
    end

    D[(Blazegraph servers)]

    A <--> |HTTP| C
    D <--> |SPARQL protocol| C
    C <--> |REST| E
    C <--> |gRPC| F

    classDef database fill:#f9f,stroke:#333,stroke-width:2px;
    class D,E database;
```

## Key Components
1. **Frontend**: A Next.js application serving as the user interface.
2. **Backend**: A FastAPI application handling core logic and data processing.
    - **Redis**: Caches frequently accessed data for improved performance.
    - **Triton Inference Server**: Manages machine learning model inference.
3. **Blazegraph Servers**: Store and manage RDF data, accessible via SPARQL protocol.

## Project Structure

The project is organised into the following main directories:

- `data_generation/`: Python scripts for entity linking and ICL data preparation. The scripts generate three kinds of data:
  - Lexicon: A collection of ontology representations in texts for classes, predicates, or entities.
  - Simplified graph schema: Captures node types, edge types, and relation types.
  - Examples of data request: Pairs of natural language queries and their corresponding data requests for ICL.
- `backend/`: The backend, called Marie Backend, it contains the FastAPI application serving as the system's backend. It consists of two main components:
  - `fastapi_app`: A FastAPI application.
  - `triton_inference_server`: Serves the Sentence-BERT model for text embedding and inference.
- `frontend/`: The frontend, called Marie Frontend, serves as the user interface for interacting with the backend services. Built using Next.js, it communicates with the backend to perform operations such as querying chemical species and filtering zeolites, and displays the results to users in a user-friendly manner.

## Getting Started

### Prerequisities
 - Docker (recommended for deployment)
 - Python 3.8 or higher
 - Node.js and npm (for frontend development)

### Deployment Setup
1. Clone the TWA repository and go to the QA_ICL folder

    ```
    cd TheWorldAvatar/QuestionAnswering/QA_ICL/
    ```
2. Backend Setup
   - Follow steps [here](https://github.com/cambridge-cares/TheWorldAvatar/tree/main/QuestionAnswering/QA_ICL/backend#docker-deployment-recommended), which covers the following topics
      - Populate data for `Fastapi App` and `triton`
      - Ingest data to `Fastapi App` and `triton`
      - Launch `Fastapi App` and `triton` containers
   - Configuration
     - Fastapp Credentials: create file `app.local.yaml` as instructed [here](https://github.com/cambridge-cares/TheWorldAvatar/tree/main/QuestionAnswering/QA_ICL/backend/fastapi_app#configurable-parameters) and provide credentials
     - Port mapping: [docker-compose.yaml](./backend/docker-compose.yaml)
3. Frontend Setup
   - Follow steps [here](https://github.com/cambridge-cares/TheWorldAvatar/tree/main/QuestionAnswering/QA_ICL/frontend/next_app_marie#deployment-via-docker) to deploy `NextApp` as docker container
     - Build image
     - Launch `NextApp` container
   - Configuration
     - `BASE_PATH`: [.env.production](./frontend/next_app_marie/.env.production)
     - Backend endpoint: [.env](./frontend/next_app_marie/.env)
4. Nginx Redirection Setup
   - Create redirection for the backend endpoint. Trailing forward slash `/` is allowed.
      ```
      location /BACKEND_PATH/ {
        proxy_pass          http://HOST:PORT/PATH/;
        ...
      }
      ```
   - Create redirection for the frontend endpoint. Trailing forward slash `/` is **NOT allowed**.
      ```
      # correct
      location /FRONTEND_PATH {
        proxy_pass          http://HOST:PORT/PATH;
        ...
      }

      # wrong
      location /FRONTEND_PATH/ {
        proxy_pass          http://HOST:PORT/PATH/;
        ...
      }
      ```
   - Update backend entpoint configuration in NextApp `.env` file to the redirection address
   - Rebuild Image for `NextApp` and launch the container
   - Troubleshoot
     - CORS error when submitting question: 
       - This is caused by incorrect setting of the backend redirection


### Development Setup
1. Clone the TWA repository and go to the QA_ICL folder

    ```
    cd TheWorldAvatar/QuestionAnswering/QA_ICL/
    ```

2. Setup the backend: 
   - This is a summary of the backend setup instructions. More details can be found in [backend/README.md](./backend/README.md)
   - Data Preparation
      - Follow the instructions in `backend/README.md` to prepare required data and configure parameters.
   - Triton Setup
      - Follow instructions [here](https://github.com/cambridge-cares/TheWorldAvatar/blob/main/QuestionAnswering/QA_ICL/backend/triton_inference_server/README.md#development)
   - FastApi Setup
      - Create a virtual environment (recommended):
        ```
        conda create --name qa_backend python==3.10
        conda activate qa_backend
        ```
        Install relavent dependencies
        ```
        pip install -r requirements.txt
        ```
      - Provide credentials as stated [here](https://github.com/cambridge-cares/TheWorldAvatar/tree/main/QuestionAnswering/QA_ICL/backend/fastapi_app#configurable-parameters)
      - Ingest data to `Redis`
        ```
        python -m ingest_data --redis_host localhost --text_embedding_backend triton --text_embedding_url localhost:8001 --drop_index --invalidate_cache
        ```
      - Start server as stated [here](https://github.com/cambridge-cares/TheWorldAvatar/tree/main/QuestionAnswering/QA_ICL/backend/fastapi_app#steps)

3. Set up the frontend:
   - Navigate to the `frontend/next_app_marie` directory.
   - Local Install:
      ```
      npm install
      ```
    - Configure the backend endpoint in the `.env` file.
    - More information can be found at [here](https://github.com/cambridge-cares/TheWorldAvatar/tree/main/QuestionAnswering/QA_ICL/frontend/next_app_marie#development)
4. Set up the data generation environment:
    - Create a virtual environment (recommended):
      ```
      conda create --name data-env python=3.8
      conda activate data-env
      ``` 

    - Install dependecies:
      ```
      cd data_generation
      pip install -r requirements.txt
      ```


### API Documentation
Once the backend is running, you can access the API documentation at: `http://localhost:5000/docs`.

This interactive documentation provides detailed information about available endpoints, request/response formats, and allows you to test the API directly from your browser.

### Usage
1. Start the backend services.
2. Start the frontend development servers.
3. Access the application at `http://localhost:3000` (or the port specified in your frontend configuration).