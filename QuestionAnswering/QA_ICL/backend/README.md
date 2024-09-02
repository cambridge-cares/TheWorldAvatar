# Marie Backend

## Table of Contents
- [Marie Backend](#marie-backend)
  - [Table of Contents](#table-of-contents)
  - [Overview](#overview)
  - [Architecture](#architecture)
  - [Deployment](#deployment)
    - [Docker Deployment (Recommended)](#docker-deployment-recommended)
      - [Prerequisites](#prerequisites)
      - [Steps](#steps)
    - [Manual Setup](#manual-setup)
  - [Usage](#usage)
  - [API Documentation](#api-documentation)


## Overview
Marie Backend is designed to handle natural language queries for chemistry data and provide search capabilities for chemical species and zeolites.

## Architecture

The backend consists of two main components:

- [`fastapi_app`](./fastapi_app/) A FastAPI application that exposes APIs for:
  - querying chemistry data via natural language
  - searching for chemical species and zeolites based on specific filtering criteria
- [`triton_inference_server`](./triton_inference_server/) Serves the Sentence-BERT model for text embedding and inference.

## Deployment

### Docker Deployment (Recommended)

#### Prerequisites

- Docker

#### Steps

1. Prepare the `fastapi_app`:
   - Populate required data and configure parameters as specified in:
     - [Required resources](./fastapi_app/README.md#required-resources)
     - [Configurable parameters](./fastapi_app/README.md#configurable-parameters)

2. Set up the `triton_inference_server`:
   - Deposit text embedding model weights as described in:
     - [Required resources](./triton_inference_server/README.md#required-resources)

3. Execute the deployment script:
   ```bash
   sh deploy.sh
   ```
After successful deployment, the application will be available at `localhost:5000`.

### Manual Setup

For manual installation and setup, please refer to the README files of individual components:
- [triton_inference_server setup](triton_inference_server/README.md)
- [fastapi_app setup](fastapi_app/README.md)

## Usage

For detailed instructions on how to interact with the application, please refer to the [Usage section](./fastapi_app/README.md#usage) in the fastapi_app README.

## API Documentation

Once the application is running, you can access the API documentation at:
`localhost:5000/docs`

This interactive documentation provides detailed information about available endpoints, request/response formats, and allows you to test the API directly from your browser.
