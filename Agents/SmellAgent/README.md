# Smell Agent

SmellAgent is an IoT sensor Python Agent designed to detect and categorise smells. It reads data from InfluxDB and leverages a trained machine learning model to predict the type of smell.

## Table of Contents

- [Smell Agent](#smell-agent)
  - [Table of Contents](#table-of-contents)
  - [Installation](#installation)
  - [Usage](#usage)
  - [Docker Deployment](#docker-deployment)
- [Author](#author)
## Installation

1. Install the required dependencies:

`pip install -r requirements.txt`

## Usage

`python -m smellagent.entry_point`

The server will be accessible at `http://localhost:9047`.

## Docker Deployment

SmellAgent can be run using Docker via Docker Compose:

`docker-compose up`

# Author
Jiawei Lai (jlai@cmcl.io), August 2023