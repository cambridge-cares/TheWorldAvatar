# UK Building Retrofit

This stack provides an interoperable environment for integrating comprehensive UK-wide data on building energy performance, energy consumption, fuel poverty, and public health records. The primary use case is to quantify retrofit urgency and identify optimal retrofit pathways across diverse geographic scales. The stack includes an Ontop SPARQL endpoint for nationwide data analysis, along with a demonstration of 3D visualisation capabilities for selected areas (Cambridge, Ely, and Wisbech), highlighting the potential for localised insights.

Key sections:
- [1. Prerequisites](#1-prerequisites)
- [2. Deployment Instructions](#2-deployment-instructions)
  - [Spinning Up the Stack](#spinning-up-the-stack)
  - [Accessing GUI Endpoints](#accessing-gui-endpoints)
  - [Spinning Up the Stack Remotely via SSH](#spinning-up-the-stack-remotely-via-ssh)
- [3. Uploading Initial Data Using Stack-Data-Uploader](#3-uploading-initial-data-using-stack-data-uploader)
- [4. Ontop SPARQL Endpoint](#4-ontop-sparql-endpoint)
  - [Example Query: Average EPC Ratings by LSOA](#example-query-average-epc-ratings-by-lsoa)
- [5. Troubleshooting](#5-troubleshooting)
- [6. Additional Resources](#6-additional-resources)

&nbsp;

## 1. Prerequisites

Ensure the following software is installed:

- [Git](https://git-scm.com/downloads)
- [Docker Desktop](https://docs.docker.com/get-docker/) or Docker Engine and Docker Compose Plugin
  - On Linux systems, [Podman](https://podman.io/) can be used as an alternative. See `README-podman.md` for further details.

### Access to Docker Registries

You will need access to the Github CARES Docker registry to pull certain images:

1. Test your access to the registry:
    ```bash
    docker login ghcr.io
    ```
2. If prompted, enter your GitHub username and an access token with `read:packages` scope.

For further information, please refer to the [CMCL Docker Registry Wiki](https://github.com/cambridge-cares/TheWorldAvatar/wiki/Docker%3A-Image-registry).

&nbsp;

## 2. Deployment Instructions

### Spinning Up the Stack

1. Clone the repository and navigate to the `StackDeployment` directory:
    ```bash
    git clone <repository-url>
    cd StackDeployment
    ```

2. Copy the `inputs` folder from the stack configuration:
    ```bash
    cp -r Deploy/stacks/UK-building-retrofit/StackDeployment/Stack manager/inputs Deploy/stacks/dynamic/stack-manager/inputs
    ```

3. Log in to the Docker registry if required:
    ```bash
    docker login ghcr.io
    ```

4. Start the stack using Docker Compose with the stack name **CamElyWis-DT**:
    ```bash
    docker-compose -p CamElyWis-DT up -d
    ```

5. Verify that all containers are running:
    ```bash
    docker ps
    ```

### Accessing GUI Endpoints

After the stack is successfully deployed, the following GUI endpoints are accessible via a web browser:
- **Adminer**: Database management
- **Blazegraph**: SPARQL endpoint
- **Ontop**: Multi-scale query endpoint
- **GeoServer**: 3D visualization

The exact endpoints and login details can be found in the [Stack Manager README](Deploy/stacks/dynamic/stack-manager/README.md).

### Spinning Up the Stack Remotely via SSH

For remote deployment, navigate to the `Settings of remote Linux server/README.md` and follow the instructions provided for SSH setup.

&nbsp;

## 3. Uploading Initial Data Using Stack-Data-Uploader

The initial data upload is handled using the **Stack-Data-Uploader** utility. Follow these steps:

1. Navigate to the `Stack-data-uploader` directory:
    ```bash
    cd StackDeployment/Stack-data-uploader
    ```

2. Copy the `inputs` folder to the dynamic stack configuration:
    ```bash
    cp -r config Deploy/stacks/dynamic/stack-data-uploader/config
    ```

3. Run the uploader script to upload initial data:
    ```bash
    python stack_uploader.py --config config/uploader_config.yaml
    ```

&nbsp;

## 4. Ontop SPARQL Endpoint

The stack offers an Ontop SPARQL endpoint to facilitate multi-scale data querying across geographic aggregations such as LSOA (Lower Layer Super Output Areas) and postcodes. This endpoint supports queries on integrated datasets, including energy performance, fuel poverty statistics, and public health metrics.

### Example Query: Average EPC Ratings by LSOA

The following SPARQL query calculates the average Energy Performance Certificate (EPC) rating for buildings within each LSOA:

```sparql
PREFIX ontobuiltenv: <https://www.theworldavatar.com/kg/ontobuiltenv/>
PREFIX building: <https://www.theworldavatar.com/kg/ontobuiltenv/building>
PREFIX GeoSparql: <http://www.opengis.net/ont/geosparql#>
PREFIX rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#>

SELECT ?lsoaCode (AVG(?energyEfficiency) AS ?averageEPC)
WHERE {
  ?postcodeEntity GeoSparql:sfWithin ?lsoaEntity ;
                  ontobuiltenv:hasPostcode ?postcode .
  ?lsoaEntity ontobuiltenv:hasIdentifier ?lsoaCode .
  ?building ontobuiltenv:hasPostcode ?postcode ;
            building:hasEnergyEfficiency ?energyEfficiency .
}
GROUP BY ?lsoaCode

```
This query retrieves the average energy efficiency ratings for all buildings in a selected area, grouped by LSOA code. It demonstrates the endpoint’s capability for multi-scale aggregation, allowing users to perform analyses at different geographic levels.

Further query templates can be found in the Deploy/stacks/UK-building-retrofit/StackDeployment/SPARQL queries directory, covering a range of metrics such as energy consumption, fuel poverty rates, and public health data.


