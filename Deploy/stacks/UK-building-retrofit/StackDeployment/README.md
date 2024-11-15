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
  - On Linux systems, [Podman](https://podman.io/) can be used as an alternative.

### Access to Docker Registries

You will need access to the Github CARES Docker registry to pull certain images with your github account and personal access token. Please ensure your personal access token has read:packages scope.

1. Test your access to the registry:
    ```bash
    docker login ghcr.io
    ```
2. If prompted, enter your GitHub username and an access token.

&nbsp;

## 2. Deployment Instructions

### Spinning Up the Stack

Before spinning up the stack using the [Stack manager], please provide the following files to the specified repositories:

-  Four secret files in `./inputs/stack-manager/inputs/secrets`:
    - `postgis_password`
    - `geoserver_password`
    - `mapbox_username`
    - `mapbox_api_key`

Subsequently, copy all files in `./Stack-manager/inputs` into their corresponding repositories of the stack-manager tool under `Deploy/stacks/dynamic/stack-manager/`.

Then navigate to `Deploy/stacks/dynamic/stack-manager` and run the following command there from a *bash* terminal. There are several [common stack scripts] provided to manage the stack:

```bash
# Start the stack (please note that this might take some time)
bash ./stack.sh start Cam-Ely-Wis-DT
# Stop the stack
bash ./stack.sh stop Cam-Ely-Wis-DT
# Remove the stack (incl. volumes)
bash ./stack.sh remove Cam-Ely-Wis-DT -v
# Remove individual service
bash ./stack.sh remove Cam-Ely-Wis-DT <service name>
```

### Spinning Up the Stack Remotely via SSH

For remote deployment, navigate to the `Settings of remote Linux server/README.md` and follow the instructions provided for SSH setup.

&nbsp;

After spinning up the stack, the GUI endpoints to the running containers can be accessed via Browser (i.e. adminer, blazegraph, ontop, geoserver). The exact endpoints and login details can be found in the [Stack Manager README](Deploy/stacks/dynamic/stack-manager/README.md).

## 3. Uploading Initial Data Using Stack-Data-Uploader

The following steps explain how to upload the data to the stack using the [Stack data uploader] (please see the referenced README for more details):

1) Navigate to the `Deploy/stacks/UK-building-retrofit/StackDeployment/Stack-data-uploader` directory:
    a) Copy the configuration files from the `Deploy/stacks/UK-building-retrofit/StackDeployment/Stack-data-uploader/config` directory to the matching directory in `Deploy/stacks/dynamic/stack-data-uploader/inputs/config/`

    b) Replace the `readme.txt` files in the `Deploy/stacks/UK-building-retrofit/StackDeployment/Stack-data-uploader/data*` sub-folders with the referenced data files

    c) Copy all data sub-directories from the `./inputs/stack-data-uploader/data` directory into the matching parent directory in `Deploy/stacks/dynamic/stack-data-uploader/inputs/data/`

2) Navigate to `Deploy/stacks/dynamic/stack-data-uploader`, and run the following command there from a *bash* terminal and wait until container has stopped again (i.e. the upload has finished). Specified Blazegraph/Geoserver/... namespaces will be created automatically by the uploader if not already exist.
    ```bash
    bash ./stack.sh start Cam-Ely-Wis-DT
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

<!-- Links -->
[allows you to publish and install packages]: https://docs.github.com/en/packages/working-with-a-github-packages-registry/working-with-the-apache-maven-registry#authenticating-to-github-packages
[Container registry on Github]: https://github.com/orgs/cambridge-cares/packages
[Create SSH key]: https://docs.digitalocean.com/products/droplets/how-to/add-ssh-keys/create-with-openssh/
[Environment Agency]: https://environment.data.gov.uk/flood-monitoring/doc/reference
[forwarding the port]: https://code.visualstudio.com/docs/remote/ssh#_forwarding-a-port-creating-ssh-tunnel
[OS Features API]: https://api.os.uk/features/
[personal access token]: https://docs.github.com/en/github/authenticating-to-github/creating-a-personal-access-token
[VSCode via SSH]: https://code.visualstudio.com/docs/remote/ssh
[Upload SSH key]: https://docs.digitalocean.com/products/droplets/how-to/add-ssh-keys/to-existing-droplet/
[MetOffice My Account]: https://register.metoffice.gov.uk/MyAccountClient/account/view

<!-- Stack references -->
[common stack scripts]: https://github.com/cambridge-cares/TheWorldAvatar/tree/main/Deploy/stacks/dynamic/common-scripts
[Stack data uploader]: https://github.com/cambridge-cares/TheWorldAvatar/tree/main/Deploy/stacks/dynamic/stack-data-uploader
[Stack manager]: https://github.com/cambridge-cares/TheWorldAvatar/blob/main/Deploy/stacks/dynamic/stack-manager/README.md


