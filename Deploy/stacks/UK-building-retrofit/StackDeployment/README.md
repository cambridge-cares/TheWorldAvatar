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
- [5. Feature Info Agent](#5-feature-info-agent)

&nbsp;

## 1. Prerequisites

Ensure the following software is installed:

- [Git](https://git-scm.com/downloads)
- [Docker Desktop](https://docs.docker.com/get-docker/) or Docker Engine and Docker Compose Plugin

### Access to Docker Registries

You will need to access the Github CARES Docker registry to pull images with your github account and personal access token. Please ensure your personal access token has read:packages scope.

1. Test your access to the registry:
    ```bash
    docker login ghcr.io
    ```
2. If prompted, enter your GitHub username and an access token.

&nbsp;

## 2. Deployment Instructions

### Spinning Up the Stack

Before spinning up the stack using the [Stack manager], please provide the following files to the specified folder:

-  Four secret files in `./inputs/stack-manager/inputs/secrets`:
    - `postgis_password`
    - `geoserver_password`
    - `mapbox_username`
    - `mapbox_api_key`

Subsequently, copy all files in `./Stack-manager/inputs` into their corresponding repositories of the stack-manager tool under `Deploy/stacks/dynamic/stack-manager/`.

Then navigate to `Deploy/stacks/dynamic/stack-manager` and run the following command there from a *bash* terminal. There are several [common stack scripts] provided to manage the stack:

```bash
# Start the stack (please note that this might take some time)
bash ./stack.sh start CamElyWis-DT
# Stop the stack
bash ./stack.sh stop CamElyWis-DT
# Remove the stack (incl. volumes)
bash ./stack.sh remove CamElyWis-DT -v
# Remove individual service
bash ./stack.sh remove CamElyWis-DT <service name>
```
After spinning up the stack, the GUI endpoints to the running containers can be accessed via Browser (i.e. adminer, blazegraph, ontop, geoserver). The exact endpoints and login details can be found in the [Stack Manager README](https://github.com/TheWorldAvatar/stack/blob/main/stack-manager/README.md).

### Spinning Up the Stack Remotely via SSH

To deploy the stack remotely via SSH, use VSCode's built-in SSH support. Follow the steps in [VSCode via SSH] to connect to a remote machine (e.g., a virtual machine on Digital Ocean) and start deployment. Regular log in relies on username and password, You can also consider generating an [SSH key] and uploading it via [Upload SSH key] to enable automatic authentication and eliminate repeated credential prompts. After logging in, [git clone] a remote copy of [The World Avatar] repository and follow the provided instructions to deploy the stack.

To access deployed containers through exposed endpoints (e.g., `http://<host IP>:3840/ontop/ui`), ensure the necessary ports are open on the remote machine. Please request port changes through your server administrator—avoid directly modifying firewall rules on individual droplets using tools like `ufw` or `iptables`.

When interacting with the GeoServer GUI remotely, some issues may arise (e.g., inability to remove layers or edit CRS information). To address this, consider [forwarding the port] used by the stack to your local machine after establishing an [SSH tunnel]. This will make GeoServer accessible at `http://localhost:{port}/geoserver/` instead of a remote address like `http://<host IP>:{port}/geoserver/`. 

## 3. Uploading Initial Data Using Stack-Data-Uploader

The following steps explain how to upload the data to the stack using the [Stack data uploader] (please see the referenced README for more details):

1) Navigate to the `Deploy/stacks/UK-building-retrofit/StackDeployment/Stack-data-uploader` directory:
    a) Copy the configuration files from the `Deploy/stacks/UK-building-retrofit/StackDeployment/Stack-data-uploader/config` directory to the matching directory in `Deploy/stacks/dynamic/stack-data-uploader/inputs/config/`

    b) Replace the `readme.txt` files in the `Deploy/stacks/UK-building-retrofit/StackDeployment/Stack-data-uploader/data*` sub-folders with the referenced data files

    c) Copy all data sub-directories from the `./inputs/stack-data-uploader/data` directory into the matching parent directory in `Deploy/stacks/dynamic/stack-data-uploader/inputs/data/`

2) Navigate to `Deploy/stacks/dynamic/stack-data-uploader`, and run the following command there from a *bash* terminal and wait until container has stopped again (i.e. the upload has finished). Specified Blazegraph/Geoserver/... namespaces will be created automatically by the uploader if not already exist.
    ```bash
    bash ./stack.sh start CamElyWis-DT
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

## 5. Feature Info Agent
The [Feature Info Agent] serves as an access point for the visualisation, enabling queries about data for individual features (e.g., specific geographical locations). In this case, the Feature Info Agent retrieves energy performance and structural descriptions for buildings by querying the Ontop endpoint using the building's IRI. The queries templates are located in [fia_queries].  

<!-- Links -->
[allows you to publish and install packages]: https://docs.github.com/en/packages/working-with-a-github-packages-registry/working-with-the-apache-maven-registry#authenticating-to-github-packages
[Container registry on Github]: https://github.com/orgs/cambridge-cares/packages
[SSH key]: https://docs.digitalocean.com/products/droplets/how-to/add-ssh-keys/create-with-openssh/
[forwarding the port]: https://code.visualstudio.com/docs/remote/ssh#_forwarding-a-port-creating-ssh-tunnel
[OS Features API]: https://api.os.uk/features/
[personal access token]: https://docs.github.com/en/github/authenticating-to-github/creating-a-personal-access-token
[VSCode via SSH]: https://code.visualstudio.com/docs/remote/ssh
[Upload SSH key]: https://docs.digitalocean.com/products/droplets/how-to/add-ssh-keys/to-existing-droplet/
[MetOffice My Account]: https://register.metoffice.gov.uk/MyAccountClient/account/view
[The World Avatar]: https://github.com/cambridge-cares/TheWorldAvatar.git
[git clone]: https://git-scm.com/docs/git-clone
[SSH tunnel]: https://code.visualstudio.com/docs/remote/tunnels
[Feature Info Agent]: https://github.com/cambridge-cares/TheWorldAvatar/tree/main/Agents/FeatureInfoAgent

<!-- Stack references -->
[common stack scripts]: https://github.com/TheWorldAvatar/stack/tree/main/common-scripts
[Stack data uploader]: https://github.com/TheWorldAvatar/stack/tree/main/stack-data-uploader
[Stack manager]: https://github.com/TheWorldAvatar/stack/blob/main/stack-manager/README.md
[fia_queries]: ./Stack-manager/inputs/data/fia-queries


