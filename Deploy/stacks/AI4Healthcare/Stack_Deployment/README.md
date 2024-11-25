# AI for Public Health: Stack Deployment

This project contains a step-by-step guide on how to spin up the Docker stack for the AI for Public Health project and instantiate all relevant data. It links to other projects and helper scripts where appropriate.

Section Overview:
- [1. Prerequisites](#1-prerequisites): Preparations required before spinning up the use case stack
- [2. Spinning up the stack](#2-spinning-up-the-stack): How to spin up the core stack 
- [3. Data instantiation workflow](#3-data-instantiation-workflow): How to upload initial data sets and deploy all required agents.
- [4. Visualisation](#4-visualisation): How to visualise instantiated cross-domain data via TWA tools
- [Potential refinements/next steps](#potential-refinementsnext-steps): Potential refinements for future work

&nbsp;
# 1. Prerequisites

## Required software installation

Ensure the following software is installed:

- [Git](https://git-scm.com/downloads)
- [Docker Desktop](https://docs.docker.com/get-docker/) or Docker Engine and Docker Compose Plugin

## Access to Docker registries

Spinning up the Docker stack requires access to the [Container registry on Github] to pull (agent) images. Access needs to be ensured beforehand via your Github [personal access token], which must have a `scope` that [allows you to publish and install packages].

To log in to the container registry, please run the following command to establish the connections and provide your access token when prompted. 

```bash
# Github Container registry
$ docker login ghcr.io -u <github_username>
$ <github_personal_access_token>
```
&nbsp;
# 2. Spinning up the stack

This section explains how to spin up the core stack. If using VSCode, ensure that all required VSCode extensions (e.g., Remote - SSH, Docker, and REST Client) are installed to enable all convenience scripts to function correctly.

## Spinning up the stack locally
Before spinning up the stack using the [Stack manager], please provide the following files to the specified folder:

-  Four secret files in `./stack-manager/inputs/secrets`:
    - `postgis_password`
    - `geoserver_password`
    - `mapbox_username`
    - `mapbox_api_key`

Subsequently, copy all files in `./Stack-manager/inputs` into their corresponding repositories of the stack-manager tool under `Deploy/stacks/dynamic/stack-manager/`.

Then navigate to `Deploy/stacks/dynamic/stack-manager` and run the following command there from a *bash* terminal. There are several [common stack scripts] provided to manage the stack:

```bash
# Start the stack (please note that this might take some time)
bash ./stack.sh start Healthcare
# Stop the stack
bash ./stack.sh stop Healthcare
# Remove the stack (incl. volumes)
bash ./stack.sh remove Healthcare -v
# Remove individual service
bash ./stack.sh remove Healthcare <service name>
```
After spinning up the stack, the GUI endpoints to the running containers can be accessed via Browser (i.e. adminer, blazegraph, ontop, geoserver). The exact endpoints and login details can be found in the [Stack Manager README](Deploy/stacks/dynamic/stack-manager/README.md).

## Spinning up the stack remotely via SSH

To deploy the stack remotely via SSH, use VSCode's built-in SSH support. Follow the steps in [VSCode via SSH] to connect to a remote machine (e.g., a virtual machine on Digital Ocean) and start deployment. Regular log in relies on username and password, You can also consider generating an [SSH key] and uploading it via [Upload SSH key] to enable automatic authentication and eliminate repeated credential prompts. After logging in, [git clone] a remote copy of [The World Avatar] repository and follow the provided instructions to deploy the stack.

To access deployed containers through exposed endpoints (e.g., `http://<host IP>:3840/ontop/ui`), ensure the necessary ports are open on the remote machine. Please request port changes through your server administrator—avoid directly modifying firewall rules on individual droplets using tools like `ufw` or `iptables`.

When interacting with the GeoServer GUI remotely, some issues may arise (e.g., inability to remove layers or edit CRS information). To address this, consider [forwarding the port] used by the stack to your local machine after establishing an [SSH tunnel]. This will make GeoServer accessible at `http://localhost:{port}/geoserver/` instead of a remote address like `http://<host IP>:{port}/geoserver/`. 

# 3. Data instantiation workflow



## 1) Food Hygiene Ratings (.xml)

## 2) Greenspace (.shp)
## 3) Points of Interest (.csv)
## 4) GPS Trajectories (.csv)

# 4. Visualisation


# Potential refinements/next steps

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

<!-- Agents -->
[AccessAgent]: https://github.com/cambridge-cares/TheWorldAvatar/tree/main/Agents/AccessAgent
[Property Value Estimation Agent]: https://github.com/cambridge-cares/TheWorldAvatar/tree/main/Agents/PropertyValueEstimationAgent/README.md
[Property Sales Instantiation Agent resources folder]: https://github.com/cambridge-cares/TheWorldAvatar/tree/main/Agents/HMLandRegistryAgent/resources
[MetOffice Agent]: https://github.com/cambridge-cares/TheWorldAvatar/tree/main/Agents/MetOfficeAgent
[AirQuality Agent]: https://github.com/cambridge-cares/TheWorldAvatar/tree/main/Agents/AirQualityAgent

<!-- Repositories -->
[data.json]: /StackDeployment/inputs/stack-manager/inputs/data/visualisation/data.json
[Agent docker-compose file folder]: /StackDeployment/inputs/docker_compose_files

<!-- Files -->
[routing.json]: /StackDeployment/inputs/access_agent/routing.json
[CKG config.properties]: https://github.com/cambridge-cares/CitiesKG/blob/develop/agents/src/main/resources/config.properties