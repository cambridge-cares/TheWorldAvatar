# Digital Twin Visualisation Framework (DTVF)

The instantiated data is visualised using the Digital Twin Visualisation Framework ([DTVF]). The file structure is based on the [example Mapbox visualisation].

&nbsp;
## Creating the Visualisation

Detailed instructions on how to create (and customise) the visualisation can be found in the [example Mapbox visualisation] and [DTVF] READMEs. To deploy the visualisation including all data as specified in the `data.json` file as Docker container, please run the following commands from the [DTVF] directory:

```bash
# To build the Image:
docker-compose -p kings-lynn -f ./docker/docker-compose.yml build --force-rm
# To generate a Container (i.e. run the Image):
docker-compose -p kings-lynn -f ./docker/docker-compose.yml up -d --force-recreate

# To rebuild the image and deploy the container:
bash ./redeploy.sh
```

**Please note**: A valid Mapbox API username and token must be provided in your `index.html` file.


&nbsp;
## Feature Info Agent (FIA)

> The following descriptions refers to commit `cfba412cf8abdc029519da01710f4d822229eb57` of the `dev-feature-info-agent` branch

The Feature Info Agent is used to retrieve meta data for visualisation(s). Details on how to spin up and deploy the agent to the spun up Stack is provided in the [FeatureInfoAgent] README. **Please note** that building the agent requires access to further [TWA Github packages], which requires both a `settings.xml` and `settings-security.xml` to be provided in the `.m2` folder of the [FeatureInfoAgent] before building. **Please also note** that building is only required if changes have been made to the agent. Otherwise, the Docker image should simply be pulled (i.e. by skipping this build command)

```bash
# Build the agent image
 bash ./stack.sh build
```
Deploying the agent creates a bind mount between the `queries` directory on the host machine, and the `/app/queries` directory within the container. This means that simply adding your configuration and query files to the former **before** running the container should automatically make them available to the agent.

The required `fia-config.json` and `.sparql` files to be placed inside the agent before building/deploying the Docker image are provided in the [FeatureInfoAgent queries] sub-folder of this repository (as well as to be found in the `resource` folders of the respective input agents (i.e. MetOfficeAgent, EPCInstantiationAgent)).
```diff
- Pending To Do: 
    - Ensure this is true after verifying that the queries work
```

To deploy the agent to the spun up Stack, please run the following command from the [FeatureInfoAgent] directory wherever the stack is running (i.e. potentially on the remote VM). :

```bash
 # Deploy the agent
 bash ./stack.sh start <STACK_NAME>
```
After deploying the agent, the NGINX routing configuration needs to be added. **Please note** that this is required after each agent startup, details are provided below.


&nbsp;
## Important Pre-requisites

The ensure communication between the DTVF and the Feature Info Agent, the following pre-requisites must be met:

* **Allow CORS (i.e. Cross-Origin Resource Sharing)**: The FIA relies on CORS information to retrieve metadata from the stack after clicking on any displayed feature. A current work-around to enable this is installing a browser plug-in to blanket-allow CORS requests. However, one should be aware of the security implications of this!

* **Add NGINX routing for Feature Info Agent**: When spinning up a stack using the stack-manager, the stack adds all necessary routes to the nginx container automatically. However, this does not apply to the retrospectively added FIA. Run the [update_nginx_conf.sh] script to update the stack NGINX configuration to make the FIA reachable from the visualisation and prevent CORS errors. **Please note** that this script needs to be run wherever the stack is running, i.e. on the remote virtual machine if deployed there.

    ```bash
    # Run helper script to add Feature Info Agent NGINX configuration
    bash ./update_nginx_conf.sh
    ```


```diff
- Pending To Dos 
    - update visualised (fudged) building data with actual data after running building matching agent
    - refine clustering with vector tiles
```

<!-- Links -->
[DTVF]: https://github.com/cambridge-cares/TheWorldAvatar/wiki/Digital-Twin-Visualisations
[example Mapbox visualisation]: https://github.com/cambridge-cares/TheWorldAvatar/tree/main/web/digital-twin-vis-framework/example-mapbox-vis
[FeatureInfoAgent]: https://github.com/cambridge-cares/TheWorldAvatar/tree/dev-feature-info-agent/Agents/FeatureInfoAgent
[TWA Github packages]: https://github.com/cambridge-cares/TheWorldAvatar/wiki/Packages

<!-- repositories -->
[FeatureInfoAgent queries]: FeatureInfoAgent/queries
[DTVF]: DTVF
[update_nginx_conf.sh]: /DTVF/FeatureInfoAgent/routing/update_nginx_conf.sh