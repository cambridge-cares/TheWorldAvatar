# Digital Twin Visualisation Framework (DTVF)

The instantiated data is visualised using the Digital Twin Visualisation Framework ([DTVF]). The file structure is based on the [example Mapbox visualisation].

&nbsp;
## Creating the Visualisation

Detailed instructions on how to create (and customise) the visualisation can be found in the [example Mapbox visualisation] and [DTVF] READMEs. To deploy the visualisation including all data as specified in the `data.json` file as Docker container, please run the following commands from the [DTVF] directory:

```bash
# To build the Image:
docker-compose -f ./docker/docker-compose.yml build --force-rm

# To generate a Container (i.e. run the Image):
docker-compose -f ./docker/docker-compose.yml up -d --force-recreate
```

**Please note**: A valid Mapbox API username and token must be provided in your `index.html` file.


&nbsp;
## Feature Info Agent

> The following description refers to commit `b02bb38598d19d96eecbb3f42fa3c816b5b59674` of the `dev-feature-info-agent` branch

The Feature Info Agent is used to retrieve meta data for visualisation. Details on how to spin up and deploy the agent to the spun up Stack is provided in the [FeatureInfoAgent] README. **Please note** that building the agent requires access to further [TWA Github packages], which requires both a `settings.xml` and `settings-security.xml` to be provided in the `.m2` folder of the [FeatureInfoAgent] before building.

The required `.sparql` files to be placed inside the agent before building the Docker image are provided in the [FeatureInfoAgent_queries] sub-folder of this repository (as well as to be found in the `resource` folders of the respective input agents (i.e. MetOfficeAgent, EPCInstantiationAgent)).

```bash
# Build the agent image
 bash ./stack.sh build
 # Deploy the agent
 bash ./stack.sh start <STACK_NAME>
```

&nbsp;
## Important pre-requisites

The ensure expected behaviour of the Feature Info Agent, the following pre-requisites must be met:

* **Allow CORS (i.e. Cross-Origin Resource Sharing)**: The Feature Info Agent relies on CORS information to retrieve metadata from the stack after selecting any displayed feature. A potential work-around  is installing a browser plug-in to blanket-allow CORS requests. One should be aware of the security implications of this!

* **Add NGINX routing for Feature Info Agent**: When spinning up a stack using the stack-manager, the stack adds all necessary routes to the nginx container automatically. However, this does not apply to the retrospectively added Feature Info Agent. To add the routing, please run:


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
[FeatureInfoAgent_queries]: FeatureInfoAgent_queries
[DTVF]: DTVF