# Digital Twin Visualisation Framework (DTVF)

The instantiated data is visualised using the Digital Twin Visualisation Framework ([DTVF]) version `3.3.4`. The configuration file structure (i.e. `data.json`) is based on the [example Mapbox visualisation].

&nbsp;
## Creating the Visualisation

Detailed instructions on how to create (and customise) the visualisation can be found in the [example Mapbox visualisation] and [DTVF] READMEs. To deploy the visualisation including all data as specified in the [data.json] file as Docker container, please run the following commands from the [DTVF subdirectory] (i.e. the location where this README is located):

```bash
# To build the Image:
docker compose -p dtvf-kings-lynn -f ./docker/docker-compose.yml build --force-rm
# To generate a Container (i.e. run the Image):
docker compose -p dtvf-kings-lynn -f ./docker/docker-compose.yml up -d --force-recreate

# To rebuild the image and deploy the container:
bash ./redeploy.sh
```

**Please note**: 

1) A valid Mapbox API username and token must be provided in your [index.html] file. After successfully deploying the visualisation, it should be available at `http://localhost:5555`.

2) Ensure that the required legend figures are provided to the [icons] sub-folder of the `data` repository. Otherwise, please run the Python scripts in the `Utilities\dtvf_legends` repository first.


&nbsp;
## Feature Info Agent (FIA)

> The following description refers to `ghcr.io/cambridge-cares/feature-info-agent:2.0.0` as of commit `f01834fec13ac25b205faeed05eb2b7ce83169d2` of the `https://github.com/cambridge-cares/TheWorldAvatar/tree/main`

The Feature Info Agent is used to retrieve meta data for visualisation(s). Details on how to spin up and deploy the agent to a spun up Stack is provided in the [FeatureInfoAgent] README. However, the following steps shall be sufficient to get the agent up and running for the `KINGS-LYNN` visualisation:

1) Copy the `feature_info_agent.json` file from the [FeatureInfoAgent subdirectory] into the `inputs/config/services` folder of the stack manager (i.e. `Deploy/stacks/dynamic/stack-manager/inputs/config/services`) - potentially adjust the absolute path of the source path of the bind mount
2) Copy the required `fia-config.json` and `.sparql` files from the [FeatureInfoAgent queries] sub-folder of this repository into the `queries` folder of the Feature Info Agent (i.e. `Agents/FeatureInfoAgent/queries`)
3) Start the stack manager as usual (i.e. `bash ./stack.sh start <STACK_NAME>` from the stack-manager repo). This should start the FIA container. **Please use a bash terminal to avoid potential issues with inconsistent path separators.**

Deploying the agent creates a bind mount between the `queries` directory on the host machine, and the `/app/queries` directory within the container. As the sparql files in the query folder are loaded upon request, added files become 'hot-reloaded' and should automatically be available to the agent without having to restart the container. Changing the `fia-config.json`, however, requires a container restart.


&nbsp;
## Important Pre-requisites

To ensure communication between the DTVF and the Feature Info Agent, the following pre-requisites must be met:

* **Allow CORS (i.e. Cross-Origin Resource Sharing)**: The FIA relies on CORS information to retrieve metadata from the stack after clicking on any displayed feature. A current work-around to enable this is installing a browser plug-in to blanket-allow CORS requests. However, one should be aware of the security implications of this!


```diff
- Pending To Dos 
    - remove test building layers from data.json (+convert them into equivalent for actual building layer)
```

<!-- Links -->
[DTVF]: https://github.com/cambridge-cares/TheWorldAvatar/wiki/Digital-Twin-Visualisations
[example Mapbox visualisation]: https://github.com/cambridge-cares/TheWorldAvatar/tree/main/web/digital-twin-vis-framework/example-mapbox-vis
[FeatureInfoAgent]: https://github.com/cambridge-cares/TheWorldAvatar/tree/main/Agents/FeatureInfoAgent

<!-- repositories -->
[FeatureInfoAgent subdirectory]: /DTVF/FeatureInfoAgent
[FeatureInfoAgent queries]: FeatureInfoAgent/queries
[DTVF subdirectory]: /DTVF
[icons]: /DTVF/data/icons
[index.html]: index.html
[data.json]: /DTVF/data.json