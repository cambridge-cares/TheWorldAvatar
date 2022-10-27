# Digital Twin Visualisation Framework (DTVF)

The instantiated data is visualised using the Digital Twin Visualisation Framework ([DTVF]). The file structure is based on the [example Mapbox visualisation].

&nbsp;
## Creating the Visualisation

Detailed instructions on how to create (and customise) the visualisation can be found in the [example Mapbox visualisation] and [DTVF] READMEs. To deploy the visualisation as Docker container, please run the following commands from the [DTVF] directory:

```bash
#To build the Image:
docker-compose -f ./docker/docker-compose.yml build --force-rm

#To generate a Container (i.e. run the Image):
docker-compose -f ./docker/docker-compose.yml up -d --force-recreate
```

**Please note**: A valid Mapbox API username and token must be provided in your `index.html` file.


&nbsp;
## Feature Info Agent

The Feature Info Agent is used to retrieve meta data for visualisation. Details on how to spin up and deploy the agent to the spun up Stack is provided in the [FeatureInfoAgent] README. The required `.sparql` files to be placed inside the agent before building the Docker image are provided in the [FeatureInfoAgent_queries] sub-folder of this repository.


<!-- Links -->
[DTVF]: https://github.com/cambridge-cares/TheWorldAvatar/wiki/Digital-Twin-Visualisations
[example Mapbox visualisation]: https://github.com/cambridge-cares/TheWorldAvatar/tree/main/web/digital-twin-vis-framework/example-mapbox-vis
[FeatureInfoAgent]: https://github.com/cambridge-cares/TheWorldAvatar/tree/dev-feature-info-agent/Agents/FeatureInfoAgent

<!-- repositories -->
[FeatureInfoAgent_queries]: FeatureInfoAgent_queries
[DTVF]: DTVF