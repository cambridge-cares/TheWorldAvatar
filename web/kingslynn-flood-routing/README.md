# Kings Lynn Flood Routing Digital Twin Visualisation Framework (TWA-VF)

This visualisation serves as a proof of concept, leveraging knowledge graph technology for:

1) Flood routing for vehicles with various water depth wading capabilities.
2) Isochrone mapping from points of interest, highlighting unreachable area and affected population.
3) Travelling Salesman Problem for fastest route to restore flooded power stations.
4) Road network sensitivity analysis before and after flood, so the roads most needed for restoration can be identified.

This visualisation uses result of

1) [IsochroneAgent](https://github.com/cambridge-cares/TheWorldAvatar/tree/main/Agents/IsochroneAgent)
2) [TravellingSalesmanAgent](https://github.com/cambridge-cares/TheWorldAvatar/tree/main/Agents/TravellingSalesmanAgent)
3) [NetworkAnalysisAgent](https://github.com/cambridge-cares/TheWorldAvatar/tree/main/Agents/NetworkAnalysisAgent)

The instantiated data is visualised using the TWA Visualisation Framework [TWA-VF](../twa-vis-framework) version `4`.

![Screenshot](TSP.png "Flood Routing Tool")

## Deployment

1. Create and/or copy files according to instructions stated in `stack-manager-inputs` and `stack-data-uploader-inputs`.
2. Define environment variables in `.env` file:
   - `PORT_NUMBER`: the port number that the stack is exported to, needed for local communication with agents.
   - `PUBLIC_URL`: the URL users use to access the visualisation. No / at the end.
   - `STACK_NAME`: the name of the stack.

3. Execute `deploy.sh` in commandline, which calls the following three scripts:

- `update_input_config.sh`: updates configuration files of the stack and agents. Be aware that this will replace the current configuration files.
- `restart_stack.sh`: restarts the stack and upload data. Be aware that this will remove any existing stack with the same name.
- `call_agent.sh`: trigger agents to act on uploaded data.

## Accessing the visualisation

Visualisation can be seen at [http://localhost:3838/visualisation](http://localhost:3838/visualisation) when deployed locally.
