# The Pirmasens Digital Twin (PSDT)

In order to deploy (on a Linux machine):

1. Spin up the `gateway` by following the instructions in its readme file.

2. The PSDT is based on 'the stack' from the `Deploy/stacks/dynamic` folder within TWA git repository. Apart from the core stack, custom containers can be added, in particular the following agents:
   - Access Agent
   - District Heating Agent
   - Feature Info Agent (FIA)
   - Forecasting Agent
   - Historical House 45 Utilities Agent
   - Historical Pirmasens Station Agent
   - Historical Pump Data Instantiation Agent
   - Ifc2Tileset Agent
   - Ifc2OntoBIM Agent
   - IfcOwlConverter Agent
   - Sewage Network Agent
   - Solarkataster Agent
   - Thingspeak Agent

   Please consult the readme files of each of these agents on how to build and deploy them as part of a stack. Essentially, in each case, the agent image needs to be built (or pulled from an image repository), and an input config `json` file needs to be copied into the `stack-manager/inputs/config/services` directory in TWA git repository. In addition, a stack config `json` file needs to be placed in `stack-manager/inputs/config` in TWA in which the custom containers (more precisely the names of the files in `stack-manager/inputs/config/services`) need to be listed. `stack-manager-inputs/config/psdt.json` is an example/template of such a config file. Please note that, as far as the stack is concerned, some services (e.g. the FIA) are built-in but optional. Such services need to be explicitly listed in the stack config file in `stack-manager/inputs/config` but must not (!) have corresponding files in `stack-manager/inputs/config/services`.

   The FIA is critical to the operation of the PSDT, as it allows the front-end to query additional information from the KG. Before spinning it up, make sure you copy all files from the `stack-manager-inputs/data/fia-queries` folder into the relevant folder (e.g. `stack-manager/inputs/data/fia-queries`) in TWA as specified in the stack config `json` file for the `fia-queries` volume. NB Any change to the `json` config file within that volume requires the agent to be restarted, whereas the SPARQL query files are hot-reloaded for each request and thus do not require the agent to be restarted when changed.

   Please note that the Access Agent is not required by the PSDT itself, but by some of the other agents (e.g. district heating and sewage instantiation).

3. From a terminal in the `stack-manager` directory, start the `stack-manager` by running the following:
    ```console
    sudo ./stack.sh start psdt
    ```

4. Many of the instantiation agents listed above need to be triggered by an HTTP request. Examples are provided in the `instantiation-agent-requests.sh` script.

5. Populate the `input` folder of the `stack-data-uploader` directory in TWA git repository with what is in the `stack-data-uploader-inputs` folder, following the readme files in each subfolder.

6. From a terminal in the `stack-data-uploader` directory, start the `stack-data-uploader` container by running the following:
    ```console
    sudo ./stack.sh start psdt
    ```

7. Spin up the visualisation by following the instructions in its readme file.

8. In order to view the visualisation webpage in a browser from outside the host machine, you will need to either open a port in the firewall of the host machine, or set up an ssh tunnel. The required port number can be found in the docker-compose file of the visualisation.

9. In order to access the web-frontends of the containers in the stack from outside the host machine, you will need to either open a port in the firewall of the host machine, or set up an ssh tunnel. For the required port number, see the readme file of the `gateway`.
