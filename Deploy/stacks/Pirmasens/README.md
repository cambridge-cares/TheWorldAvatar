# District Heating Optimisation (stack deployment)

This README provides instructions to spin up a Docker stack called `dhstack` for the district heating (DH) optimisation use case implemented using [chained derivations] to:
1) Forecast anticipated heat demand (and grid temperatures) using the [Forecasting Agent]
2) Optimise the heat generation mix for that demand with regards to total generation cost using the [DH Optimisation Agent]
3) Estimate associated emissions using the [DH Emission Estimation Agent] and
4) Simulate their dispersion using the [Aermod Agent]

The initiation/update of this chain of derivations is managed by the [DH Optimisation Trigger Agent].

The workflow outlined below:
- requires the target stack name to be `dhstack`
- has only been tested with a clean `namespace` so far; hence, the instantiation into a `namespace` with pre-existing triples has not been tested and the provided derivation markup-methods might fail

### 1) Stack manager configuration

Before spinning up the stack using the [Stack manager], please provide the following files to the specified repositories:

1) Create 5 secret files in `./stack-manager/inputs/secrets`
    - geoserver_password
    - postgis_password
    - grafana_password
    - mapbox_username
    - mapbox_api_key

    NOTE: `geoserver_password` and `postgis_password` are personal choices, valid credential for MapBox is required to view the visualisation.

2) Adjust a few properties in the provided stack manager input configurations found in `./stack-manager/inputs/config/services/`:
    1) Insert a valid API key for openweather in `weather-agent.json`
    2) Replace `<REPLACE_WITH_YOUR_PIRMASENS_DIRECTORY>` in the bind mount path in `dh-optimisation-trigger-agent.json` with the absolute path to this repository
    3) Potentially replace `localhost` in `STACK_URL` in `dispersion-interactor.json` with target host, e.g., Digital Ocean IP address. This is used to construct the WMS endpoint in the visualisation data.json, also used by the visualisation to contact the feature info agent.

3) Edit `./stack-manager/inputs/data/visualisation/index.html`, replace `localhost` in the following lines to the host name where the stack is deployed.
    ```javascript
	var dispersionHandler = new DispersionHandler("http://localhost:3838", manager);
    
	let custom_component = new SeachEntityComponent("Search Land Plot", "http://localhost:3838", MapHandler.MAP, "0.0.plots");
	```

### 2) Build district-heating-instantiation Docker image locally

This step requires access to a private repository as it leverages confidential operations data. In case access is needed, please contact the authors. Similarly, the URLs to [pre-trained forecasting model] files have been replaced with placeholders and would also need to be adjusted.

1) Populate the [timeseries.properties] file with the following values:
    ```properties
    db.user = postgres
    db.password = <postgis_password>
    db.url=jdbc:postgresql://dhstack-postgis:5432/postgres
    sparql.query.endpoint=http://dhstack-blazegraph:8080/blazegraph/namespace/kb/sparql
    sparql.update.endpoint=http://dhstack-blazegraph:8080/blazegraph/namespace/kb/sparql
    ```
    Please note:
    - The `db.password` needs to match the value provided in the `postgis_password` input file to the stack-manager
    - Blazegraph namespace shall be default `kb` (as required by Aermod later)
    <br/><br/>

2) Specify the 2020 time series data to instantiate by setting the corresponding `DYNAMIC_VALUES_FILE` URL in [dataproperties.py]. As the actual data is confidential, a [time series data example] is provided to outline the nature of the required data.

3) Execute the following command within the `districtheating` sub-repository:
    ```bash
    docker build -t district-heating-instantiation:2020 -f ./Dockerfile ..
    ```

### 3) Start the stack

Execute the following command in the current directory:
```bash
bash ./startup.sh
```
Please note that the initial instantiation of the district heating network data might take some time (~15-20min) and the `dh-instantiation` container will stop once finished.

**Please note**: Agents are added to the stack in the order as stated in the `dhstack.json` file. To avoid derivation markup issues, the `dh-optimisation-trigger-agent` service only starts after the instantiation has finished; naming of Docker services matters, as `http://dhstack-dh-instantiation` is curled to assess whether the instantiation container has yet finished.

This step is finished once the `dh-instantiation` container stops.

### 4) Upload building, elevation, and TBox data

1) Do not start this process while `dh-instantiation` is still ongoing as this will change the default schema and cause data to be instantiated in the wrong schema.

2) Follow instructions in `stack-data-uploader/data/readme.txt` to download and extract data to upload and place extracted contents into `stack-data-uploader/data` repository

3) Execute the following command within the stack-data-uploader repository:
    ```bash
    bash ./stack.sh start dhstack
    ```

This step is finished once the `stack-data-uploader` container stops.

### 5) Trigger an optimisation

To trigger an optimisation run, please send a POST request to the [DH Optimisation Trigger Agent], similar to the [example_opt_request.http]. Please note that the `<host>` needs to be adjusted beforehand.

### 6) TWA Visualisation

At least one timestep of AERMOD simulation needs to be present for the visualisation to work, and it can be viewed at http://localhost:3838/visualisation, replace `localhost` if deployed elsewhere.

### 7) Grafana Dashboard

Two dashboards are provided as .json models in the [grafana-prep] repository, i.e., one `demand.json` and one `generation.json`. To update the provided model files to the current instantiation, one has to 1) update the grafana internal datasource uid and 2) update placeholder time series table names. To do so, please follow the provided [grafana-prep readme].

The dashboards can be viewed at http://localhost:3838/analytics, replace `localhost` if deployed elsewhere.


### 8) Backup ontop mapping for re-deployment

Before re-deploying the stack, i.e., spinning the entire stack down and restarting it, please make sure to download the OBDA mapping file from the `ontop` container, the path of the file is `/ontop.obda` within the container. This avoids rerunning everything in case the stack requires restarting.

The backed up `ontop.obda` file can be placed into the `./ontop-backup/` folder.

To spin down a stack without deleting data, 
```bash
bash ./stack.sh remove dhstack
```

Running the following script will start up the stack and copy the mapping file into the container,
```bash
bash ./startup-with-backup-obda.sh
```


&nbsp;
# Authors #
Markus Hofmeister (mh807@cam.ac.uk), November 2023

Kok Foong Lee (kokfoong.lee@cares.cam.ac.uk), November 2023


<!-- Links -->
[time series data example]: ./dh-instantiation/timeseries_data_example.csv
[pre-trained forecasting model]: ./dh-optimisation-trigger-agent-inputs/triples/fc_models.ttl
[example_opt_request.http]: ./http-request/example_opt_request.http
[grafana-prep]: ./stack-manager/inputs/data/grafana-prep
[grafana-prep readme]: ./stack-manager/inputs/data/grafana-prep/readme.txt

[chained derivations]: https://lucid.app/publicSegments/view/8dfdf102-bb7d-47de-bb52-c22d86a50bcf/image.jpeg
[timeseries.properties]: https://github.com/cambridge-cares/pirmasens/blob/main/districtheating/resources/timeseries.properties
[dataproperties.py]: https://github.com/cambridge-cares/pirmasens/blob/main/districtheating/resources/dataproperties.py

<!-- Agent -->
[Forecasting Agent]: https://github.com/cambridge-cares/TheWorldAvatar/tree/main/Agents/ForecastingAgent
[DH Optimisation Agent]: https://github.com/cambridge-cares/TheWorldAvatar/tree/main/Agents/DistrictHeatingOptimisationAgent
[DH Emission Estimation Agent]: https://github.com/cambridge-cares/TheWorldAvatar/tree/main/Agents/DistrictHeatingEmissionEstimationAgent
[Aermod Agent]: https://github.com/cambridge-cares/TheWorldAvatar/tree/main/JPS_VIRTUALSENSOR/AermodAgent
[DH Optimisation Trigger Agent]: https://github.com/cambridge-cares/TheWorldAvatar/tree/main/Agents/DistrictHeatingOptimisationTriggerAgent
[Stack manager]: https://github.com/cambridge-cares/TheWorldAvatar/blob/main/Deploy/stacks/dynamic/stack-manager/README.md
