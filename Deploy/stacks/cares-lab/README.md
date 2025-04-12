# CARES Digital Lab
This repository contains the instructions, directory structure, and configurations required to deploy the Lab stack. 

## 1. Preparations
### Knowledge of the stack tools adopted in The World Avatar
Please read through the [Stack Manager](https://github.com/TheWorldAvatar/stack/tree/main/stack-manager) to set up your stack accordingly.

## 2. Deployment Workflow
### Stack manager
Recommended stack name: labStack

1) Add these secret files in [stack-manager-secrets]
    - geoserver_password
    - grafana_password
    - keycloak.conf (Download the file from https://www.dropbox.com/scl/fi/wxhjxxwrwkuk9nr3kf6lv/keycloak.conf?rlkey=tdovr3higr2k25houanb7wbkk&st=pfvw5i0p&dl=0)
	- postgis_password
	- mapbox_username
	- mapbox_api_key

### HTTPS and domain
A domain is required for the lab stack. The current domain being utilized is https://careslab.theworldavatar.io. To set up HTTPS for this domain, refer to the guide listed here: https://www.dropbox.com/scl/fi/tvrj0n0qkvfmuici07fzq/let-s-encrpt-with-certbot-guide.txt?rlkey=kmpkb478izk6qrb6admam4sfi&st=jf96zqyv&dl=0

### Keycloak
1. Navigate to [keycloak.json] and modify the following environment variables if necessary:
- KEYCLOAK_ADMIN 
- KEYCLOAK_ADMIN_PASSWORD
- JAVA_OPTS_APPEND - the trust store password can be modified if necessary

2. Navigate to the following dropbox folder and download the truststore.jks file:
https://www.dropbox.com/scl/fi/vnnrpz3gte71jpzew6kvu/truststore.jks?rlkey=664b2nykhx2af1qyuhknspwrl&st=q6ms2mco&dl=0

3. In the [keycloak.json] and under the `Mounts` section, modify `Source` based on the location of the truststore.jks file.

### Visualisation
1. Go to the following [dropbox folder](https://www.dropbox.com/scl/fi/hztcpc0fxz2ql8jimsm3u/data.zip?rlkey=ejdv5e85l6gmfq7ka1zadxpae&st=7mua6kbp&dl=0) and download the `data` folder. It should contain the glb files for the lab visualisation.

2. Unzip the folder and extract the `data` folder to [webspace].

3. Navigate to [services], open [visualisation.json] and under the `Mounts` section, modify `Source` based on where [webspace] is located at.

### Stack Agents
To set up and build each of the agents, follow the instructions in their respective READMEs:
1. [Android Status Agent](https://github.com/cambridge-cares/TheWorldAvatar/tree/main/Agents/AndroidStatusAgent)
2. [Asset Manager Agent](https://github.com/cambridge-cares/TheWorldAvatar/tree/main/Agents/AssetManagerAgent)
- After setting up, navigate to [services], open [asset-manager-agent.json] and under the `Mounts` section, modify `Source` accordingly.
3. [BMS Query Agent](https://github.com/cambridge-cares/TheWorldAvatar/tree/main/Agents/BMSQueryAgent)
4. [BMS Update Agent](https://github.com/cambridge-cares/TheWorldAvatar/tree/main/Agents/BMSUpdateAgent)
- After setting up, navigate to [services], open [bms-update-agent.json] and under the `Mounts` section, modify `Source` accordingly.
5. [CARES Weather Station Agent](https://github.com/cambridge-cares/TheWorldAvatar/tree/main/Agents/CARESWeatherStationAgent)
- After setting up, navigate to [services], open [cares-weather-station-agent.json] and under the `Mounts` section, modify `Source` accordingly.
6. [Dashboard Agent](https://github.com/cambridge-cares/TheWorldAvatar/tree/main/Agents/DashboardAgent)
7. [Data Bridge Agent](https://github.com/cambridge-cares/TheWorldAvatar/tree/main/Agents/DataBridgeAgent)
- After setting up, navigate to [services], open [data-bridge-agent.json] and under the `Mounts` section, modify `Source` accordingly.
8. [Email Agent](https://github.com/cambridge-cares/TheWorldAvatar/tree/main/Agents/EmailAgent)
- After setting up, navigate to [services], open [email-agent.json] and under the `Mounts` section, modify `Source` accordingly.
9. [Feature Info Agent](https://github.com/cambridge-cares/TheWorldAvatar/tree/main/Agents/FeatureInfoAgent)
- The queries and config files can be found in [fia_configs]. Navigate to [services], open [feature-info-agent.json] and under the `Mounts` section, modify `Source` based on where the queries and config files are located at. 
10. [FH Sash And Occupancy Agent](https://github.com/cambridge-cares/TheWorldAvatar/tree/main/Agents/FHSashAndOccupancyAgent)
- After setting up, navigate to [services], open [fh-sash-and-occupancy-agent.json] and under the `Mounts` section, modify `Source` accordingly.
11. [PIPS TimeSeries Agent](https://github.com/cambridge-cares/TheWorldAvatar/tree/main/Agents/PIPSTimeSeriesAgent)
- After setting up, navigate to [services], open [pips-timeseries-agent.json] and under the `Mounts` section, modify `Source` accordingly.
- In [pips-timeseries-agent.json], fill in the values for `CLIENT_ID` and `KEYCLOAK_REALM_PATH` under the `Env` section.
12. [RFID Query Agent](https://github.com/cambridge-cares/TheWorldAvatar/tree/main/Agents/RFIDQueryAgent)
- After setting up, navigate to [services], open [rfid-query-agent.json] and under the `Mounts` section, modify `Source` accordingly.
13. [RFID Update Agent](https://github.com/cambridge-cares/TheWorldAvatar/tree/main/Agents/RFIDUpdateAgent)
- After setting up, navigate to [services], open [rfid-update-agent.json] and under the `Mounts` section, modify `Source` accordingly.

The exact configurations and properties files for each agent that was deployed on Astoria can be found here: 
https://www.dropbox.com/scl/fi/tdw3ai1zoikb03b5la3w5/labStack_agent_configs.zip?rlkey=hb8q6z3e5hgeuv3xmbtc4ry97&st=ymohvpni&dl=0

### External Agents
Due to design/network constraints and stack incompatibility, some of the agents are deployed out of the stack. Follow the instructions in their respective READMEs to set them up:
1. [API Agent](https://github.com/cambridge-cares/TheWorldAvatar/tree/main/Agents/APIAgent)
2. [FH Agent](https://github.com/cambridge-cares/TheWorldAvatar/tree/main/Agents/FHAgent)
3. [Forecasting Agent](https://github.com/cambridge-cares/TheWorldAvatar/tree/main/Agents/ForecastingAgent)
4. [Mackay Data Agent](https://github.com/cambridge-cares/TheWorldAvatar/tree/main/Agents/MackayDataAgent)
5. [RFID Agent](https://github.com/cambridge-cares/TheWorldAvatar/tree/main/Agents/RFIDAgent)

The exact configurations and properties file for each agent that was deployed on Astoria can be found here:
https://www.dropbox.com/scl/fo/zne8c9e4bf3ikfpeymzoj/AM1dp5_i6b9rEthmzHSZQPY?rlkey=w39frdpv7yzgzmezm52zyu717&st=2z7dwtgz&dl=0


[stack-manager-secrets]: ./stack-manager/inputs/secrets/
[webspace]: ./stack-manager/inputs/data/webspace/
[services]: ./stack-manager/inputs/config/services/
[stack-manager]: ./stack-manager/
[asset-manager-agent.json]: ./stack-manager/inputs/config/services/asset-manager-agent.json
[bms-update-agent.json]: ./stack-manager/inputs/config/services/bms-update-agent.json
[cares-weather-station-agent.json]: ./stack-manager/inputs/config/services/cares-weather-station-agent.json
[data-bridge-agent.json]: ./stack-manager/inputs/config/services/data-bridge-agent.json
[email-agent.json]: ./stack-manager/inputs/config/services/email-agent.json
[feature-info-agent.json]: ./stack-manager/inputs/config/services/feature-info-agent.json
[fh-sash-and-occupancy-agent.json]: ./stack-manager/inputs/config/services/fh-sash-and-occupancy-agent.json
[pips-timeseries-agent.json]: ./stack-manager/inputs/config/services/pips-timeseries-agent.json
[rfid-query-agent.json]: ./stack-manager/inputs/config/services/rfid-query-agent.json
[rfid-update-agent.json]: ./stack-manager/inputs/config/services/rfid-update-agent.json
[visualisation.json]: ./stack-manager/inputs/config/services/visualisation.json
[keycloak.json]: ./stack-manager/inputs/config/services/keycloak.json
[fia_configs]: ./fia_configs/