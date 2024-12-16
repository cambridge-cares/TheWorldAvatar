# Hamburg

This is a collection of stack input configuration files to spin up a basic visualisation of some data of the city of Hamburg.

## Start the stack
Make sure to populate ./stack-manager-inputs/inputs/secrets/ with these files
- geoserver_password
- mapbox_api_key
- mapbox_username
- postgis_password

Navigate to ./stack-manager-inputs/
```
./stack.sh start <STACK_NAME>
```

## Data uploader
Contents for the stack data uploader, excluding TBox for OntoDispersion: 
https://www.dropbox.com/scl/fi/2ctt7bmax2eqsldwez2hw/hh-data.tar.gz?rlkey=3niplbiyrxpc9itp5h5bedrno&dl=1

Place contents in ./stack-data-uploader-inputs/data

Run [copy_tbox.sh] to copy tbox files into the right places for data uploader.

Then start the data uploader from ./stack-data-uploader-inputs/:
```
./stack.sh start <STACK_NAME>
```
## Dispersion
### Requirements
Two API keys required for weather and ship data.
1) Obtain an API key from OpenWeather, the API key needs to have OneCall enabled (credit card required, you can set the call limit below the limit before it starts charging). Set the value of API_KEY in [weather-agent.json] to the API key obtained from OpenWeather.
2) Obtain an API key from aisstream.io. Set the value of API_KEY in [ship-input-agent.json] to the obtained API key.

### Data generation
Start ship live updates using [live-ship-updates.http].
After one timestep's worth of data is added by the ship input agent (make sure there are no ongoing logs in the form of `Instantiated derivation <https://www.theworldavatar.com/kg/ontodispersion/Derivation_bfeb8f72-701e-44c9-91a5-f4c8352eddfb> with derivation type <https://www.theworldavatar.com/kg/ontoderivation/Derivation>`), start scheduled Hamburg updates using [hamburg-live.http], the default parameters will run one simulation every 30 minutes.

### Visualisation
Visualisation will be available at http://localhost:3838/visualisation locally.

## Useful links

* Portal to obtain data: [MetaVer - Metadatenverbund](https://metaver.de/startseite)
* [Verwaltungsgrenzen (API)](https://api.hamburg.de/datasets/v1/verwaltungsgrenzen)
* [Verwaltungsgrenzen (MetaVer)](https://metaver.de/trefferanzeige?docuuid=F35EAC11-C236-429F-B1BF-751C0C18E8B7)
* [Behindertenstellplätze (API)](https://api.hamburg.de/datasets/v1/behindertenstellplaetze)
* [Behindertenstellplätze (MetaVer)](https://metaver.de/trefferanzeige?docuuid=5B8DA006-3626-4156-BBF3-EEEB3AB51741)

[weather-agent.json]: ./stack-manager-inputs/inputs/config/services/weather-agent.json
[ship-input-agent.json]: ./stack-manager-inputs/inputs/config/services/ship-input-agent.json
[live-ship-updates.http]: ./http_requests/live-ship-updates.http
[hamburg-live.http]: ./http_requests/hamburg-live.http
[copy_tbox.sh]: ./copy_tbox.sh
