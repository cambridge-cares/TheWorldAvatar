##MacKay Calculator Agent
This agent is designed to provide access to the controls and outputs (I/O) of the Singapore MacKay Carbon Calculator model which is originally in Excel format. A React web interface is additionally provided to interact with the Singapore MacKay Calculator and view the model outputs. The agent is designed to be used with [MacKay Data Agent] to provide an update functionality. If user requests an update, a list of mapped entries in the model will be automatically updated with corresponding Timeseries data ([OntoTimeSeries]) in the TWA KG.  [MacKay Data Agent] is required for the management of the automatic initiation and updates of these Timeseries. Otherwise, the Calculator Model will not be updated with up-to-date data.

MacKay Calculator Model requires many timeseries data and (their forecasts) as inputs to run. These data will change over time. Consequently, updating the Calculator Model after a period of time previously requires laborious manual modifications. With our agents operating on the dynamic TWA KG, this update is done automatically. 




##API

###Endpoints
####Set Calculator levers and retrieve data
URL:  /data 

Method: POST

Data constraints: Provide lever values:

`````{"levers":[ an array of 45 intergers in range 1-4]}`````



Success Responses: Content: 

```{"status": "Success", "values": [...]}```


#### Update the Calculator with up-to-date input data from World Avatar KG
URL: /update

Method: Get

Success Responses: Code: 200 OK


##  Usage
###Overview
Currently the agent relies on an Excel model and therefore requires a Windows OS and a copy of the Excel software to run. Until we finish converting the model into another form of code in the future this agent will not have a dockerized version.

###Requirements
python >=3.10, java jdk>=11, nodeJS>=18


###Data Agent
Note that for the update-from-TWA function to work, another agent - [MacKay Data Agent] needs to be deployed. Refer to its document for a set-up guide.


###Setup


1. Install required python packages.
```shell
pip install -r requirements.txt
```
2. Create a folder called `model` in the same directory as this README if it does not exist yet. Download MacKay Calculator Excel model (```Excel_MacKay_Carbon_Calculator_v209.xlsm```) and mapping csvs (```controls.csv```, ```output.csv```,```single_values.csv```) into ```model``` from [CARES Mackay Calculator model and mapping Dropbox](https://www.dropbox.com/scl/fo/sktgvt6mxuxbffjo5lyfy/AHYhKQPPHdVIY2ChAGaWDIo?rlkey=di2o9os1rp0y6lghp6lthtbrv&dl=0).
3. Create a folder called `json` at `/frontend/src/assets/`. Run ```prepare_initial.py```, which generates a `initial_plotdata.json` and a `descriptions.json` in `/frontend/src/assets/json`.
4. From [CARES Mackay Calculator fonts Dropbox](https://www.dropbox.com/scl/fo/imkyr5x0pjw387s1nkjoq/AKfiQ-rGBFEfMfX3wTjya3E?rlkey=c9sycvqaujswomhycr0rdtbri&st=fow8ivth&dl=0), Download the zip folder (```fonts-ttf.zip```). The zip folder contains a folder named as `fonts-ttf`, extract that folder to `/frontend/src/assets/`. All of the fonts ttf files should now be located at `/frontend/src/assets/fonts-ttf`.
5. To build and use the React web interface, install [Node.js](https://nodejs.org/en/download/) and npm package manager following their official guide.
6. Open a command terminal at `/frontend` and run the following commands:
```commandline
npm install
npm run build
```

7. Start backend.
```
flask run --host=X.X.X.X --port=XXXX
```
[MacKay Data Agent]: https://github.com/cambridge-cares/TheWorldAvatar/tree/main/Agents/MackayDataAgent
[Derived Information Framework]: https://github.com/cambridge-cares/TheWorldAvatar/tree/main/JPS_BASE_LIB/src/main/java/uk/ac/cam/cares/jps/base/derivation
[OntoTimeSeries]: https://github.com/cambridge-cares/TheWorldAvatar/tree/main/JPS_Ontology/ontology/ontotimeseries