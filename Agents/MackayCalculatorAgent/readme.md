##MacKay Calculator Agent
This agent is designed to provide access to the controls and outputs (I/O) of the Singapore MacKay Carbon Calculator model which is originally in Excel format. The agent interacts with the worldavatar knowledge graph with APIs to automatically update the calculator with values from the KG in real time.

A React web interface is additionally provided to interact with the Singapore MacKay Calculator.



##API
TODO
##  Usage
###Overview
Currently the agent relies on an Excel model and therefore requires a Windows OS and a copy of the Excel software to run. Until we finish converting the model into another form of code in the future this agent will not have a dockerized version.

###Setup


1. Install required python packages. The project requires Python 3.10.
```shell
pip install -r requirements.txt
```
2. Download Mackay Calculator Excel model and mapping csvs (```controls.csv```, ```output.csv```,```single_values.csv```) into ```model/```.
3. Run ```prepare_initial.py```, which generates a `initial_plotdata.json` and a `descriptions.json` in `/frontend/src/assets/json`.
4. To build and use the React web interface, install [Node.js](https://nodejs.org/en/download/) and npm package manager following their official guide. Then use the following command to install dependencies and build the React frontend app. 
```commandline
cd frontend
npm run install
npm run build
```

5. Start backend.
```
flask run --host=X.X.X.X --port=XXXX
```