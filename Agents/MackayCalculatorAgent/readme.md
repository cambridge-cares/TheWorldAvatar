##MacKay Calculator Agent
This agent is designed to provide access to the controls and outputs (I/O) of the Singapore MacKay Carbon Calculator model which is originally in Excel format. The agent interacts with the worldavatar knowledge graph with APIs to automatically update the calculator with values from the KG in real time.

A webapp is provided as the GUI for users to interact with the Singapore MacKay Calculator.



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
4. Build webapp frontend:
```commandline
cd frontend
npm run build
```

6. Start backend.
```
flask run --host=X.X.X.X --port=XXXX
```