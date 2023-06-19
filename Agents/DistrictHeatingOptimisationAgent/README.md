# District Heating Optimisation Agent

## About the agent

## Set up Virtual Env
How to create a Python virtual environment (in Windows):
```bash
python -m venv <name>
cd <name>
cd Scripts
activate
python -m pip install --upgrade pip
python -m pip install -r requirements.txt
```

## Folder contents and description

### Files added by Srishti:
1. **/OptimisationAgent/javagateway.py** - Create and start JAVA resource gateway objects to the TWA/JPS base library
2. **/OptimisationAgent/utils.py** - Contains some util functions for sparql queries and to read properties file
3. **/OptimisationAgent/resources/timeseries.properties** - Define sparql endpoints and postgres db url


Files under **/OptimisationAgent/district_heating/generation_optimisation**

4. **/create_queries.py** - Contains sparql queries to obtain
optimisation inputs from the KG
5. **/load_input.py** - Run queries to obtain the timeseries 
input data, arrange it into the required dataframe format, then follow the original code to create a
dictionary describing the full SWPS optimization setup
6. **/update_results_to_kg.py** - Run sparql updates and
update triples and timeseries data (results) into KG
7. **/run_optimisation.py** - main function which sets up data,
runs the optimisation functions and outputs data to the KG and the python console. opt_period is set here.


All remaining files are from the original optimisation code. Files from original code that can be potentially removed:
Files under **/OptimisationAgent/district_heating/generation_optimisation**:
1. /cost_optimization_preprocessing.py - Preprocessing of data
2. /cost_optimization_postprocessing.py - Postprocessing of data
3. /cost_optimization_mpc.py - Main function to run the optimisation (refactored into run_optimisation.py)

## How to run
1. Ensure data has been uploaded to the correct namespace and postgres database as specified in timeseries.properties
file.

2. Create folders at /OptimisationAgent/data/output/optimization for the output log files (logs can be removed)
3. Currently the code uses data models which need to be copied into these locations from the original optimisation data files.:
      '..\\..\\data\\models\\gas_consumption_boilers' and '..\\..\\data\\models\\gas_consumption_gt'

4. Run the python file at /OptimisationAgent/district_heating/generation_optimisation/run_optimisation.py
