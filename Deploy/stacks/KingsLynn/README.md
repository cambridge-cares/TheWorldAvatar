# Kings Lynn Flooding

## Setting up a virtual environment and installing required packages

It is highly recommended to use a virtual environment for this project. The virtual environment can be created as follows (`python` command might need to be replaced with `py` depending on whether Python is specified in system's `PATH` variables):

```cmd
$ python -m venv venv
$ venv\Scripts\activate.bat
(venv) $
```

The above commands will create and activate the virtual environment `venv` in the current directory. Install all required packages listed in `requirements.txt`:

```cmd
(venv) $ python -m pip install --upgrade pip  
(venv) $ python -m pip install -r requirements.txt
```

## Using the UPRN agent scripts

> The following steps refer to commit `2b5869650c39d8c754edfec98b6cde431a14fb06` on `https://github.com/cambridge-cares/CitiesKG/tree/uprn-agent`

The scripts within the `uprn_agent` subdirectory help to run the UPRN Agent from `https://github.com/cambridge-cares/CitiesKG/tree/uprn-agent` in batches of individual buildings. This workaround is necessary to avoid heap space issues when processing ~38,000 buildings in King's Lynn at once. Furthermore, a script is provided to verifying whether all relevant UPRN information has been added "correctly" to the KG.

### _run_uprn_agent_in_chunks.py_
Requirements:

1) Build CKG agents following the instructions on branch `https://github.com/cambridge-cares/CitiesKG/tree/develop/agents` (commit `63c0321e10e1ed0cbc0b561de8aac80438bf0d7c`), but please use the correct UPRN agent branch for the actual code: `https://github.com/cambridge-cares/CitiesKG/tree/uprn-agent` (requires Java 8!)

2) Deploy .war file in local Tomcat server

3) Deploy Access Agent via Docker as described here `https://github.com/cambridge-cares/TheWorldAvatar/tree/main/JPS_ACCESS_AGENT` (commit `18ded8d1626ad05b62c05eab73f6740d35c57944`) and upload correct endpoint mappings `https://github.com/cambridge-cares/TheWorldAvatar/blob/main/JPS_ACCESS_AGENT/access-agent-dev-stack/routing.json` 

4) Start local Blazegraph with OntoCityGML building instances (which need to be already enriched with LOD0 footprints using the TSD Agent as described here `https://github.com/cambridge-cares/CitiesKG/tree/develop/agents#footprint-mode`) (commit `63c0321e10e1ed0cbc0b561de8aac80438bf0d7c`)

5) Specify `blazegraph` and `agent` endpoints in `run_uprn_agent_in_chunks.py` as well as waiting time between individual agent requests - this might need experimenting, as the HTTP request to agent returns <200> after a successful request; however, the actual upload of triples to Blazegraph might take longer. Hence, too little waiting time between requests will suppress successful instantiation of UPRN triples in Blazegraph. 10s waiting time seem to work well.

6) An KG export in N-Quads after successfully running the UPRN agent on the King's Lynn building data is provided in `\Data\KG snapshots\20220901 kings_lynn`

### _compare_uprns.py_

Assess the added UPRN information via the agent with static data once added from Ordnance Survey using FME.

## Using the Building Retrieval scripts

The `query_buildings.py` script is used to retrieve building footprints for buildings with vs. without instantiated UPRN information to analyse potential structural differences between respective buildings. Primary focus is to understand whether the non-retrieval of UPRN information using the UPRN agent seems legit or rather erroneous. The script creates .geojson files to be overlayed with other maps in QGIS / FEM Data Inspector to visually analyse potential structural differences.
