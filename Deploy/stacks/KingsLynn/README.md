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

## Using the scripts

The following scripts help to run the UPRN Agent from `https://github.com/cambridge-cares/CitiesKG/tree/uprn-agent` in batches of individual buildings to avoid head space issues when processing ~38,000 buildings at once as well as verifying whether all relevant UPRN information has been added to the KG

### _run_uprn_agent_in_chunks.py_
Requirements:

1) Build CKG agents per `https://github.com/cambridge-cares/CitiesKG/tree/develop/agents`, but please use the correct UPRN agent branch: `https://github.com/cambridge-cares/CitiesKG/tree/uprn-agent` (requires Java 8!)

2) Deploy .war file in local Tomcat server

3) Deploy Access Agent via Docker as described here `https://github.com/cambridge-cares/TheWorldAvatar/tree/main/JPS_ACCESS_AGENT` and upload correct endpoint mappings `https://github.com/cambridge-cares/TheWorldAvatar/blob/main/JPS_ACCESS_AGENT/access-agent-dev-stack/routing.json` 

4) Start local Blazegraph with OntoCityGML building  instances (which already have been enriched with LOD0 footprints using the TSD Agent as described here `https://github.com/cambridge-cares/CitiesKG/tree/develop/agents#footprint-mode`)

5) Specify `blazegraph` and `agent` endpoints in `run_uprn_agent_in_chunks.py` as well as waiting time between individual agent requests - this might need experimenting, as the HTTP request to agent returns <200> after a successful request; however, the actual upload of triples to Blazegraph might take longer. Hence, too little waiting time between requests will suppress successful instantiation of UPRN triples in Blazegraph. 10s waiting time seem to work well

### _compare_uprns.py_

Assess the added UPRN information via the agent with static data once added from Ordnance Survey using FME