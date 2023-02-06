# Kings Lynn Utilities

This project contains several modules with utility functions and helper scripts for the King's Lynn use case.

&nbsp;
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

&nbsp;
## 1) Using the UPRN Agent scripts

> The following steps refer to commit `68abb0eb4c24438fa46a3acdb2f6a3c3786292a8` on `https://github.com/cambridge-cares/CitiesKG/tree/develop`

The scripts within the `uprn_agent` subdirectory help to run the UPRN Agent from `https://github.com/cambridge-cares/CitiesKG/tree/develop/agents` in batches of individual buildings. This workaround might be necessary to 1) avoid heap space issues when processing the entire Blazegraph namespace at once (i.e. containing ~38,000 buildings for King's Lynn) and/or JSON Exceptions "arbitrarily" thrown by the UPRN Agent. Furthermore, a script is provided to verifying whether all relevant UPRN information has been added "correctly" to the KG.

### _run_uprn_agent_in_chunks.py_
Requirements:

1) Build CKG agents following the instructions in [Semantic 3D City Agents README] on branch `https://github.com/cambridge-cares/CitiesKG/tree/develop/agents` (requires Java 8!)

2) Deploy .war file in local Tomcat server

3) Deploy Access Agent (locally) via Docker as described in [Access Agent README] (commit `db76f502fd293c69c509371cc9694d19accda0fa` on branch `https://github.com/cambridge-cares/TheWorldAvatar/tree/main`) and upload correct endpoint mappings

4) Ensure Blazegraph namespace with OntoCityGML building instances is available (which need to be already enriched with LOD0 footprints using the TSD Agent as described here `https://github.com/cambridge-cares/CitiesKG/tree/develop/agents#footprint-mode`)

5) Specify `blazegraph` namespace and `agent` endpoins in [run_uprn_agent_in_chunks.py] as well as waiting time between individual agent requests - this might need experimenting, as the HTTP request to agent returns <200> after a successful request; however, the actual upload of triples to Blazegraph might take longer. Hence, too little waiting time between requests will suppress successful instantiation of UPRN triples in Blazegraph. 5-10s waiting time seem to work well.

6) An KG export in N-Quads after successfully running the UPRN agent on the King's Lynn building data is provided in `\Data\KG snapshots\20220901 kings_lynn`

### _compare_uprns.py_

Assess the added UPRN information via the agent with static data once added from Ordnance Survey using FME.

&nbsp;
## 2) Using the Building Retrieval scripts

It has been observed that not all buildings instantiated in OntoCityGml get UPRN information attached after running the UPRN agent. The scripts within the `building_retrieval` subdirectory help to assess whether there is a specific pattern for buildings with vs. without UPRN information after running the agent, i.e. the `query_buildings.py` script is used to retrieve building footprints for buildings with vs. without instantiated UPRN information to analyse potential structural differences between respective buildings. Primary focus is to understand whether the non-retrieval of UPRN information using the UPRN agent seems legit or rather erroneous. The script creates .geojson files to be overlayed with other maps in QGIS / FEM Data Inspector to visually analyse potential structural differences.

&nbsp;
## Knowledge Graph utilities

The scripts within the `kg_utils` subdirectory provide functionality to interact with an online knowledge graph (i.e. SPARQL endpoint):

> The `export_triples.py` script can be used to export all triples from a specified Blazegraph endpoint and serialse them in NTriples format in the `data/outputs` folder (file name will be `triples.nt`). This script has been used to create snapshots of the knowledge base along the instantiation workflow (see `..\Data\KG snapshots` in the overall project folder).

> The `import_triples.py` script can be used to upload all triples from a `.nt` file to an online Blazegraph SPARQL endpoint. The specified namespace might need to be created manually beforehand.

> The `compare_rdf_files.py` script can be used to compare two `.nt` files and identify differences, i.e. triples only present in one file but not the other as well as triples which are present in both files.

<!-- Links -->
[Semantic 3D City Agents README]: https://github.com/cambridge-cares/CitiesKG/tree/develop/agents
[Access Agent README]: https://github.com/cambridge-cares/TheWorldAvatar/tree/main/JPS_ACCESS_AGENT
[run_uprn_agent_in_chunks.py]: \uprn_agent\run_uprn_agent_in_chunks.py