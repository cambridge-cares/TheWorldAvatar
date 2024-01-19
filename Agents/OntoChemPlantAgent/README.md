# OntoChemPlant Agent
This agent queries the knowledge graph for information on various structures on Jurong Island, and interacts with the
City Information Agent (CIA) to display this information on the Jurong Island web visualisation
found at http://www.theworldavatar.com:83/citieskgweb/index.html?city=jurongisland?context=chemplant

In order to query the KG, a CityObject IRI as a JSON Object is passed to this agent as an input. This IRI is sent by the CIA
when a user interacts with a structure on the web visualisation. The agent then creates appropriate models using this IRI based on the
Modeling Engine Framework, and pulls relevant information regarding the structure from the knowledge graph at the specified context.
The output information is sent back to the CIA as a JSON Object, and is displayed on the visualisation.

# Building the agent

The HeatEmission Agent is set up to use the Maven repository at https://maven.pkg.github.com/cambridge-cares/TheWorldAvatar/ 
(in addition to Maven central). You'll need to provide your credentials in single-word text files located like this:

./credentials/ 
repo_username.txt
repo_password.txt

repo_username.txt should contain your github username, and repo_password.txt your github [personal access token](https://docs.github.com/en/github/authenticating-to-github/creating-a-personal-access-token),
which must have a 'scope' that [allows you to publish and install packages](https://docs.github.com/en/packages/working-with-a-github-packages-registry/working-with-the-apache-maven-registry#authenticating-to-github-packages).

To build and start the agent locally, open up the command prompt in the same directory as this README, run
```
docker-compose up -d
```
The agent is reachable at http://localhost:1083/ontochemplant-agent/query.

# Running the agent
To run the agent, a POST request must be sent to http://localhost:1083/ontochemplant-agent/query with the input as a JSON Object.
Below is an example input for the POST request. This input follows the same format as required by the CIA.
```
POST http://localhost:1083/ontochemplant-agent/query 
Content-Type: json 
{"iris": ["http://www.theworldavatar.com:83/citieskg/namespace/jriEPSG24500/sparql/cityobject/UUID_bd07e1dd-7ffe-4776-8cf0-5409c007e437/"]}
```
If the agent runs successfully, you should see a returned JSON Object that is similar to the one shown below.

```
{"chemplant":[{"costs":[{"iri":"http://www.theworldavatar.com/kb/ontochemplant/Costs_of_plantitem_217","context":{"quads":false}}],
"iri":"http://www.theworldavatar.com/kb/ontochemplant/Plant_item217","individualCO2Emission":[{"iri":
"http://www.theworldavatar.com/kb/ontochemplant/CO2_of_plantitem_217","context":{"quads":false},"CO2Emission_TonsPerYear":202445.36}],
"generatedHeat":[{"iri":"http://www.theworldavatar.com/kb/ontochemplant/Plant_item217_Heat","context":{"quads":false},
"generatedHeat_MegaWatt":50.948414711732816}],"context":{"quads":false},"cityFurnitureIRI":
"http://www.theworldavatar.com:83/citieskg/namespace/jriEPSG24500/sparql/cityfurniture/UUID_bd07e1dd-7ffe-4776-8cf0-5409c007e437/",
"ownedBy":"http://www.theworldavatar.com/kb/ontochemplant/Chemical_plant_of_ExxonMobilPAC"}]}
```

