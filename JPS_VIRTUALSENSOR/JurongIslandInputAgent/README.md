# Overview
Queries emissions and OntoCityGML IRIs of pollutant emitting points on Jurong Island from the http://www.theworldavatar.com/blazegraph/namespace/jibusinessunits/sparql endpoint. This data has been instantiated as per the OntoChemPlant ontology. The queried data is then instantiated in the stack blazegraph according to the OntoDispersion ontology. 

# Requirements
Follow the instructions in Deploy/stacks/dynamic/stack-manager/README.md to spin up a stack.

# API
Start the agent by executing the following command from within this folder. Replace STACK_NAME with the actual stack name:

./stack.sh start <STACK_NAME>

Send a GET reuest to the agent as follows from an Ubuntu terminal:

curl -X GET http://localhost:8082/JurongIslandInputAgent/update

If everything works correctly, the agent returns a JSON response with an "emissionsUpdate" parameter having a value of "done". 