# Heat Emission Agent

The heat emission agent calculates heat emissions of various types of factories. The factories' data needs to have been instantiated in a blazegraph namespace according to either the [OntoCompany](https://github.com/cambridge-cares/TheWorldAvatar/tree/main/JPS_Ontology/ontology/ontocompany) or [OntoChemPlant](https://github.com/cambridge-cares/TheWorldAvatar/tree/main/JPS_Ontology/ontology/ontochemplant) ontologies. The former ontology would be used for namespaces containing data for various different types of factories while the latter is suitable for instantiating data for petrochemical refining hubs such as Jurong Island. The heat emissions values are instantiated in the same namespace from which the companies' properties are queried:


The steps required to run the agent for each case are described below:

## Instructions

### 1. Agent Deployment

The agent's docker container can be run within a stack or as a standalone container.  


### 1.1 Preparation

#### Maven Repository credentials

This agent is set up to use this [Maven repository](https://maven.pkg.github.com/cambridge-cares/TheWorldAvatar/) (in addition to Maven central). You'll need to provide your credentials (github username/personal access token) in single-word text files located like this:
```
./credentials/
        repo_username.txt
        repo_password.txt
```
repo_username.txt should contain your Github username. repo_password.txt should contain your Github [personal access token](https://docs.github.com/en/github/authenticating-to-github/creating-a-personal-access-token),
which must have a 'scope' that [allows you to publish and install packages](https://docs.github.com/en/packages/working-with-a-github-packages-registry/working-with-the-apache-maven-registry#authenticating-to-github-packages).


#### Blazegraph namespace

The blazegraph namespace contains manufacturing facilities' data in the form of triples. The agent performs heat emissions calculations for the following two scenarios

(1) Triples instantiated according to [OntoCompany](https://github.com/cambridge-cares/TheWorldAvatar/tree/main/JPS_Ontology/ontology/ontocompany). In this case, values for the following attributes need to be present for each type of factory:

- Chemical Plant: Specific energy consumption in units of megaJoules per kilogram, production volume in tons per year and thermal efficiency, which must be between 0 and 1.

Although the agent currently only performs heat emissions calculations for chemical plants, it is being extended to include data centres and all other factory types in OntoCompany. The triples can be instantiated according to this ontology using the [heat instantiation agent](https://github.com/cambridge-cares/TheWorldAvatar/tree/1690-add-heat-instantiation-agent/Agents/HeatInstantiationAgent).

(2) Triples instantiated according to [OntoChemPlant](https://github.com/cambridge-cares/TheWorldAvatar/tree/main/JPS_Ontology/ontology/ontochemplant). In this case, each chemical plant instance in the knowledge graph must contain at least one instance of the PlantItem class. Each PlantItem instance must have an associated CO2 emissions value. The heat emissions values are calculated separately for each plant item.


##### Stack containers
If the agent is being run as part of a stack, the user can opt to use a namespace located in the stack blazegraph. The procedure for spinning up the stack is described at [stack manager page](https://github.com/cambridge-cares/TheWorldAvatar/tree/main/Deploy/stacks/dynamic/stack-manager).

#### 1.2 Docker Deployment

- From the same directory location as this README, build the agent's image using the `docker compose build` command. If the container is run as part of a stack, copy the `heatemissionagent.json` file from the `stack-manager-input-config` folder into the `Deploy/stacks/dynamic/stack-manager/inputs/config/services` folder of the stack manager before starting the stack.


### 2. Agent route

The agent has a single API route which requires a POST request. It accepts the following input parameters:

- ```namespace```: The name of the Blazegraph namespace from which the companies' properties are queried. This parameter should be included for cases in which this data is stored in a namespace in the stack blazegraph.
-```endpoint```: The full endpoint of the Blazegraph namespace from which the companies' properties are queried. This parameter should be used if this data is stored in a blzegraph namespace outside the stack.
- ```ontology```: The ontology used to instantiate the triples in the SPARQL endpoint. This must be either 'ontocompany' or 'ontochemplant'. The agent executes different SPARQL queries in the Blazegraph endpoint dpeending upon the value of this parameter. 

- ```lower_bounds```: The lower bounds of the bounding box. This must be specified as three floating point values separated by a '#' symbol. It is only applicable for the ontocompany case and has no effect otherwise. If run for a namespace with OntoChemPlant triples, the agent only calculates heat emissions and instantiates the triples for heat sources located within this bounding box.
-```upper_bounds```: The upper bounds of the bounding box. This must be specified as three floating point values separated by a '#' symbol. It is only applicable for the ontocompany case.

Some example POST request for different values of input parameters are listed below.


#### Running the agent as a standalone Docker container for OntoCompany triples

In this example, the triples are assumed to be instantiated in a local namespace called 'sgbusinessunits'.

```
curl -X POST -H "Content-Type: application/json" -d '{"ontology":"ontocompany","endpoint":"http://localhost:48889/blazegraph/namespace/sgbusinessunits/sparql"}'  "http://localhost:8082/heatemissionagent/performheatquery"
```

#### Running the agent as a standalone Docker container for OntoChemPlant triples

In this example, the triples are assumed to be instantiated in a local namespace called 'jibusinessunits'.

```
curl -X POST -H "Content-Type: application/json" -d '{"lower_bounds":"8464#23588#0","upper_bounds":"17619#30520#105","ontology":"ontochemplant","endpoint":"http://localhost:48889/blazegraph/namespace/jibusinessunits/sparql"}'  "http://localhost:8082/heatemissionagent/performheatquery"
```



#### Running the agent within a stack for OntoCompany triples

In this example, the triples are assumed to be instantiated in a stack namespace called 'sgbusinessunits'. The stack is assumed to have been spun up using the default port 3838.

```
curl -X POST -H "Content-Type: application/json" -d '{"ontology":"ontocompany","namespace":"sgbusinessunits"}'  "http://localhost:3838/heatemissionagent/performheatquery"
```

#### Running the agent within a stack for OntoChemPlant triples

In this example, the triples are assumed to be instantiated in a stack namespace called 'jibusinessunits'. The stack is assumed to have been spun up using the default port 3838.

```
curl -X POST -H "Content-Type: application/json" -d '{"lower_bounds":"8464#23588#0","upper_bounds":"17619#30520#105","ontology":"ontochemplant","endpoint":"http://localhost:48889/blazegraph/namespace/jibusinessunits/sparql"}'  "http://localhost:3838/heatemissionagent/performheatquery"
```



```
curl -X POST -H "Content-Type: application/json" -d '{"lower_bounds":"8464#23588#0","upper_bounds":"17619#30520#105","ontology":"ontocompany"}'  "localhost:3838/heatemissionagent/performheatquery"
```


#### Return values 


For the ontocompany case, the agent prints the number of factories for which heat emissions were calculated in the form of a JSON object upon successful completion. An example is shown below.

`
{"success": "true", "number_factories": 96 }
`

For the ontochemplant case, the agent prints the coordinates of the heat sources and their corresponding heat emissions in the form of a JSON object upon successful completion. An example is shown below: 

`{"result":[{"Coordinate":"13469.086796413478#26462.33106185692#41.0","Heat Emission":80.22970472647032},{"Coordinate":"13252.157092925416#28023.664545558153#25.0","Heat Emission":80.22970472647032},{"Coordinate":"13341.125256486484#26898.004451070658#21.0","Heat Emission":80.22970472647032}]}
`
