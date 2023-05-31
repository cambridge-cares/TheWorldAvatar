# Device Instantiation Agent

This agent is responsible for instantiating sensors, microcontrollers and devices in a knowledge graph. The agent took in informations from a `.json` file (the device descriptor) and instantiates microcontroller modules with their respective sensor and readings instances. The agent is designed with the device instantiation framework in mind.

Instantiation is based on ontodevice ontology.

## Requirements

### Device Instantiation framework
The device instantiation framework is a framework to allow fast and consistent sensor instantiation on the knowledge graph to allow faster deployment and development. It can be summarised in the following figure:


Sensors will be connected to a microcontroller and it will send data to an IoT database. For this agent Thingsboard is used for the development of the agent. The Post-Processing agent will pull the data from the database and instantiate a timeseries of the dataon the knowledge grap. THis wirk with and without the use of the stack. If the stack is usedm the agent will send the data to the [data bridge agent]() for instantiation. The post processing agent will also be responsible for instantiating agent and derivation instances when needed. It is assumed that the post processing agent will be run first before the device instantiation agent.

 The device instantiaion agent will be responsible for instantiating the sensors and devices in the knowledge graph using a device descrpitor file written by the user.  

### Descriptor files
The agent require a description of the device instantiated. This description must be written in a `.json` file for the agent to take in. A template of the file is provided in the repository.

Each device will require 1 descriptor file. A descriptor file can be composed of 1 microcontroller and several sensor modules. Each sensor modules can be composed of several sensors. Each sensor will have 1 type of sensor reading. The diagram of the json file structure is shown in image below.

THe file can be sparated into 3 different main keys, namely: 
- MicroController: The descriptor of the device. Contains informations of the sensors and readings.
    - type: The type of mucrocontrller used (ESP32, etc.).
    - name : The unique id of the mictontorller
    - label: The rdf:Label of the microcontroller 
    - MainSensorMap: The map of the sensor modules connected to the microcontroller.  Composed of a JSONObject that maps the sensor names and their respective information. Each sensor map is composed of (relevant to the device instantiation agent): sensor type, output, output datatype and unit, Thingsboard field name
    -  derivation: Maps the derivation of the raw readings ot the derived variables. The content of the key is optional as some use cases, no derivation will be done by the agent and raw readings will be instantiated instad.

- IRIMapper: Map of specfic IRI. As the agent will default to ontodevice for the instantiation, in the case an IRI from a custom ontology is needed, the IRI will need to be specified here.

- AdditionalQuery: For update queries that are not automatically ran by the agent. Instances created automatically by the agent can be found in the following section.

The file also contain informations such as the pin connections of the sensors and as the json file is also used in the template maker to make the arcuino script for the microcontorller. However these inforamtions are not vital to the device instantiation agent and can be ignored if the user does not plan to use the template maker for their microcontroller scripts. 

### Instantiation format

Generally the following instances are created by the agent without any additional queries:

[Image here]

## Running the agent

The agent could be build using the following command in the folder of the repo:
```
docker-compose up -d
```

The agent will be available at port `1017/retrieve`. 
The agent accepts a POST request containing:
- `CLIENTPROPERTIES` : The location of the client proiperties file containing the SPARQL endpoint of the KG. File location is stored in an environment variable `CLIENTPROPERTIES`.
- The device descriptor.

An example of the request:
[PUT EXAMPLE HERE]

