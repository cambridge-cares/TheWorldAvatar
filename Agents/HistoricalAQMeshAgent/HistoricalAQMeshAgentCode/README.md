# AQMesh input agent package
This README describes the classes and their public methods contained in the AQMesh input agent module.
For more detail check the classes and documentation contained in the source files.

## AQMeshInputAgentLauncher
This class has only one (main) method, which at the same time is the entrypoint of the executable jar. The main method
orchestrates the other classes and forms a pipeline to receive and store data from the AQMesh API. The pipeline contains
the following steps:
1. Create the agent object that wraps the time-series client (see also [AQMeshInputAgent](#aqmeshinputagent)).
2. Create the time-series client object that interacts with the knowledge graph and database, and pass it to the agent.
3. If necessary initialize time-series instances and relationships in the knowledge graph, and the tables in the 
database using the agent. This step is only done once when run for the first time.
4. Create the API connector that interacts with the AQMesh API (see also [AQMeshAPIConnector](#aqmeshapiconnector)).
5. Retrieve particle and gas readings from the API using the connector.
6. If both readings are not empty the database is updated with the readings using the agent.

If any of the steps fails, an error is thrown with an informative message.

## AQMeshInputAgent
This class is a wrapper for the time-series client and adds functionality and utility methods that are specific for the
AQMesh API.

### Constructor
The agent is initialized with a property file that contains a path to a folder containing JSON key to IRI mappings. For
each file in the mapping folder a JSONKeyToIRIMapper is created (see [JSONKeyToIRIMapper](#jsonkeytoirimapper)).

### Public fields
There are a few public, static fixed fields that define specific properties and might need to be changed if the API
specifics change:
- The prefix used when generating IRIs (`generatedIRIPrefix`)
- The time unit used when creating a time-series instance (`timeUnit`)
- The JSON key in the readings that denotes the timestamp (`timestampKey`)
- The offset from UTC time used for the timestamps that are stored in the database (`ZONE_OFFSET`). This should not be
changed as UTC should be the default timezone. But some processing (conversion) might be required if the timezone of the
AQMesh API (currently UTC) changes.

### Public methods
There are a few utility methods:
- `getNumberOfTimeSeries` to retrieve the number of time-series (mappings).
- `setTsClient` to pass the time-series client to the agent.

The main functionality of the agent is accessible through two public methods:
- `initializeTimeSeriesIfNotExist`: this method is used to invoke the time-series client to initialize all the time-series'
and connections when they do not exist yet (when the whole agent is run for the first time).
- `updateData`: this method is used by the launcher to update the database. It takes the readings and invokes the 
time-series client's add data method.

## AQMeshAPIConnector
This class interacts with the AQMesh API. It is used by the launcher class to retrieve new readings. However, it can
also be used outside this module (by importing the jar) to directly interact with the API if required. Note, that there
should be only one entity (agent, person, piece of code) that retrieves readings from the API per device (see the 
README in the root folder for more information). 

### Constructor
The connector is initialized providing the API URL and credentials, as well as, optionally the pod index (default is 0),
or alternatively with a property file that contains the same information.

### Public methods
The connector has the following public methods that provide a java API to interact with the AQMesh API and removes the
need to know about the specifics of authentication, building HTTP requests and processing HTTP responses:
- `connect`: This method needs to be invoked before any other public method and establishes the connection to the API, 
i.e retrieves the token.
- `ping`: Sends a request to the ping endpoint and returns the current server time.
- `getParticleReadings`: Retrieve the new particle readings using the *Next* endpoint. The method makes sure to retrieve
a location first if none is set yet.
- `getGasReadings`: Retrieve the new gas readings using the *Next* endpoint. The method makes sure to retrieve
a location first if none is set yet.

## JSONKeyToIRIMapper
This class represents a mapping between JSON keys (from the API response when retrieving data) and IRIs (used to 
identify instances in the knowledge graph). It is basically a wrapper for two hash maps that map between the keys and 
IRIs and vice versa.

### Constructor
The mapper can be initialized providing a prefix (used for keys without IRI), and alternatively a properties file from which
to read the mapping. If using the constructor with only the prefix, the method `readMappingFromFile` can be used to read
the mapping later.

### Public methods
The mapper has public methods to read, save and retrieve mappings:
- `readMappingFromFile` reads mappings from a property file. The method also checks that the IRIs are valid, no IRIs are
used twice, and generates a new IRI if there is none set for a key.
- `saveToFile` saves the mapping stored in the hash maps back to a property file.
- `generateIRI` can be used to generate an IRI given a key. This automatically generated IRI will have the
following form: `[prefix]/[key]_[UUID]`, where the `[prefix]` is the prefix set when constructing the mapper or using 
the set method, `[key]` is the JSON key passed to the method, and `[UUID]` is a randomly generated UUID.
- `getIRI` retrieves the IRI given a JSON key.
- `getJSONKey` retrieves the JSON key given an IRI.
- `getAllIRIs` and `getAllJSONKeys` returns all IRIs or JSON keys respectively that exist in the mapping.
- `getIRIPrefix` and `setIRIPrefix` can be used to retrieve or change the prefix.