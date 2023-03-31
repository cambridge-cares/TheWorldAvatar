# RFID Agent

This agent is designed to extend the functionalities of the GAORFID XML API. The agent acts as a servlet that is capable of receiving data send via the API,
parse the data to extract the tag ID number, antenna number, RSSI value, timestamps and save these values in a postgreSQL database. The agent also provide
a GET interface that allows retrieval of historical data from the postgreSQL database with a GET request.

Before explaining the usage of the agent, we will briefly summarize the RFID API that is contacted by one of the classes in this package to retrieve data.

## RFID API

The API will be briefly described here. 

### Prerequisite
To use the API, the UHF Service Application provided by GAORFID must be deployed on a compatible RFID reader.

### Sending Data
Data is sent from the UHF Service Application in a XML format similar to the one below:
```
<s:Envelope xmlns:s="http://schemas.xmlsoap.org/soap/envelope/"> <s:Body> <GetRFIDData xmlns="http://tempuri.org/"><value1>00000000000000A000009727,0,-85</value1></GetRFIDData></s:Body></s:Envelope>
```

#### The endpoint
The endpoint for the RFID Agent that will receive the data sent from the UHF Service Application is:
``` 
http://<IP address>:1016/rfid-agent/
```

### Retrieving Data
Historical data can be retrieved by sending a GET request in the format below:
```
curl -X GET http://localhost:1016/rfid-agent/values=<status or RSSI or antennanumber>/limit=<any integer>/keys=<tag ID number 01>,<tag ID number 02>
```

#### Example readings
Readings are returned in the response body in form of a JSON Object which consist of key-value pairs. The values are in the form of JSON Arrays 
which contains multiple JSON Objects. Each JSON Objects consist of two key-value pairs with the keys being "ts" and "value" respectively.

The following shows a single JSON object for historical time-series data for multiple keys for the status value:
```
{"tag_00000000000000A000009726_status":[{"value":"Out","ts":1655966558492},{"value":"In","ts":1655964909815},{"value":"Out","ts":1655718804322}],
 "tag_00000000000000A000009727_status":[{"value":"In","ts":1655718784802},{"value":"Out","ts":1655718780343},{"value":"In","ts":1655718777121}]}
```

## Usage 
This part of the README describes the usage of the input agent. The module itself can be packaged into an executable war, deployed as a web servlet on tomcat. 
Sending the appropriate request to the correct URL will initiate the agent.

The [next section](#requirements) will explain the requirements to run the agent.

### Requirements
It is required to have access to a Postgres database. This can run on the same machine or need to be accessible from the host machine via a fixed URL.

This can be either in form of a Docker container or natively running on a machine. It is not in the scope of this README
to explain the set-up of a Postgres database.

### Property files
For running the agent, one property file is required:
- One [property file for the postgreSQL client](#client-properties) defining how to access the database.

#### client properties
The client property file needs to contain all credentials and endpoints to access the Postgres database. It should contain the following keys:
- `db.url` the [JDBC URL](https://www.postgresql.org/docs/7.4/jdbc-use.html) for the Postgres database
- `db.user` the username to access the Postgres database
- `db.password` the password to access the Postgres database

More information can be found in the example property file `client.properties` in the `config` folder.

### Building the RFID Agent
The RFID Agent is set up to use the Maven repository at https://maven.pkg.github.com/cambridge-cares/TheWorldAvatar/ (in addition to Maven central).
You'll need to provide  your credentials in single-word text files located like this:
```
./credentials/
    repo_username.txt
    repo_password.txt
```

repo_username.txt should contain your github username, and repo_password.txt your github [personal access token](https://docs.github.com/en/github/authenticating-to-github/creating-a-personal-access-token),
which must have a 'scope' that [allows you to publish and install packages](https://docs.github.com/en/packages/working-with-a-github-packages-registry/working-with-the-apache-maven-registry#authenticating-to-github-packages).

Modify `client.properties` in the `config` folder accordingly. The Dockerfile will copy all properties files and set environment variables pointing 
to their location thus you do not need to shift the properties files nor add in environment variables manually.

To build and start the agent, open up the command prompt in the same directory as this README, run
```
docker-compose up -d
```

The agent is reachable at "rfid-agent/" on localhost port 1016.


#### Run the agent
To run the agent, either a POST request is sent to http://localhost:1016/rfid-agent/ with data in the correct XML format or a GET request can be sent to http://localhost:1016/rfid-agent/ in the correct format. The POST request 
is usually send automatically via the UHF Service Application but it can be send manually as well.
An example POST request is shown below.
```
POST http://localhost:1016/rfid-agent/
Content-Type: application/xml
<s:Envelope xmlns:s="http://schemas.xmlsoap.org/soap/envelope/"> <s:Body> <GetRFIDData xmlns="http://tempuri.org/"><value1>00000000000000A000009727,0,-85</value1></GetRFIDData></s:Body></s:Envelope>
```

An example GET request is shown below.
```
GET http://localhost:1016/rfid-agent/values=status/limit=20/keys=00000000000000A000009727,00000000000000A000009726
```