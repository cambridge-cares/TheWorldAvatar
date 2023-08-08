# Heat Emission Agent GeoJson
 This agent is to return the heat emission information of different buildings/objects in the GeoJson format based on the Heat Emission Agent. 

# Usage 
This part of the README describes the usage of the input agent. The agent itself can be packaged into an executable war, deployed as a web servlet on tomcat. Sending the appropriate request to the correct URL will initiate the agent. 

The next section explains the requirements to run the agent.

# Requirements
It is required to have access to a knowledge graph SPARQL endpoint. These can run on the same machine or need to be accessible from the host machine via a fixed URL.

This can be either in form of a Docker container or natively running on a machine. 

# Building the Heat Emission Agent GeoJson

The Heat Emission Agent GeoJson is set up to use the Maven repository at https://maven.pkg.github.com/cambridge-cares/TheWorldAvatar/ (in addition to Maven central). You'll need to provide your credentials in single-word text files located like this:

./credentials/
    repo_username.txt
    repo_password.txt

repo_username.txt should contain your github username, and repo_password.txt your github [personal access token](https://docs.github.com/en/github/authenticating-to-github/creating-a-personal-access-token),
which must have a 'scope' that [allows you to publish and install packages](https://docs.github.com/en/packages/working-with-a-github-packages-registry/working-with-the-apache-maven-registry#authenticating-to-github-packages).

To build and start the agent, open up the command prompt in the same directory as this README, run

docker-compose up -d

The agent is reachable at "heatemissionagentgeojson/performheatgeojson" on localhost port 1080.

# Run the agent
To run the agent, a POST request must be sent to http://localhost:1080/heatemissionagentgeojson/performheatgeojson with any JSON Object.
Follow the request shown below.

POST http://localhost:1080/heatemissionagentgeojson/performheatgeojson
Content-Type: application/json
{"Heat Output":"GeoJson Format"}

In curl syntax:

curl -X POST --header "Content-Type: application/json" -d "{\"Heat Output\":\"GeoJson Format\"}"  http://localhost:1080/heatemissionagentgeojson/performheatgeojson

If the agent runs successfully, you should see a returned JSON Object that is similar to the one shown below.

{"Result":"GeoJson heat data outputted."}

The final required txt file in the GeoJson format can be located under the output folder

