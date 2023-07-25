# Heat Emission Agent
 This agent is to return the heat emission information of different buildings/objects presented in a given area. Therefore, the heat emission data can be automatically assigned to corresponding buildings within a specific area. This is of interest to the Cooling Singapore 2.0 Project. 
 
 It demonstrates firstly the feasibility of a cross-domain query and secondly the evaluation of the heat emission data in terms of emission values and respective coordinates within a bounding box in Jurong Island. To achieve this, it consists of four parts. First, we obtain all the chemical plants, plant items, IRIs and CO2 emission via query in "jibusinessunits"; Second, for a particular chemical plant, its fuel CEI and thermal efficiency are queried; subsequently, all the heat emission coordinates are evaluated via query in "jriEPSG24500"; finally, the heat emission values are calculated with CO2 emission, CEI and efficiency and assigned to the emission coordinates, after a filter based on a boundary area specified.The heat emission data are saved into the TheWorldAvatar blazegraph.

# Example Heat Emission Data
Heat Emission Data is returned to the response body in form of a JSON Object which consist of key-value pair. The JSONObject has the 
key:"result", which contains a JSONArray containing JSONObjects. Each of these individual JSONObjects found within the JSONArray
provide the heat emission coordinates and the corresponding heat emission value in the unit of MW.

# Usage 
This part of the README describes the usage of the input agent. The agent itself can be packaged into an executable war, deployed as a web servlet on tomcat. Sending the appropriate request to the correct URL will initiate the agent. 

The next section explains the requirements to run the agent.

# Requirements
It is required to have access to a knowledge graph SPARQL endpoint. These can run on the same machine or need to be accessible from the host machine via a fixed URL.

This can be either in form of a Docker container or natively running on a machine. It is not in the scope of this README to explain the set-up of a knowledge graph triple store

# Building the HeatEmission Agent

The HeatEmission Agent is set up to use the Maven repository at https://maven.pkg.github.com/cambridge-cares/TheWorldAvatar/ (in addition to Maven central). You'll need to provide your credentials in single-word text files located like this:

./credentials/
    repo_username.txt
    repo_password.txt

repo_username.txt should contain your github username, and repo_password.txt your github [personal access token](https://docs.github.com/en/github/authenticating-to-github/creating-a-personal-access-token),
which must have a 'scope' that [allows you to publish and install packages](https://docs.github.com/en/packages/working-with-a-github-packages-registry/working-with-the-apache-maven-registry#authenticating-to-github-packages).

To build and start the agent, open up the command prompt in the same directory as this README, run

docker-compose up -d

The agent is reachable at "heat-agent/performheatquery" on localhost port 1080.

# Run the agent
To run the agent, a POST request must be sent to http://localhost:8084/heat-agent/performheatquery with a correct JSON Object.
From a Windows Subsystem for Linux (WSL) terminal, execute the request shown below.

curl -X POST -H "Content-Type: application/json" -d '{"job":{"lower_bounds":"8464#23588#0","upper_bounds":"17619#30520#105"}}' http://localhost:8084/heat-agent/performheatquery


If the agent runs successfully, you should see a returned JSON Object that is similar to the one shown below.

{"result":[{"Coordinate":"13469.086796413478#26462.33106185692#41.0","Heat Emission":80.22970472647032},{"Coordinate":"13252.157092925416#28023.664545558153#25.0","Heat Emission":80.22970472647032},{"Coordinate":"13341.125256486484#26898.004451070658#21.0","Heat Emission":80.22970472647032}]}

If there is a returned message as shown below, it means that the input JSON object is written wrongly. Check whether the values and formats are written correctly.

"Error in input parameters, please check the input file"
