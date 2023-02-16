## Description
This agent is designed to calculate fuel emission (total emission, gas emission and electricity emission) based on a series of input parameters, details provided below:
### Agent Setup
Before building and deploying the Docker image, several key properties need to be set in the [Docker compose file] (further details and defaults are provided in the file):
```bash
# Stack & Stack Clients configuration
STACK_NAME            # Name of stack to which agent shall be deployed
NAMESPACE             # Blazegraph namespace into which to instantiate data
DATABASE              # PostGIS/PostgreSQL database name (default: `postgres`)
LAYERNAME             # Geoserver layer name, ALSO table name for geospatial features in PostGIS
GEOSERVER_WORKSPACE   
ONTOP_FILE            # Path to ontop mapping file (i.e. within Docker container)
```


### How to deploy this agent on stack
Details about the routes on the stack establishment, and how to deploy the agent on the stack can be found [here](https://htmlpreview.github.io/?https://github.com/cambridge-cares/TheWorldAvatar/blob/dev-heat-pump-migration-to-stack-2/Agents/LSOAInputAgent/deploy_agent_on_stack.html)

### How to use this agent
An overview of all provided API endpoints and their functionality is provided after agent start-up at the API root http://localhost:5030/. All requests are to be sent as GET requests

Details about how to use the agent please see the [home page](https://htmlpreview.github.io/?https://github.com/cambridge-cares/TheWorldAvatar/blob/dev-heat-pump-migration-to-stack-2/Agents/LSOACalculationAgent_fuel_emission/agent/flaskapp/templates/index.html) of this agent


<!-- files -->
[Dockerfile]: Dockerfile
[docker compose file]: docker-compose.yml
[docker-compose.test.yml]: tests\docker-compose.test.yml
[example retrieve all request]: resources\HTTPRequest_retrieve_all.http
[resources]: resources
[stack.sh]: stack.sh
[tests]: tests