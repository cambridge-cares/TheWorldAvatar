# NOTE the triple store URL provided here is the URL to access the Blazegraph container WITHIN the Docker testing stack (not! the CMCL stack)
# If Blazegraph is accessed from outside the docker stack, i.e. accessed by agent instance created in memory for localised agent test, then 
# the "host.docker.internal" should be replaced with "localhost", which can be done by `host_docker_internal_to_localhost` function
# For external Blazegraph deployed on a server, e.g. http://www.theworldavatar.com/blazegraph, the exact URL should be provided

from .env_configs_mock import DATABASE, NAMESPACE, THRESHOLD

DB_USER='postgres'
DB_PASSWORD='postgres'
DB_URL='jdbc:postgresql://host.docker.internal:7432/postgres'
QUERY_ENDPOINT='http://host.docker.internal:27149/blazegraph/namespace/kb/sparql'
UPDATE_ENDPOINT='http://host.docker.internal:27149/blazegraph/namespace/kb/sparql'
