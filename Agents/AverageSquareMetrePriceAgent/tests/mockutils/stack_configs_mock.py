# NOTE All URLs below refer to the endpoints to access Blazegraph/PostgreSQL from OUTSIDE the docker testing stack
#      (i.e. those are the endpoints required by the agent instance created in memory for localised agent tests 
#       to query/update data)
#
# To access Blazegraph/PostgreSQL from WITHIN the docker stack (i.e. by the dockerised agent instance), 'localhost'
# needs to be replaced with 'host.docker.internal' (This is done automatically when building the Docker image!)

from .env_configs_mock import DATABASE, NAMESPACE, THRESHOLD

DB_USER='postgres'
DB_PASSWORD='postgres'
DB_URL='jdbc:postgresql://localhost:7432/postgres'
QUERY_ENDPOINT='http://localhost:27149/blazegraph/namespace/kb/sparql'
UPDATE_ENDPOINT='http://localhost:27149/blazegraph/namespace/kb/sparql'
