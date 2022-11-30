# NOTE the endpoints provided here are suitable for running the agent tests both locally and as dockerised tests.
# In the latter case `localhost` gets replaced with `host.docker.internal` while building the Docker image to ensure
# communication between dockerised agent and required test containers
# For external Blazegraph deployed on a server, e.g. http://www.theworldavatar.com/blazegraph, the exact URL should be provided

# PostgreSQL database details specified to avoid issues when starting agent, although
# not needed for tests, as geospatial query (normally conducted in PostGIS) is mocked

from .env_configs_mock import DATABASE, NAMESPACE

DB_USER='postgres'
DB_PASSWORD='postgres'
DB_URL='jdbc:postgresql://localhost:7432/postgres'
QUERY_ENDPOINT='http://localhost:27149/blazegraph/namespace/kb/sparql'
UPDATE_ENDPOINT='http://localhost:27149/blazegraph/namespace/kb/sparql'


# # Required settings for debugging tests within Docker
# DB_URL='jdbc:postgresql://host.docker.internal:7432/postgres'
# QUERY_ENDPOINT='http://host.docker.internal:27149/blazegraph/namespace/kb/sparql'
# UPDATE_ENDPOINT='http://host.docker.internal:27149/blazegraph/namespace/kb/sparql'