# The endpoints provided here are suitable for running the agent tests both in debug
# and non-debug mode (using Docker within Docker)
# For external Blazegraph deployed on a server, e.g. http://www.theworldavatar.com/blazegraph,
# the exact URL should be provided

# PostgreSQL database details are specified to avoid issues when starting agent, although
# not needed for tests, as geospatial query (normally conducted in PostGIS) is mocked

from .env_configs_mock import DATABASE, NAMESPACE

DB_USER='postgres'
DB_PASSWORD='postgres'
DB_URL='jdbc:postgresql://host.docker.internal:7432/postgres'
QUERY_ENDPOINT='http://host.docker.internal:27149/blazegraph/namespace/kb/sparql'
UPDATE_ENDPOINT='http://host.docker.internal:27149/blazegraph/namespace/kb/sparql'