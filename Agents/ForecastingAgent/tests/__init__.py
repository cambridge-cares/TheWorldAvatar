import os

# Set environment variables for testing (i.e. agent deployed as Flask App)
# The values provided here need to match information in docker-compose.test file
# Furthermore, provided endpoints must match KG_ROUTE and DATABASE value in conftest

# os.environ["STACK_NAME"] = ""
# os.environ["DB_URL"] = "jdbc:postgresql://localhost:7432/postgres"
# os.environ["DB_USER"] = "postgres"
# os.environ["DB_PASSWORD"] = "postgres"
# os.environ["SPARQL_QUERY_ENDPOINT"] = "http://localhost:27149/blazegraph/namespace/kb/sparql"
# os.environ["SPARQL_UPDATE_ENDPOINT"] = "http://localhost:27149/blazegraph/namespace/kb/sparql"