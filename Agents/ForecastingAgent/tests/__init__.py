import os

#TODO: Implement properly, ideally as dockerised version
# Set environment variables for standalone testing (non-dockerised deployment)
os.environ["STACK_NAME"] = ""
os.environ["DB_URL"] = "jdbc:postgresql://localhost:9432/districtheating"
os.environ["DB_USER"] = "postgres"
os.environ["DB_PASSWORD"] = "postgres"
os.environ["QUERY_ENDPOINT"] = "http://localhost:9999/blazegraph/namespace/districtheating/sparql"
os.environ["UPDATE_ENDPOINT"] = "http://localhost:9999/blazegraph/namespace/districtheating/sparql"