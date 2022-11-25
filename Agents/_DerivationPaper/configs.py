# Endpoints and properties for KG and RDB interaction
# (need to match values specified in docker-compose file)
DB_USER = 'postgres'
DB_PASSWORD = 'postgres'
KG_NAMESPACE = 'mvp'

# Use default 'postgres' namespace for RDB
DB_URL = 'jdbc:postgresql://localhost:9997/postgres'
SPARQL_QUERY_ENDPOINT = f'http://localhost:9998/blazegraph/namespace/{KG_NAMESPACE}/sparql'
SPARQL_UPDATE_ENDPOINT = f'http://localhost:9998/blazegraph/namespace/{KG_NAMESPACE}/sparql'