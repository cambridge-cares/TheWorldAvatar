from .env_configs_mock import DATABASE, NAMESPACE

# TODO: delete this file
# This is a workaround to allow for deployment outside of stack for derivation mvp
DB_USER = 'postgres'
DB_PASSWORD = 'postgres'
DB_URL = 'jdbc:postgresql://host.docker.internal:9997/postgres'
QUERY_ENDPOINT = f'http://host.docker.internal:9998/blazegraph/namespace/{NAMESPACE}/sparql'
UPDATE_ENDPOINT = f'http://host.docker.internal:9998/blazegraph/namespace/{NAMESPACE}/sparql'
