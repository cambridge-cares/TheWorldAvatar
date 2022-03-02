import logging

logger = logging.getLogger(__name__)

FILE_SERVER = "file server"
TRIPLE_STORE = "triple store"

UPLOAD_SETTINGS_KEY = "upload_settings"
QUERY_SETTINGS_KEY = "query_settings"
WRITERS_PREFIXES_KEY = "prefixes"
HANDLER_KWARGS = "handler_kwargs"

TRIPLE_STORE_SPARQL_ENDPOINT_KEY = "triple_store_sparql_endpoint"
TRIPLE_STORE_SECRETS_FILE_KEY = "triple_store_secrets_file"
TRIPLE_STORE_NO_AUTH_KEY = "triple_store_no_auth"
FILE_SERVER_UPLOAD_ENDPOINT_KEY = "file_server_upload_endpoint"
FILE_SERVER_SECRETS_FILE_KEY = "file_server_secrets_file"
FILE_SERVER_SUBDIR_KEY = "file_server_subdir"
FILE_SERVER_NO_AUTH_KEY = "file_server_no_auth"
OSPECIES_QUERY_ENDPOINT_KEY = "ospecies_query_endpoint"
OMOPS_QUERY_ENDPOINT_KEY = "omops_query_endpoint"
OPSSCAN_QUERY_ENDPOINT_KEY = "opsscan_query_endpoint"
OCOMPCHEM_QUERY_ENDPOINT_KEY = "ocompchem_query_endpoint"
UPLOAD_TO_FILE_SERVER_KEY = "upload_to_file_server"
UPLOAD_TO_TRIPLE_STORE_KEY = "upload_to_triple_store"

DEFAULT_CONFIG_KEYS = [
    TRIPLE_STORE_SPARQL_ENDPOINT_KEY,
    TRIPLE_STORE_SECRETS_FILE_KEY,
    TRIPLE_STORE_NO_AUTH_KEY,
    FILE_SERVER_UPLOAD_ENDPOINT_KEY,
    FILE_SERVER_SECRETS_FILE_KEY,
    FILE_SERVER_SUBDIR_KEY,
    FILE_SERVER_NO_AUTH_KEY,
    OCOMPCHEM_QUERY_ENDPOINT_KEY,
    OSPECIES_QUERY_ENDPOINT_KEY,
    OMOPS_QUERY_ENDPOINT_KEY,
    OPSSCAN_QUERY_ENDPOINT_KEY,
    UPLOAD_TO_FILE_SERVER_KEY,
    UPLOAD_TO_TRIPLE_STORE_KEY,
]
HANDLERS_CONFIG_KEY = "handlers"
