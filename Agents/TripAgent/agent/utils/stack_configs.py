from twa import agentlogging
from agent.utils.stack_gateway import stack_clients_view
from agent.utils.env_configs import NAMESPACE, DATABASE

# Initialise logger instance (ensure consistent logger level`)
logger = agentlogging.get_logger('dev')


class StackException(Exception):
    """Raise in case of exception when using the stack clients."""


def retrieve_stack_settings():
    """
    Note: do not obtain an object from stack_clients_view and pass it to the baselib_view
    e.g. getting a RemoteStoreClient via BlazegraphClient and pass it to TimeSeriesClient
    """
    # Define global scope for global variables
    global BLAZEGRAPH_URL, ONTOP_URL, RDB_URL, RDB_USER, RDB_PASSWORD
    try:
        # Retrieve endpoint configurations from Stack clients
        ONTOP_URL = stack_clients_view.OntopClient.getInstance(
            "ontop").readEndpointConfig().getUrl()
        RDB_URL = stack_clients_view.PostGISClient.getInstance(
        ).readEndpointConfig().getJdbcURL(DATABASE)
        RDB_USER = stack_clients_view.PostGISClient.getInstance(
        ).readEndpointConfig().getUsername()
        RDB_PASSWORD = stack_clients_view.PostGISClient.getInstance(
        ).readEndpointConfig().getPassword()

        BLAZEGRAPH_URL = stack_clients_view.BlazegraphClient.getInstance(
        ).readEndpointConfig().getUrl(NAMESPACE)

    except Exception as e:
        err_msg = "General Stack client parameter extraction error: {}".format(
            str(e))
        logger.error(err_msg)
        raise StackException(err_msg)


# Run when module is imported
retrieve_stack_settings()
