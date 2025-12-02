import os
from py4jps import agentlogging
from .stack_configs import retrieve_stack_settings

logger = agentlogging.get_logger('prod')

# Module-level variables (always imported by other modules)
NAMESPACE = None
STACK_NAME = None
QUERY_ENDPOINT = None
UPDATE_ENDPOINT = None


def retrieve_default_settings():
    """
    Retrieve and validate endpoint and namespace settings.

    Requirements implemented:
    - QUERY_ENDPOINT and UPDATE_ENDPOINT must *always* be provided.
    - STACK_NAME and NAMESPACE are optional but stored if provided.
    - No stack auto-discovery is performed.
    """
    global QUERY_ENDPOINT, UPDATE_ENDPOINT, STACK_NAME, NAMESPACE

    STACK_NAME = os.getenv('STACK_NAME')
    NAMESPACE = os.getenv('NAMESPACE', '')

    # Endpoints must always be set
    QUERY_ENDPOINT = os.getenv('QUERY_ENDPOINT')
    UPDATE_ENDPOINT = os.getenv('UPDATE_ENDPOINT')

    if not QUERY_ENDPOINT or not UPDATE_ENDPOINT:
        logger.error("QUERY_ENDPOINT and UPDATE_ENDPOINT must be provided.")
        raise RuntimeError("Missing mandatory SPARQL endpoints.")

    logger.info("Retrieved default connection parameters:")
    logger.info(f"  STACK_NAME: {STACK_NAME}")
    logger.info(f"  NAMESPACE: {NAMESPACE}")
    logger.info(f"  QUERY_ENDPOINT: {QUERY_ENDPOINT}")
    logger.info(f"  UPDATE_ENDPOINT: {UPDATE_ENDPOINT}")


# Run at module import time
retrieve_default_settings()
