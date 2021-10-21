import os
import logging
import pyuploader.errorhandling.appexceptions as appexcept
from typing import Tuple

logger = logging.getLogger(__name__)

def get_tstore_url() -> str:
    logger.info("Reading triple store endpoint from the environment variables.")
    try:
        tstore_url = os.environ['TRIPLE_STORE_URL']
    except KeyError:
        raise appexcept.EnvironmentVarError("Error: Triple store endpoint not found in the environment variables.")
    return tstore_url

def get_tstore_credentials_from_envar() -> Tuple[str,str]:
    logger.info("Reading user triple store credentials from the environment variables.")
    try:
        username = os.environ['TRIPLE_STORE_USER']
        password = os.environ['TRIPLE_STORE_PASSWD']
    except KeyError:
        raise appexcept.EnvironmentVarError("Error: User triple store credentials not found in the environment variables.")

    return (username, password)

