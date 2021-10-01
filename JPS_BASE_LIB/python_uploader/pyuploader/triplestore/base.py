import os
import logging
import pyuploader.errorhandling.appexceptions as appexcept

logger = logging.getLogger(__name__)

def get_striple_store_url():
    logger.info("Reading triple store info.")
    try:
        tstore_url = os.environ['TRIPLE_STORE_URL']
    except KeyError:
        raise appexcept.EnvironmentVarError("Error: Triple store url not found in environment variables.")
    return tstore_url


