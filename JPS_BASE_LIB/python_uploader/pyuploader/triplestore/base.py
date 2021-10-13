import os
import logging
import pyuploader.errorhandling.appexceptions as appexcept

logger = logging.getLogger(__name__)

def get_striple_store_url():
    logger.info("Reading triple store endpoint from the environment variables.")
    try:
        tstore_url = os.environ['TRIPLE_STORE_URL']
    except KeyError:
        raise appexcept.EnvironmentVarError("Error: Triple store endpoint not found in the environment variables.")
    return tstore_url

def get_user_credentials():
    logger.info("Reading user triple store credentials from the environment variables.")
    try:
        user_login = os.environ['TRIPLE_STORE_USER']
        user_passwd = os.environ['TRIPLE_STORE_PASSWD']
    except KeyError:
        raise appexcept.EnvironmentVarError("Error: User triple store credentials not found in the environment variables.")

    return (user_login, user_passwd)

