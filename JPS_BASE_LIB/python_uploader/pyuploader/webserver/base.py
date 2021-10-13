import os
import logging
import pyuploader.errorhandling.appexceptions as appexcept

logger = logging.getLogger(__name__)

def get_user_credentials():
    logger.info("Reading user file server credentials from the environment variables.")
    try:
        user_login = os.environ['KG_FILE_SERVER_USER']
        user_passwd = os.environ['KG_FILE_SERVER_PASSWD']
    except KeyError:
        raise appexcept.EnvironmentVarError("Error: User file server credentials not found in the environment variables.")

    return (user_login, user_passwd)

def get_server_url():
    logger.info("Reading file server url from the user environment variables.")
    try:
        server_url = os.environ['KG_FILE_SERVER_URL']
    except KeyError:
        raise appexcept.EnvironmentVarError("Error: File server url not found in the environment variables.")
    return server_url