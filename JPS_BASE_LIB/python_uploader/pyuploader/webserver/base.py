import os
import logging
import pyuploader.errorhandling.appexceptions as appexcept

logger = logging.getLogger(__name__)

def get_user_credentials():
    logger.info("Reading user credentials.")
    try:
        user_login = os.environ['CMCL_FILE_SERVER_USER']
        user_passwd = os.environ['CMCL_FILE_SERVER_PASSWD']
    except KeyError:
        raise appexcept.EnvironmentVarError("Error: User file server credentials not found in environment variables.")

    return (user_login, user_passwd)

def get_server_upload_url():
    logger.info("Reading file server details.")
    try:
        server_url = os.environ['CMCL_FILE_SERVER_URL']
        server_upload = os.environ['CMCL_FILE_SERVER_UPLOAD']
    except KeyError:
        raise appexcept.EnvironmentVarError("Error: File server upload url not found in environment variables.")
    return server_url+server_upload