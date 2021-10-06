import os
import logging
import pyuploader.errorhandling.appexceptions as appexcept

logger = logging.getLogger(__name__)

def get_user_credentials():
    logger.info("Reading user credentials.")
    try:
        user_login = os.environ['KG_FILE_SERVER_USER']
        user_passwd = os.environ['KG_FILE_SERVER_PASSWD']
    except KeyError:
        raise appexcept.EnvironmentVarError("Error: User file server credentials not found in environment variables.")

    return (user_login, user_passwd)

def get_server_url():
    logger.info("Reading file server details.")
    try:
        server_url = os.environ['KG_FILE_SERVER_URL']
    except KeyError:
        raise appexcept.EnvironmentVarError("Error: File server upload url not found in environment variables.")
    return server_url

def get_server_upload_route():
    logger.info("Reading the file server upload route details.")
    try:
        server_upload_route = os.environ['KG_FILE_SERVER_UPLOAD_ROUTE']
    except KeyError:
        server_upload_route = 'upload'
        logger.info(f"File server upload route not found in environment variables. Using the default '{server_upload_route}' route.")
    return server_upload_route

def get_server_download_route():
    logger.info("Reading the file server downnload route details.")
    try:
        server_download_route = os.environ['KG_FILE_SERVER_DOWNLOAD_ROUTE']
    except KeyError:
        server_download_route = 'download/'
        logger.info(f"File server download route not found in environment variables. Using the default '{server_download_route}' route.")
    return server_download_route