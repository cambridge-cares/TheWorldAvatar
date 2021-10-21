import os
import logging
import pyuploader.errorhandling.appexceptions as appexcept
import pyuploader.common.utils as utils

logger = logging.getLogger(__name__)

def get_fserver_credentials_from_envar() -> str:
    logger.info("Reading file server secrets file path from the environment variables.")
    try:
        secrets_file = os.environ['KG_FILE_SERVER_SECRETS']
    except KeyError:
        raise appexcept.EnvironmentVarError("Error: File server secrets file path not found in environment variables.")

    logger.info("Reading file server auth from the secrets file.")
    try:
        auth_str = utils.read_file_content(secrets_file).strip()
    except FileNotFoundError:
        raise FileNotFoundError("Error: File server secrets file does not exists.")

    return auth_str

def get_fserver_url_from_envar() -> str:
    logger.info("Reading file server specs file path from the user environment variables.")
    try:
        specs_file = os.environ['KG_FILE_SERVER_SPECS']
    except KeyError:
        raise appexcept.EnvironmentVarError("Error: File server specs file path not found in the environment variables.")

    logger.info("Reading file server url from the specs file.")
    try:
        server_url = utils.read_file_content(specs_file).strip()
    except FileNotFoundError:
        raise FileNotFoundError("Error: File server specs file does not exists.")

    logger.info(f"Read file server url: {server_url}")
    return server_url