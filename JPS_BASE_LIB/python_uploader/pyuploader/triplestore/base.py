import os
import logging
import pyuploader.errorhandling.appexceptions as appexcept
import pyuploader.common.utils as utils
from typing import Tuple

logger = logging.getLogger(__name__)

def get_tstore_url() -> str:
    logger.info("Reading triple store specs file path from the environment variables.")
    try:
        specs_file = os.environ['TRIPLE_STORE_SPECS']
    except KeyError:
        raise appexcept.EnvironmentVarError("Error: Triple store specs file path not found in the environment variables.")

    logger.info("Reading triple store url from the specs file.")
    try:
        tstore_url = utils.read_file_content(specs_file).strip()
    except FileNotFoundError:
        raise FileNotFoundError("Error: Triple store specs file does not exists.")

    logger.info(f"Read triple store url: {tstore_url}")
    return tstore_url

def get_tstore_credentials_from_envar() -> str:
    logger.info("Reading triple store secrets file path from the environment variables.")
    try:
        secrets_file = os.environ['TRIPLE_STORE_SECRETS']
    except KeyError:
        raise appexcept.EnvironmentVarError("Error: Triple store secrets file path not found in the environment variables.")

    logger.info("Reading triple store auth from the secrets file.")
    try:
        auth_str = utils.read_file_content(secrets_file).strip()
    except FileNotFoundError:
        raise FileNotFoundError("Error: Triple store secrets file does not exists.")

    return auth_str

