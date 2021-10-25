import pyuploader.triplestore.upload as tripleupload
import pyuploader.webserver.upload as fileupload
import pyuploader.common.logconfig as logconfig
import logging
from typing import Union

logger = logging.getLogger(__name__)

def ts_upload_wrapper(
        file_or_dir: str,
        url: Union[str, None]=None,
        auth_str: Union[str, None]=None,
        no_auth: bool= False,
        file_ext: str='owl',
        log_file_dir: Union[str, None]= None,
        log_file_name: str= 'ts_upload.log',
        no_file_logging: bool = False,
        dry_run: bool=False) -> None:

    logconfig.config_logging(
        log_file_dir,
        log_file_name,
        no_file_logging)

    if dry_run:
        logger.info(f"#######################")
        logger.info(f"## THIS IS A DRY-RUN ##")
        logger.info(f"#######################")
        logger.info(f"")

    try:
        tripleupload.upload_to_triple_store(
                file_or_dir,
                url,
                auth_str,
                no_auth,
                file_ext,
                dry_run)
    except Exception as e:
        logger.error("Triples upload failed. Please check the log for a more detailed error description.")
        logger.exception(e)

def fs_upload_wrapper(
        file_or_dir: str,
        url: Union[str, None]=None,
        auth_str: Union[str, None]=None,
        no_auth: bool= False,
        file_ext: str='log',
        subdirs: Union[str, None]=None,
        log_file_dir:  Union[str, None]= None,
        log_file_name: str='ts_upload.log',
        no_file_logging: bool= False,
        dry_run: bool= False) -> None:

    logconfig.config_logging(
        log_file_dir,
        log_file_name,
        no_file_logging)

    if dry_run:
        logger.info(f"#######################")
        logger.info(f"## THIS IS A DRY-RUN ##")
        logger.info(f"#######################")
        logger.info(f"")

    try:
        _ = fileupload.upload_to_web_server(
            file_or_dir,
            url,
            auth_str,
            no_auth,
            file_ext,
            subdirs,
            dry_run)
    except Exception as e:
        logger.error("File upload failed. Please check the log for a more detailed error description.")
        logger.exception(e)