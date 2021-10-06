import pyuploader.triplestore.upload as tripleupload
import pyuploader.webserver.upload as fileupload
import pyuploader.webserver.download as filedownload
import pyuploader.common.logconfig as logconfig
import logging

logger = logging.getLogger(__name__)

def ts_upload_wrapper(
        file_or_dir,
        tstore_nmsp='base',
        tstore_filext='.owl',
        log_file_dir= None,
        log_file_name = 'ts_upload.log',
        no_file_logging = False,
        dry_run=False):

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
                tstore_nmsp,
                tstore_filext,
                dry_run)
    except Exception as e:
        logger.exception(e)
        raise e

def fs_upload_wrapper(
        file_or_dir,
        file_server_filext,
        file_server_subdirs='',
        log_file_dir= None,
        log_file_name = 'ts_upload.log',
        no_file_logging = False,
        dry_run =False):

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
        file_locations = fileupload.upload_to_web_server(
            file_or_dir,
            file_server_filext,
            file_server_subdirs,
            dry_run)
    except Exception as e:
        logger.exception(e)
        raise e

def fs_download_wrapper(
            file_url,
            destination_path,
            log_file_dir= None,
            log_file_name= 'fs_download.log',
            no_file_logging=False,
            dry_run = False
        ):

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
        filedownload.download_from_web_server(
            file_url,
            destination_path,
            dry_run)
    except Exception as e:
        logger.exception(e)
        raise e