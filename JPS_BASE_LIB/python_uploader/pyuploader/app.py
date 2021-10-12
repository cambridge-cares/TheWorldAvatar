import pyuploader.triplestore.upload as tripleupload
import pyuploader.webserver.upload as fileupload
import pyuploader.common.logconfig as logconfig
import logging

logger = logging.getLogger(__name__)

def ts_upload_wrapper(
        file_or_dir,
        url='',
        auth='',
        file_ext='.owl',
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
                url,
                auth,
                file_ext,
                dry_run)
    except Exception as e:
        logger.error("Triples upload failed. Please check the log for a more detailed error description.")
        logger.exception(e)

def fs_upload_wrapper(
        file_or_dir,
        url='',
        auth='',
        file_ext='.log',
        subdirs='',
        log_file_dir= None,
        log_file_name = 'ts_upload.log',
        no_file_logging = False,
        dry_run= False):

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
            auth,
            file_ext,
            subdirs,
            dry_run)
    except Exception as e:
        logger.error("File upload failed. Please check the log for a more detailed error description.")
        logger.exception(e)