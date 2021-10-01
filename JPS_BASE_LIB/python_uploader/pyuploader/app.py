import pyuploader.triplestore.upload as tripleupload
import pyuploader.webserver.upload as fileupload
import pyuploader.common.logconfig as logconfig
import pyuploader.webserver.base as webservbase
import pyuploader.triplestore.base as tsbase
import logging

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
    logger = logging.getLogger(__name__)

    if dry_run:
        logger.info(f"#######################")
        logger.info(f"## THIS IS A DRY-RUN ##")
        logger.info(f"#######################")
        logger.info(f"")

    tstore_url = tsbase.get_striple_store_url()
    tripleupload.upload_to_triple_store(
            file_or_dir,
            tstore_url,
            tstore_nmsp,
            tstore_filext,
            dry_run)

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
    logger = logging.getLogger(__name__)

    if dry_run:
        logger.info(f"#######################")
        logger.info(f"## THIS IS A DRY-RUN ##")
        logger.info(f"#######################")
        logger.info(f"")

    upload_URL= webservbase.get_server_upload_url()
    auth = webservbase.get_user_credentials()

    fileupload.upload_to_web_server(
        file_or_dir,
        upload_URL,
        auth,
        file_server_filext,
        file_server_subdirs,
        dry_run)