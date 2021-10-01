import pyuploader.tripleupload.tripleupload as tripleupload
import pyuploader.fileupload.fileupload as fileupload
import pyuploader.common.logconfig as logconfig
import re
import logging
import os

def ts_upload_wrapper(
        fileOrDir,
        tstore_nmsp='base',
        tstore_filext='.owl',
        log_file_name='ts_upload.log',
        log_file_dir=None,
        no_logging=False,
        dry_run=False):

    if log_file_dir is None: log_file_dir = os.getcwd()
    log_file = os.path.join(log_file_dir, log_file_name)
    logconfig.config_logging(log_file, log_file_name, no_logging)

    if dry_run:
        logging.info(f"#######################")
        logging.info(f"## THIS IS A DRY-RUN ##")
        logging.info(f"#######################")
        logging.info(f"")

    tripleupload.upload_to_triple_store(
            fileOrDir,
            tstore_url,
            namespace,
            tstore_file_ext,
            dry_run
        )

    fileupload.upload_to_web_server(
        fileOrDir,
        file_server_ext,
        namespace,
        dry_run
    )

def fs_upload_wrapper()