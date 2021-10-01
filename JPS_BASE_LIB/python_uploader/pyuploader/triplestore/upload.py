import pyuploader.common.utils as utils
import logging
import pathlib

logger = logging.getLogger(__name__)

def upload_to_triple_store(
        file_or_dir,
        tstore_url,
        tstore_nmsp,
        tstore_fileext,
        dry_run):

    files = utils.get_files_by_extensions(file_or_dir,tstore_fileext)

    logger.info(f"TRIPLE STORE UPLOAD")
    logger.info(f"---------------------------------------------------------------------------")
    if files:
        logger.info(f"Uploading files to the triple store: {tstore_url} at namespace: {tstore_nmsp}.")
        for f in files:
            basenf = pathlib.Path(f).name
            logger.info(f"Uploading file: {basenf}")
    else:
        logger.info('No files to upload')
    logger.info(f"---------------------------------------------------------------------------")