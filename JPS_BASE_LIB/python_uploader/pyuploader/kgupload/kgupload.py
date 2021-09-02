import pyuploader.common.utils as utils
import logging
import pathlib

def upload_to_triple_store(fileOrDir, tstore_url, namespace,
                           tstore_file_ext, dry_run, **kwargs):

    files = utils.get_files_by_extensions(fileOrDir,tstore_file_ext)

    logging.info(f"TRIPLE STORE UPLOAD")
    logging.info(f"---------------------------------------------------------------------------")
    if files:
        logging.info(f"Uploading files to the triple store: {tstore_url} at namespace: {namespace}")
        for f in files:
            basenf = pathlib.Path(f).name

            logging.info(f"Uploading file: {basenf}")
    else:
        logging.info('No files to upload')
    logging.info(f"---------------------------------------------------------------------------")