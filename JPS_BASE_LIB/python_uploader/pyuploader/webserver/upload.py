import pyuploader.common.utils as utils
import pathlib
import logging
import requests

logger = logging.getLogger(__name__)

def upload_to_web_server(
        file_or_dir,
        upload_URL,
        auth,
        file_server_filext,
        server_subdirs,
        dry_run):

    files = utils.get_files_by_extensions(file_or_dir,file_server_filext)

    logger.info(f"FILE SERVER UPLOAD")
    logger.info(f"---------------------------------------------------------------------------")
    if files:
        logger.info(f"Uploading files to the file server.")

        for f in files:
            extf = ''.join(pathlib.Path(f).suffixes)
            basenf = pathlib.Path(f).name
            logger.info(f"Uploading file: {basenf} to the file server.")
            if not dry_run:
                server_file_location, status = upload_file_to_web_server(f,auth,upload_URL,server_subdirs)
    else:
        logger.info('No files to upload')
    logger.info(f"---------------------------------------------------------------------------")

def upload_file_to_web_server(file_path, auth, upload_URL, remote_subdir={}):
    server_file_location = None
    with open(file_path,'rb') as file_obj:
        response = requests.post(upload_URL, auth=auth, headers=remote_subdir, files={'file':file_obj})
    if (response.status_code == requests.status_codes.codes.OK):
        server_file_location = response.headers['file']   
    return server_file_location, response.status_code