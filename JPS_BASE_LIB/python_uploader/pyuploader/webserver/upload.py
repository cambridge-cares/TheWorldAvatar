import pyuploader.webserver.base as webservbase
import pyuploader.common.utils as utils
import pathlib
import logging
import requests

logger = logging.getLogger(__name__)

def upload_to_web_server(
        file_or_dir,
        url='',
        auth='',
        file_ext='.log',
        subdirs= '',
        dry_run= False):

    if not url: url= webservbase.get_server_url()
    if not auth: auth = webservbase.get_user_credentials()
    files = utils.get_files_by_extensions(file_or_dir,file_ext)

    server_file_locations = {}
    logger.info(f"---------------------------------------------------------------------------")
    logger.info(f"FILE SERVER UPLOAD")
    logger.info(f"---------------------------------------------------------------------------")
    if files:
        logger.info(f"Uploading files to the file server.")

        for f in files:
            basenf = pathlib.Path(f).name
            logger.info(f"Uploading file: {basenf} to the file server.")
            if not dry_run:
                location = upload_file_to_web_server(f,url,auth,subdirs)
                logger.info(f"File: {basenf} successfully uploaded to: {location}.")
                server_file_locations[f] = location
    else:
        logger.info('No files to upload')
    logger.info(f"---------------------------------------------------------------------------")
    return server_file_locations

def upload_file_to_web_server(
        file_path,
        url,
        auth,
        subdirs=''):

    headers = {}
    if subdirs:
        if not subdirs.endswith('/'): subdirs = f"{subdirs}/"
        headers = {'subDir': subdirs}

    with open(file_path,'rb') as file_obj:
        response = requests.post(url= url,
                                auth= auth,
                                headers= headers,
                                files= {'file': file_obj})
        response.raise_for_status()
    return response.headers['file']