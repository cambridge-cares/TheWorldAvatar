import pyuploader.webserver.base as webservbase
import pyuploader.common.utils as utils
import pathlib
import logging
import requests

logger = logging.getLogger(__name__)

def upload_to_web_server(
        file_or_dir,
        file_server_filext,
        server_subdirs,
        dry_run):

    server_url= webservbase.get_server_url()
    server_upload_route = webservbase.get_server_upload_route()
    server_download_route = webservbase.get_server_download_route()
    auth = webservbase.get_user_credentials()
    files = utils.get_files_by_extensions(file_or_dir,file_server_filext)

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
                location = upload_file_to_web_server(f,auth,server_url,server_upload_route,
                                                        server_download_route, server_subdirs)
                logger.info(f"File: {basenf} successfully uploaded to: {location}.")
                server_file_locations[f] = location
    else:
        logger.info('No files to upload')
    logger.info(f"---------------------------------------------------------------------------")
    return server_file_locations

def upload_file_to_web_server(
        file_path,
        auth,
        server_url,
        server_upload_route='',
        server_download_route='',
        remote_subdir=''):

    file_url_out = None
    headers = {}
    if remote_subdir:
        if not remote_subdir.endswith('/'): remote_subdir = f"{remote_subdir}/"
        headers = {'subDir': remote_subdir}

    if server_download_route and not server_download_route.endswith('/'):
        server_download_route = f"{server_download_route}/"

    with open(file_path,'rb') as file_obj:
        response = requests.post(url= f"{server_url}{server_upload_route}",
                                auth= auth,
                                headers= headers,
                                files= {'file': file_obj})
        response.raise_for_status()

        server_file_location = response.headers['file']
    file_url_out = f"{server_url}{server_download_route}{remote_subdir}{server_file_location}"
    return file_url_out