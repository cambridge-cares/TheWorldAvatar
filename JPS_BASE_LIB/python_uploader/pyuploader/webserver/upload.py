import pyuploader.webserver.base as webservbase
import pyuploader.common.utils as utils
import pathlib
import logging
import requests
from typing import Union, Dict, Tuple

logger = logging.getLogger(__name__)

def upload_to_web_server(
        file_or_dir: str,
        url: Union[str, None]=None,
        auth_str: Union[str, None]=None,
        no_auth: bool = False,
        file_ext: str='log',
        subdirs: Union[str, None]=None,
        dry_run: bool= False) -> Dict[str,str]:

    if url is None: url= webservbase.get_fserver_url_from_envar()
    if not no_auth:
        if auth_str is None: auth_str = webservbase.get_fserver_credentials_from_envar()
        auth = utils.get_credentials_from_str(auth_str)
    else:
        # the file server will simply ignore the auth if it doesnt require it
        auth = ('','')

    files = utils.get_files_by_extensions(file_or_dir,file_ext)

    server_file_locations: Dict[str,str] = {}
    logger.info(f"---------------------------------------------------------------------------")
    logger.info(f"FILE SERVER UPLOAD")
    logger.info(f"---------------------------------------------------------------------------")
    if files:
        logger.info(f"Uploading files to the file server...")
        for f in files:
            basenf = pathlib.Path(f).name
            logger.info(f"Uploading file: {basenf} to the file server.")
            if not dry_run:
                location = upload_file_to_web_server(f, url, auth, subdirs)
                logger.info(f"File: {basenf} successfully uploaded to: {location}.")
                server_file_locations[f] = location
        logger.info(f"Uploading files to the file server finished.")
    else:
        logger.info('No files to upload')
    logger.info(f"---------------------------------------------------------------------------")
    return server_file_locations

def upload_file_to_web_server(
        file_path: str,
        url: str,
        auth: Tuple[str,str],
        subdirs: Union[str, None]=None) -> str:

    headers = {}
    if subdirs is not None:
        if not subdirs.endswith('/'): subdirs = f"{subdirs}/"
        headers = {'subDir': subdirs}

    with open(file_path,'rb') as file_obj:
        response = requests.post(url= url,
                                auth= auth,
                                headers= headers,
                                files= {'file': file_obj})
        response.raise_for_status()
    return response.headers['file']