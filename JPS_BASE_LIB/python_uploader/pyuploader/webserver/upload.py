import pyuploader.common.utils as utils
import pathlib
import logging
import requests

def upload_to_web_server(fileOrDir, file_ext, namespace,
                         dry_run, **kwargs):

    files = utils.get_files_by_extensions(fileOrDir,file_ext)

    logging.info(f"FTP UPLOAD")
    logging.info(f"---------------------------------------------------------------------------")
    if files:
        logging.info(f"Uploading files to the web server")

        for f in files:
            extf = ''.join(pathlib.Path(f).suffixes)
            basenf = pathlib.Path(f).name
            logging.info(f"Uploading file: {basenf} to the server https://{namespace}")
    else:
        logging.info('No files to upload')
    logging.info(f"---------------------------------------------------------------------------")


def upload_file_to_web_server(filePath, auth, upload_URL, remote_subdir={}):
    server_file_location = None
    with open(filePath,'rb') as file_obj:
        response = requests.post(upload_URL, auth=auth, headers=remote_subdir, files={'file':file_obj})
    if (response.status_code == requests.status_codes.codes.OK):
        server_file_location = response.headers['file']
    return server_file_location, response.status_code