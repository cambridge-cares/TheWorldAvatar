import pyuploader.common.utils as utils
import pathlib
import logging
import os
import json

FTP_MAP_FILE_NAME= 'ftpmap'

def upload_to_ftp_server(fileOrDir, ftp_file_ext, namespace,
                         dry_run, ftp_mapfile_dir, **kwargs):

    files = utils.get_files_by_extensions(fileOrDir,ftp_file_ext)
    ftp_map_file = os.path.join(ftp_mapfile_dir,FTP_MAP_FILE_NAME)
    ftpmap = {}

    logging.info(f"FTP UPLOAD")
    logging.info(f"---------------------------------------------------------------------------")
    if files:
        logging.info(f"Uploading files to the ftp server")
        if os.path.isfile(ftp_map_file):
            logging.info(f"Found the {FTP_MAP_FILE_NAME} file.")
            with open(ftp_map_file, 'r') as ftpf:
                ftpmap = json.load(ftpf)
        for f in files:
            extf = ''.join(pathlib.Path(f).suffixes)
            basenf = pathlib.Path(f).name
            if basenf in ftpmap:
                for ftp_file_name in ftpmap[basenf]:
                    logging.info(f"Uploading file: {basenf} to ftp://{namespace}/{ftp_file_name}")
            else:
                ftp_file_name = utils.hash_file(f)
                logging.info(f"Uploading file: {basenf} to ftp://{namespace}/{ftp_file_name}{extf}")
    else:
        logging.info('No files to upload')
    logging.info(f"---------------------------------------------------------------------------")