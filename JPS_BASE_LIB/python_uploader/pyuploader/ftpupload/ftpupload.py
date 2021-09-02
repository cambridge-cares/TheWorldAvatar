import pyuploader.common.utils as utils
import pathlib
import re
import logging

UPLOAD_KEYWORD= '_UPLOAD_'
START_IND_KEYWORD= '_START_INDEX_'
UPLOAD_RE = re.compile(r'.*'+UPLOAD_KEYWORD+'(\d+)')
START_IND_RE = re.compile(r'.*'+START_IND_KEYWORD+'(\d+)')

def upload_to_ftp_server(fileOrDir, ftp_file_ext, namespace,
                         dry_run, **kwargs):

    files = utils.get_files_by_extensions(fileOrDir,ftp_file_ext)

    logging.info(f"FTP UPLOAD")
    logging.info(f"---------------------------------------------------------------------------")
    if files:
        logging.info(f"Uploading files to the ftp server")
        for f in files:
            hashf = utils.hash_file(f)
            extf = ''.join(pathlib.Path(f).suffixes)
            basenf = pathlib.Path(f).name
            upload_num_match = re.match(UPLOAD_RE, basenf)
            start_ind_match = re.match(START_IND_RE, basenf)
            if start_ind_match:
                start_ind = int(start_ind_match.groups()[0])
            else:
                start_ind = 0
            if upload_num_match:
                upload_num = int(upload_num_match.groups()[0])
                for i in range(upload_num):
                    file_ind = i + start_ind
                    logging.info(f"Uploading file: {basenf} to ftp://{namespace}/{hashf}_{file_ind}{extf}")
            else:
                logging.info(f"Uploading file: {basenf} to ftp://{namespace}/{hashf}{extf}")
    else:
        logging.info('No files to upload')
    logging.info(f"---------------------------------------------------------------------------")