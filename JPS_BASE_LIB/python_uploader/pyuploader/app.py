import pyuploader.kgupload.kgupload as kgupload
import pyuploader.ftpupload.ftpupload as ftpupload
import pyuploader.common.logconfig as logconfig
import re
import json
import logging
import os

def oupload_wrapper(args):
    args =_remove_special_characters(args)
    args = _add_defaults(args)

    log_file = os.path.join(args['log_file_dir'], args['log_file_name'])
    logconfig.config_logging(log_file)

    if args['dry_run']:
        logging.info(f"#######################")
        logging.info(f"## THIS IS A DRY-RUN ##")
        logging.info(f"#######################")
        logging.info(f"")

    kgupload.upload_to_triple_store(**args)
    ftpupload.upload_to_ftp_server(**args)

def _add_defaults(args):
    if args['log_file_dir'] is None:
        args['log_file_dir'] = os.getcwd()
    if args['ftp_mapfile_dir'] is None:
        fileOrDir = args['fileOrDir']
        if os.path.isfile(fileOrDir):
            args['ftp_mapfile_dir'] = os.path.dirname(fileOrDir)
        else:
            args['ftp_mapfile_dir'] = fileOrDir
    return args

def _remove_special_characters(args):
    argsWithNoHyphens = {}
    for key, value in args.items():
        new_key = re.sub('^-+', '', key)
        new_key = re.sub('-', '_', new_key)
        new_key = re.sub('<|>', '', new_key)
        argsWithNoHyphens[new_key] = value
    return argsWithNoHyphens