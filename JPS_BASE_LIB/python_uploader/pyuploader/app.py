import pyuploader.kgupload.kgupload as kgupload
import pyuploader.ftpupload.ftpupload as ftpupload
import pyuploader.common.logconfig as logconfig
import re
import json
import logging
import os

def oupload_wrapper(args):
    fileOrDir = args.pop('<fileOrDir>')
    config_file = args.pop('--config-file', None)
    log_file_dir = args.pop('--log_file_dir', os.getcwd())
    if config_file is not None:
        args = _read_config_file(config_file)
    args = _preprocessArgs(args)

    log_file = os.path.join(log_file_dir, args['log_file_name'])
    logconfig.config_logging(log_file)

    if args['dry_run']:
        logging.info(f"#######################")
        logging.info(f"## THIS IS A DRY-RUN ##")
        logging.info(f"#######################")
        logging.info(f"")
    kgupload.upload_to_triple_store(fileOrDir, **args)
    ftpupload.upload_to_ftp_server(fileOrDir, **args)

def _preprocessArgs(args):
    #args = _removeNoneArgs(args)
    args =_removeHypens(args)
    return args

def _read_config_file(config_file):
    with open(config_file, 'r') as fp:
        return json.load(fp)

def _removeNoneArgs(args):
    filtered = {k: v for k, v in args.items() if v is not None}
    args.clear()
    args.update(filtered)
    return args

def _removeHypens(args):
    argsWithNoHyphens = {}
    for key, value in args.items():
        new_key = re.sub('^-+', '', key)
        new_key = re.sub('-', '_', new_key)
        argsWithNoHyphens[new_key] = value
    return argsWithNoHyphens