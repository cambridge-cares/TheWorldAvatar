import docopt
import pyuploader.uploaders as uploaders
import pyuploader.uploaders.uploader_factory as uploader_factory
import pyuploader.common.logconfig as logconfig
from typing import Dict
import logging
import sys

logger = logging.getLogger(__name__)


__doc__: str = """pyuploader
Usage:
    pyuploader ts_upload <file_or_dir> [options]
    pyuploader fs_upload <file_or_dir> [options]

Options:
--url=<url>             Upload endpoint. If not specified, the code
                        will try to read it from a file whose location
                        should be specified in an environment variable:
                            KG_FILE_SERVER_SPECS - for file server uploads
                            TRIPLE_STORE_SPECS - for triple store uploads
--auth-file=<file>      File path to the secrets file containing the user
                        authorization string of the following form:
                        "username:password". If not specified, the code will
                        try to read the secrets file path from an environment
                        variable:
                            KG_FILE_SERVER_SECRETS - for file server uploads
                            TRIPLE_STORE_SECRETS - for triple store uploads
                        DO NOT store your secrets directly in environment
                        variables, only store the secrets file path.
--no-auth               Disables reading credentials from the environment
                        variables and sending it to the upload endpoint.
--file-ext=<ext>        List of extensions used to select files
                        that will be uploaded to the file server.
                        Example: --file-ext='log,txt'
                        Defaults to:
                            all - for file server uploads
                            owl - for triple store uploads
--subdirs=<dir>         Optional subdirectories to be created on
                        the file server to upload your files into.
                        Example: --subdirs='dir1/dir2/'
--log-file-name=<name>  Name of the generated log file.
                        Defaults to:
                            fs_uploader.log - for file server uploads
                            ts_uploader.log - for triple store uploads
--log-file-dir=<dir>    Path to the log file storing information of
                        what has been uploaded and where. Defaults
                        to the <file_or_dir> directory.
--no-file-logging       No logging to a file flag.
--dry-run               Run the file uploader tool in a dry
                        run without uploading any files.
"""
def start() -> None:
    try:
        args = docopt.docopt(__doc__)
    except docopt.DocoptExit: #type: ignore
        print(sys.argv)
        raise docopt.DocoptExit('Error: pyuploader called with wrong arguments.') #type: ignore

    try:

        _process_args(args=args)


        logconfig.config_logging(
            log_file_dir=args['--log-file-dir'],
            log_file_name=args['--log-file-name'],
            no_file_logging=args['--no-file-logging'])


        uploader = uploader_factory.get_uploader(
            uploader_type=args["--uploader_type"],
            url=args['--url'],
            auth_file = args['--auth-file'],
            no_auth=args['--no-auth'],
            subdirs = args['--subdirs'],
            )

        uploader.upload(
            file_or_dir = args['<file_or_dir>'],
            file_ext = args['--file-ext'],
            dry_run = args['--dry-run']
        )
    except Exception as e:
        logger.error(
            (
                "Uploader failed. Please check the log "
                "for a more detailed error description."
            )
        )
        logger.exception(e)



def _process_args(args: Dict) -> None:
    if args['--log-file-name'] is None:
        args['--log-file-name'] = 'fs_uploader.log' \
                                 if args['fs_upload'] \
                                 else 'ts_uploader.log'
    if args['--file-ext'] is None:
        args['--file-ext'] = 'all' \
                              if args['fs_upload'] \
                              else 'owl'


    if args['fs_upload']:
        args['--uploader_type'] = uploaders.FS_UPLOADER
    else:
        args['--uploader_type'] = uploaders.TS_UPLOADER
        if args['--subdirs'] is not None:
            logger.warning(
                (
                    "The --subdirs option is not supported by the triple store "
                    "uploader and will be omitted."
                )
            )

if __name__ == '__main__':
    start()