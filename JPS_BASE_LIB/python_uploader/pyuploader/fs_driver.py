import docopt
import pyuploader.app as app

__doc__ = """pyuploader
Usage:
    pyuploader fs_upload <file_or_dir>  (--file-server-filext=<ext>)
                                        [--file-server-subdirs=<dir>]
                                        [--log-file-name=<name>]
                                        [--log-file-dir=<dir>]
                                        [--no-file-logging]
                                        [--dry-run]
    pyuploader fs_download <file_url>   [--destination_path=<dest>]
                                        [--log-file-name=<name>]
                                        [--log-file-dir=<dir>]
                                        [--no-file-logging]
                                        [--dry-run]

Options:
--file-server-filext=<ext>   List of extensions used to select files
                             that will be uploaded to the file server.
                             Example: --file-server-filext='.log,.txt'
--file-server-subdirs=<dir>  Optional subdirectories to be created on
                             the file server to upload your files into.
                             Example: --file-server-subdirs='dir1/dir2/'
--log-file-name=<name>       Name of the generated log file.               [default: fs_upload.log]
--log-file-dir=<dir>         Path to the log file storing information of
                             what has been uploaded and where. Defaults
                             to the <fileOrDir> directory.
--destination_path=<dest>    Destination path to save the downloaded file
                             from a server. If not provided, the path will
                             be saved in the location the pyuploader call
                             was made as a 'web_file'.
--no-file-logging            No logging to a file flag.
--dry-run                    Run the triple store uploader tool in a dry
                             run without uploading any triples.
"""
def start():
    try:
        args = docopt.docopt(__doc__)
    except docopt.DocoptExit:
        raise docopt.DocoptExit('Error: fs_upload called with wrong arguments.')

    if args['fs_upload']:
        app.fs_upload_wrapper(
            file_or_dir = args['<file_or_dir>'],
            file_server_filext = args['--file-server-filext'],
            file_server_subdirs = args['--file-server-subdirs'],
            log_file_dir = args['--log-file-dir'],
            log_file_name = args['--log-file-name'],
            no_file_logging = args['--no-file-logging'],
            dry_run = args['--dry-run']
        )
    else:
        app.fs_download_wrapper(
            file_url = args['<file_url>'],
            destination_path = args['--destination_path'],
            log_file_dir = args['--log-file-dir'],
            log_file_name = args['--log-file-name'],
            no_file_logging = args['--no-file-logging'],
            dry_run = args['--dry-run']
        )
if __name__ == '__main__':
    start()