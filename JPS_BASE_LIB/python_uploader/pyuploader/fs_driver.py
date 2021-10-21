import docopt
import pyuploader.app as app

__doc__: str = """pyuploader
Usage:
    fs_upload <file_or_dir>  [--url=<url>]
                             [--auth=<auth>]
                             [--file-ext=<ext>]
                             [--subdirs=<dir>]
                             [--log-file-name=<name>]
                             [--log-file-dir=<dir>]
                             [--no-file-logging]
                             [--dry-run]

Options:
--file-ext=<ext>        List of extensions used to select files
                        that will be uploaded to the file server.
                        Example: --file-ext='.log,.txt'                     [default: log]
--url=<url>             File server upload url. If not specified, the code
                        will try to read it from a file whose location
                        should be specified in user environment variables.
--auth=<auth>           File server authorization string given as a
                        "username:password". If not specified, the code
                        will try to read it from a file whose location
                        should be specified in user environment variables.
--subdirs=<dir>         Optional subdirectories to be created on
                        the file server to upload your files into.
                        Example: --subdirs='dir1/dir2/'                     [default: ]
--log-file-name=<name>  Name of the generated log file.                     [default: fs_upload.log]
--log-file-dir=<dir>    Path to the log file storing information of
                        what has been uploaded and where. Defaults
                        to the <fileOrDir> directory.
--no-file-logging       No logging to a file flag.
--dry-run               Run the triple store uploader tool in a dry
                        run without uploading any triples.
"""
def start() -> None:
    try:
        args = docopt.docopt(__doc__)
    except docopt.DocoptExit: #type: ignore
        raise docopt.DocoptExit('Error: fs_upload called with wrong arguments.') #type: ignore

    app.fs_upload_wrapper(
        file_or_dir = args['<file_or_dir>'],
        url = args['--url'],
        auth_str = args['--auth'],
        file_ext = args['--file-ext'],
        subdirs = args['--subdirs'],
        log_file_dir = args['--log-file-dir'],
        log_file_name = args['--log-file-name'],
        no_file_logging = args['--no-file-logging'],
        dry_run = args['--dry-run']
    )

if __name__ == '__main__':
    start()