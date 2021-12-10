import docopt
import pyuploader.app as app

__doc__: str = """pyuploader
Usage:
    fs_upload <file_or_dir>  [--url=<url>]
                             [--auth-file=<file>]
                             [--no-auth]
                             [--file-ext=<ext>]
                             [--subdirs=<dir>]
                             [--log-file-name=<name>]
                             [--log-file-dir=<dir>]
                             [--no-file-logging]
                             [--dry-run]

Options:
--url=<url>             File server upload url. If not specified, the code
                        will try to read it from a file whose location
                        should be specified in user 'KG_FILE_SERVER_SPECS'
                        environment variable.
--auth-file=<file>      File path to the file server secrets file containing
                        the user authorization string of the following form:
                        "username:password". If not specified, the code will
                        try to read the secrets file path from a user
                        'KG_FILE_SERVER_SECRETS' environment variable.
                        DO NOT store your secrets directly in environment
                        variables, only store the secrets file path.
--no-auth               Disables reading credentials from the environment
                        variables and sending it to the file server.
--file-ext=<ext>        List of extensions used to select files
                        that will be uploaded to the file server.
                        Example: --file-ext='log,txt'                       [default: log]
--subdirs=<dir>         Optional subdirectories to be created on
                        the file server to upload your files into.
                        Example: --subdirs='dir1/dir2/'                     [default: ]
--log-file-name=<name>  Name of the generated log file.                     [default: fs_uploader.log]
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
        raise docopt.DocoptExit('Error: fs_upload called with wrong arguments.') #type: ignore

    app.app_upload(
        uploader_type='fs_uploader',
        file_or_dir = args['<file_or_dir>'],
        url = args['--url'],
        auth_file = args['--auth-file'],
        no_auth = args['--no-auth'],
        file_ext = args['--file-ext'],
        subdirs = args['--subdirs'],
        log_file_dir = args['--log-file-dir'],
        log_file_name = args['--log-file-name'],
        no_file_logging = args['--no-file-logging'],
        dry_run = args['--dry-run']
    )

if __name__ == '__main__':
    start()