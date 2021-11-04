import docopt
import pyuploader.app as app

__doc__: str = """pyuploader
Usage:
    ts_upload <file_or_dir>  [--url=<url>]
                             [--auth_file=<file>]
                             [--no-auth]
                             [--file-ext=<ext>]
                             [--log-file-name=<name>]
                             [--log-file-dir=<dir>]
                             [--no-file-logging]
                             [--dry-run]

Options:
--url=<url>             Triple store upload endpoint. If not specified,
                        the code will try to read it from a file whose
                        location should be specified in user environment
                        variables.
--auth_file=<file>      File path to the triple store secrets file containing
                        the user authorization string of the following form:
                        "username:password". If not specified, the code will
                        try to read the secrets file path from a user
                        'TRIPLE_STORE_SECRETS' environment variable.
                        DO NOT store your secrets directly in environment
                        variables, only store the secrets file path.
--no-auth               Disables reading credentials from the environment
                        variables and sending it to the triple store.
--file-ext=<ext>        List of extensions used to select files             [default: owl]
                        that will be uploaded to the triple store.
--log-file-name=<name>  Name of the generated log file.                     [default: ts_uploader.log]
--log-file-dir=<dir>    Path to the log file storing information of
                        what has been uploaded and where. Defaults
                        to the <fileOrDir> directory.
--no-file-logging       No logging flag to a file.
--dry-run               Run the triple store uploader tool in a dry
                        run without uploading any triples.
"""

def start() -> None:
    try:
        args = docopt.docopt(__doc__)
    except docopt.DocoptExit: #type: ignore
        raise docopt.DocoptExit('Error: ts_upload called with wrong arguments.') #type: ignore

    app.app_upload(
        uploader_type='ts_uploader',
        file_or_dir = args['<file_or_dir>'],
        url = args['--url'],
        auth_file = args['--auth_file'],
        no_auth = args['--no-auth'],
        file_ext = args['--file-ext'],
        log_file_dir = args['--log-file-dir'],
        log_file_name = args['--log-file-name'],
        no_file_logging = args['--no-file-logging'],
        dry_run = args['--dry-run']
    )

if __name__ == '__main__':
    start()