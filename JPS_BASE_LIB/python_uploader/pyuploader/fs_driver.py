from docopt import docopt, DocoptExit
import pyuploader.app as app

doc = """pyuploader
Usage:
    fs_upload <fileOrDir>  (--file-server-filext=<ext>)
                           [--file-server-subdir=<dir>]
                           [--log-file-name=<name>]
                           [--log-file-dir=<dir>]
                           [--no-logging]
                           [--dry-run]

Options:
--file-server-filext=<ext>   List of extensions used to select files
                             that will be uploaded to the file server.
                             Example: --file-server-filext='.log,.txt'
--file-server-subdir=<dir>   Optional subdirectory to be created on
                             the file server to upload your files into.
--log-file-name=<name>       Name of the generated log file.               [default: fs_upload.log]
--log-file-dir=<dir>         Path to the log file storing information of
                             what has been uploaded and where. Defaults
                             to the <fileOrDir> directory.
--no-logging                 No logging flag.
--dry-run                    Run the triple store uploader tool in a dry
                             run without uploading any triples.
"""
def start():
    try:
        args = docopt(doc)
    except DocoptExit:
        raise DocoptExit('Error: oupload called with wrong arguments.')

    app.fs_upload_wrapper(
        fileOrDir = args['<fileOrDir>'],
        file_server_filext = args['--file-server-filext'],
        file_server_subdir = args['--file-server-subdir'],
        log_file_name = args['--log-file-name'],
        log_file_dir = args['--log-file-dir'],
        no_logging = args['--no-logging'],
        dry_run = args['--dry-run']
    )

if __name__ == '__main__':
    start()