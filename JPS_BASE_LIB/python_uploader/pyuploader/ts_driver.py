import docopt
import pyuploader.app as app

doc = """pyuploader
Usage:
    ts_upload <file_or_dir>  [--tstore-nmsp=<nmsp>]
                             [--tstore-filext=<ext>]
                             [--log-file-name=<name>]
                             [--log-file-dir=<dir>]
                             [--no-file-logging]
                             [--dry-run]

Options:
--tstore-nmsp=<nmsp>    Triple store namespace.                       [default: base]
--tstore-filext=<ext>   List of extensions used to select files       [default: .owl]
                        that will be uploaded to the triple store.
--log-file-name=<name>  Name of the generated log file.               [default: ts_upload.log]
--log-file-dir=<dir>    Path to the log file storing information of
                        what has been uploaded and where. Defaults
                        to the <fileOrDir> directory.
--no-file-logging       No logging flag to a file.
--dry-run               Run the triple store uploader tool in a dry
                        run without uploading any triples.
"""

def start():
    try:
        args = docopt.docopt(doc)
    except docopt.DocoptExit:
        raise docopt.DocoptExit('Error: ts_upload called with wrong arguments.')

    app.ts_upload_wrapper(
        file_or_dir = args['<file_or_dir>'],
        tstore_nmsp = args['--tstore-nmsp'],
        tstore_filext = args['--tstore-filext'],
        log_file_dir = args['--log-file-dir'],
        log_file_name = args['--log-file-name'],
        no_file_logging = args['--no_file_logging'],
        dry_run = args['--dry-run']
    )

if __name__ == '__main__':
    start()