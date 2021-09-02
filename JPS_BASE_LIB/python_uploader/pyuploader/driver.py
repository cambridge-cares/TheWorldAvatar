from docopt import docopt, DocoptExit
import pyuploader.app as app

doc = """pyuploader
Usage:
    oupload <fileOrDir> --config-file=<file>
    oupload <fileOrDir> (--tstore-url=<url>)
                        (--namespace=<nmsp>)
                        [--tstore-file-ext=<ext>]
                        [--ftp-file-ext=<ext>]
                        [--log-file-name=<name>]
                        [--log-file-dir=<dir>]
                        [--dry-run]

Options:
--tstore-url=<url>             Url of the triple store.
--namespace=<nmsp>             Namespace of the triple store.
--tstore-file-ext=<ext>        List of extensions used to select files             [default: .owl]
                               that will be uploaded to the triple store.
--ftp-file-ext=<ext>           List of extensions used to select files
                               that will be uploaded to the ftp server.
--log-file-name=<name>         Name of the generated log file.                     [default: app.log]
--log-file-dir=<dir>           Path to the log file storing information of
                               what has been uploaded and where. Defaults
                               to the current working directory.
--dry-run                      Run the uploader tool in dry run without
                               uploading any files.
"""

def start():
    try:
        args = docopt(doc)
    except DocoptExit:
        raise DocoptExit('Error: oupload called with wrong arguments.')

    app.oupload_wrapper(args)

if __name__ == '__main__':
    start()