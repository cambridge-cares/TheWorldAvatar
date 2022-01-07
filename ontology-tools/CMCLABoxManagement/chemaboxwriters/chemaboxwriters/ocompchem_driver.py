import docopt
from chemaboxwriters.app import write_abox

__doc__: str = """aboxwriter
Usage:
    ocompchem <fileOrDir>   [--inp-file-type=<type>]
                            [--out-dir=<dir>]
                            [--qc-log-ext=<ext>]
                            [--log-file-name=<name>]
                            [--log-file-dir=<dir>]
                            [--no-file-logging]
                            [--dry-run]

Options:
--inp-file-type=<type>  Types of the allowed input files
                        to the ocompchem abox writer:
                          - quantum calculation log         [default: qc_log]
                          - quantum calculation json        [qc_json]
                          - ontocompchem meta json          [oc_json]
                          - ontocompchem meta csv           [oc_csv]
--out-dir=<dir>         Output directory to write the
                        abox files to. If not provided
                        defaults to the directory of the
                        input file
--qc-log-ext=<ext>      Extensions of the quantum
                        calculation log files, defaults
                        to ".log, .g09" if not specified
--log-file-name=<name>  Name of the generated log file.
--log-file-dir=<dir>    Path to the abox writer log file.
                        Defaults to the <file_or_dir> dir.
--no-file-logging       No logging to a file flag.
--dry-run               Run the abox writer tool in a dry
                        run mode (files are not uploaded).
"""

def start():
    try:
        args = docopt.docopt(__doc__)
    except docopt.DocoptExit: #type: ignore
        raise docopt.DocoptExit('Error: ocompchem called with wrong arguments.') #type: ignore

    write_abox(
        pipeline_type = 'oc',
        fileOrDir = args['<fileOrDir>'],
        inpFileType = args['--inp-file-type'],
        outDir = args['--out-dir'],
        qcLogExt = args['--qc-log-ext'],
        log_file_dir = args['--log-file-dir'],
        log_file_name = args['--log-file-name'],
        no_file_logging = args['--no-file-logging'],
        dry_run = args['--dry-run']
    )

if __name__ == '__main__':
    start()