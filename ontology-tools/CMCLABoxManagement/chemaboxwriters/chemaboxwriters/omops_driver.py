import docopt
from chemaboxwriters.app import write_abox

__doc__: str = """aboxwriter
Usage:
    omops <fileOrDir>  [--inp-file-type=<type>]
                       [--out-dir=<dir>]
                       [--log-file-name=<name>]
                       [--log-file-dir=<dir>]
                       [--no-file-logging]
                       [--dry-run]

Options:
--inp-file-type=<type>  Types of the allowed input files
                        to the omops abox writer:
                         - omops input json file           [default: ominp_json]
                         - omops processed json file       [omops_json]
                         - omops meta csv                  [omops_csv]
--out-dir=<dir>         Output directory to write the
                        abox files to. If not provided
                        defaults to the directory of the
                        input file
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
        raise docopt.DocoptExit('Error: omops called with wrong arguments.') #type: ignore

    write_abox(
        pipeline_type = 'om',
        fileOrDir = args['<fileOrDir>'],
        inpFileType = args['--inp-file-type'],
        outDir = args['--out-dir'],
        log_file_dir = args['--log-file-dir'],
        log_file_name = args['--log-file-name'],
        no_file_logging = args['--no-file-logging'],
        dry_run = args['--dry-run']
    )

if __name__ == '__main__':
    start()