import docopt
from chemaboxwriters.app import write_abox
from chemaboxwriters.ontospecies.pipeline import OS_PIPELINE

__doc__: str = """aboxwriter
Usage:
    ospecies    [--file-or-dir]
                [--inp-file-type=<type>]
                [--file-ext=<ext>]
                [--out-dir=<dir>]
                [--config-file=<file>]
                [--log-file-name=<name>]
                [--log-file-dir=<dir>]
                [--no-file-logging]
                [--dry-run=<dry_run>]
                [--info]

Options:
--file-or-dir                  Path to input file or directory
--inp-file-type=<type>         Types of the allowed input files
                               to the ospecies abox writer:
                                - quantum calculation log            [default: qc_log]
                                - quantum calculation json           [qc_json]
                                - ontospecies meta json              [os_json]
                                - ontospecies meta csv               [os_csv]
--file-ext=<ext>               Extensions of the input files,
                               specified as a comma separated
                               string, e.g. --file-ext="out,log"
                               if not provided, defaults to the
                               following values:
                                - qc_log stage:
                                  "qc_log,log,out,g03,g09,g16"
                                - for all other stages
                                  the extension equals to the
                                  input file type
--config-file=<file>           Path to the config file specifying upload
                               options. If not provided, the code will
                               try to read the config file path from
                               the ABOXWRITERS_CONFIG_FILE environment
                               variable
--out-dir=<dir>                Output directory to write the
                               abox files to. If not provided
                               defaults to the directory of the
                               input file.
--log-file-name=<name>         Name of the generated log file.
--log-file-dir=<dir>           Path to the abox writer log file.
                               Defaults to the <file_or_dir> dir.
--no-file-logging              No logging to a file flag.
--dry-run=<dry_run>            Run the abox writer tool in a dry    [default: True]
                               run mode (files are not uploaded).
                               Choose between True / False
--info                         Prints the pipeline's info without running it.
"""


def start():
    try:
        args = docopt.docopt(__doc__)
    except docopt.DocoptExit:  # type: ignore
        raise docopt.DocoptExit("Error: ospecies called with wrong arguments.")  # type: ignore

    if args["--dry-run"].upper() == "TRUE":
        dry_run = True
    elif args["--dry-run"].upper() == "FALSE":
        dry_run = False
    else:
        raise docopt.DocoptExit("Error: incorrect --dry-run option. Please choose between True and False.")  # type: ignore

    write_abox(
        pipeline_type=OS_PIPELINE,
        file_or_dir=args["--file-or-dir"],
        input_file_type=args["--inp-file-type"],
        file_ext=args["--file-ext"],
        config_file=args["--config-file"],
        out_dir=args["--out-dir"],
        log_file_dir=args["--log-file-dir"],
        log_file_name=args["--log-file-name"],
        no_file_logging=args["--no-file-logging"],
        dry_run=dry_run,
        info=args["--info"],
    )


if __name__ == "__main__":
    start()
