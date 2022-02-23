import docopt
from chemaboxwriters.app import write_abox
from chemaboxwriters.ontomops.pipeline import OMOPS_PIPELINE

__doc__: str = """aboxwriter
Usage:
    omops   [--file-or-dir=<file>]
            [--inp-file-type=<type>]
            [--file-ext=<ext>]
            [--config-file=<file>]
            [--out-dir=<dir>]
            [--log-file-name=<name>]
            [--log-file-dir=<dir>]
            [--no-file-logging]
            [--dry-run=<dry_run>]
            [--info]

Options:
--file-or-dir=<file>           Path to the input file or directory
--inp-file-type=<type>         Types of the allowed input files
                               to the omops abox writer:
                                - omops input json file           [default: ominp_json]
                                - omops processed json file       [omops_json]
                                - omops meta csv                  [omops_csv]
--file-ext=<ext>               Extensions of the input files,
                               specified as a comma separated
                               string, e.g. --file-ext="ominp_json"
                               if not provided, defaults to the
                               input file type
--config-file=<file>           Path to the config file specifying upload
                               options. If not provided, the code will
                               try to read the config file path from
                               the ABOXWRITERS_CONFIG_FILE environment
                               variable
--out-dir=<dir>                Output directory to write the
                               abox files to. If not provided
                               defaults to the directory of the
                               input file
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
        raise docopt.DocoptExit("Error: omops called with wrong arguments.")  # type: ignore

    if args["--dry-run"].upper() == "TRUE":
        dry_run = True
    elif args["--dry-run"].upper() == "FALSE":
        dry_run = False
    else:
        raise docopt.DocoptExit("Error: incorrect --dry-run option. Please choose between True and False.")  # type: ignore

    write_abox(
        pipeline_type=OMOPS_PIPELINE,
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
