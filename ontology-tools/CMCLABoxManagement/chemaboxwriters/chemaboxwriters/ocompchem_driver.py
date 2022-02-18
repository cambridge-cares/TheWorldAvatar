import docopt
from chemaboxwriters.app import write_abox
from chemaboxwriters.ontocompchem.pipeline import OC_PIPELINE

__doc__: str = """aboxwriter
Usage:
    ocompchem <fileOrDir>   [--inp-file-type=<type>]
                            [--out-dir=<dir>]
                            [--file-ext=<ext>]
                            [--log-file-name=<name>]
                            [--log-file-dir=<dir>]
                            [--no-file-logging]
                            [--dry-run=<dry_run>]
                            [--info]

Options:
--inp-file-type=<type>         Types of the allowed input files
                               to the ocompchem abox writer:
                                 - quantum calculation log         [default: qc_log]
                                 - quantum calculation json        [qc_json]
                                 - ontocompchem meta json          [oc_json]
                                 - ontocompchem meta csv           [oc_csv]
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
                               To see handlers default nmsp use the --info option.
--info                         Prints the pipeline's info without running it.
"""


def start():
    try:
        args = docopt.docopt(__doc__)
    except docopt.DocoptExit:  # type: ignore
        raise docopt.DocoptExit("Error: ocompchem called with wrong arguments.")  # type: ignore

    if args["--dry-run"].upper() == "TRUE":
        dry_run = True
    elif args["--dry-run"].upper() == "FALSE":
        dry_run = False
    else:
        raise docopt.DocoptExit("Error: incorrect --dry-run option. Please choose between True and False.")  # type: ignore

    write_abox(
        pipeline_type=OC_PIPELINE,
        file_or_dir=args["<fileOrDir>"],
        input_file_type=args["--inp-file-type"],
        file_ext=args["--file-ext"],
        out_dir=args["--out-dir"],
        log_file_dir=args["--log-file-dir"],
        log_file_name=args["--log-file-name"],
        no_file_logging=args["--no-file-logging"],
        dry_run=dry_run,
        info=args["--info"],
    )


if __name__ == "__main__":
    start()
