import docopt
from chemaboxwriters.app import write_abox
from chemaboxwriters.ontopesscan.pipeline import OPS_PIPELINE
from typing import Any, Dict

__doc__: str = """aboxwriter
Usage:
    opesscan <fileOrDir>  [(--os-iris=<iri> --os-atoms-iris=<iris> --oc-atoms-ids=<ids>)]
                          [--inp-file-type=<type>]
                          [--file-ext=<ext>]
                          [--out-dir=<dir>]
                          [--log-file-name=<name>]
                          [--log-file-dir=<dir>]
                          [--no-file-logging]
                          [--dry-run=<dry_run>]
                          [--info]

Options:
--os-iris=<iri>                OntoSpecies iri associated with the
                               scan points
--os-atoms-iris=<iris>         Comma separated iris of ontospecies
                               atoms defining the scan coordinate
--oc-atoms-ids=<ids>           Positions of atoms in ontocompchem
                               scan point geometries (index starts
                               from one), e.g. "1,2"
--inp-file-type=<type>         Types of the allowed input files
                               to the opesscan abox writer. There
                               are two input file categories:
                               * Input files requiring extra
                                 species/atoms iris and positions
                                 input:
                                 - quantum calculation log            [default: qc_log]
                                 - quantum calculation json           [qc_json]
                                 - ontocompchem meta json             [oc_json]
                               * Input files not requiring extra
                                 input:
                                 - ontopesscan meta json              [ops_json]
                                 - ontopesscan meta csv               [ops_csv]
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
                               input file.
--log-file-name=<name>         Name of the generated log file.
--log-file-dir=<dir>           Path to the abox writer log file.
                               Defaults to the <file_or_dir> dir.
--no-file-logging              No logging to a file flag.
--dry-run=<dry_run>            Run the abox writer tool in a dry    [default: True]
                               run mode (files are not uploaded).
                               Choose between True / False
--info                         Prints the pipeline's info. Do not run it.
"""


def start():
    try:
        args = docopt.docopt(__doc__)
    except docopt.DocoptExit:  # type: ignore
        raise docopt.DocoptExit("Error: opesscan called with wrong arguments.")  # type: ignore

    handlerFuncKwargs: Dict[str, Any] = {}
    if args["--os-iris"] is not None:
        handlerFuncKwargs = {
            "OC_JSON_TO_OPS_JSON": {
                "os_iris": args["--os-iris"],
                "os_atoms_iris": args["--os-atoms-iris"],
                "oc_atoms_pos": args["--oc-atoms-ids"],
            }
        }

    if args["--dry-run"].upper() == "TRUE":
        dry_run = True
    elif args["--dry-run"].upper() == "FALSE":
        dry_run = False
    else:
        raise docopt.DocoptExit("Error: incorrect --dry-run option. Please choose between True and False.")  # type: ignore

    write_abox(
        pipeline_type=OPS_PIPELINE,
        file_or_dir=args["<fileOrDir>"],
        input_file_type=args["--inp-file-type"],
        file_ext=args["--file-ext"],
        out_dir=args["--out-dir"],
        handler_kwargs=handlerFuncKwargs,
        log_file_dir=args["--log-file-dir"],
        log_file_name=args["--log-file-name"],
        no_file_logging=args["--no-file-logging"],
        dry_run=dry_run,
        info=args["--info"],
    )


if __name__ == "__main__":
    start()
