import docopt
import chemaboxwriters.app as app
import chemaboxwriters.app_exceptions.app_exceptions as app_exc
import chemaboxwriters.common.assemble_pipeline as asp
import chemaboxwriters.common.utilsfunc as utilsfunc
from typing import Dict
import logging


logger = logging.getLogger(__name__)

__doc__: str = """aboxwriter
Usage:
   aboxwriter ospecies   [options]
   aboxwriter ocompchem  [options]
   aboxwriter omops      [options]
   aboxwriter opsscan    [options]

Options:
--help                  Prints this help message.
--file-or-dir=<dir>     Path to the input file or directory.
--inp-file-type=<type>  Types of the allowed input files to the:
                          * ospecies aboxwriter
                            - quantum calculation log (default)  [qc_log]
                            - quantum calculation json           [qc_json]
                            - ontospecies meta json              [os_json]
                            - ontospecies meta csv               [os_csv]
                          * ocompchem aboxwriter
                            - quantum calculation log (default)  [qc_log]
                            - quantum calculation json           [qc_json]
                            - ontocompchem meta json             [oc_json]
                            - ontocompchem meta csv              [oc_csv]
                          * omops aboxwriter
                            - omops input json file (default)    [ominp_json]
                            - omops processed json file          [omops_json]
                            - omops meta csv                     [omops_csv]
                          * opsscan aboxwriter
                            - ontocompchem meta json (default)   [oc_json]
                            - ontopesscan meta json              [ops_json]
                            - ontopesscan meta csv               [ops_csv]
--file-ext=<ext>        Extensions of the input files,
                        specified as a comma separated
                        string, e.g. --file-ext="out,log"
                        if not provided, defaults to the
                        following values:
                         - qc_log stage:
                           "qc_log,log,out,g03,g09,g16"
                         - for all other stages
                           the extension equals to the
                           input file type (e.g. oc_json)
--config-file=<file>    Path to the abox config file.
--out-dir=<dir>         Output directory to write the
                        abox files to. If not provided
                        defaults to the directory of the
                        input file.
--log-file-name=<name>  Name of the generated log file.
--log-file-dir=<dir>    Path to the abox writer log file.
                        If not provided defaults to the
                        directory of the input file.
--no-file-logging       No logging to a file flag.
--dry-run=<dry_run>     Run the abox writer tool in a dry        [default: True]
                        run mode (files are created but
                        not uploaded).
                        Choose between True / False
--info                  Prints the pipeline info.
                        Can be used together with
                        the --config-file option to check
                        the pipeline configuration before
                        running it.
"""


def start():
    try:
        args = docopt.docopt(__doc__)
    except docopt.DocoptExit:  # type: ignore
        raise docopt.DocoptExit(  # type: ignore
            "Error: aboxwriter called with wrong arguments."
        )

    utilsfunc.config_logging(
        log_file_dir=args["--log-file-dir"],
        log_file_name=args["--log-file-name"],
        no_file_logging=args["--no-file-logging"],
    )

    try:
        _process_args(args)

        pipeline = asp.assemble_pipeline(
            pipeline_type=args["--pipeline-type"], config_file=args["--config-file"]
        )

        if args["--info"]:
            pipeline.info()
            return

        if args["--file-or-dir"] is None:
            logger.warning("""Missing --file-or-dir argument. Nothing to process.""")
            return

        pipeline.check_handlers_configs(input_type=args["--inp-file-type"])

        app.write_abox(
            pipeline=pipeline,
            file_or_dir=args["--file-or-dir"],
            input_file_type=args["--inp-file-type"],
            file_ext=args["--file-ext"],
            out_dir=args["--out-dir"],
            dry_run=args["--dry-run"],
        )
        logger.info("Abox writer finished successfully.")
    except Exception as e:
        logger.error(
            (
                "Abox writer failed. Please check the log "
                "for a more detailed error description."
            )
        )
        logger.exception(e)


def _process_args(args: Dict) -> None:

    # set default handlers_kwargs argument
    args["--handlers-kwargs"] = None

    # process dry-run option
    if args["--dry-run"].upper() == "TRUE":
        args["--dry-run"] = True
    elif args["--dry-run"].upper() == "FALSE":
        args["--dry-run"] = False
    else:
        raise app_exc.IncorrectInputArgument(
            "Error: incorrect --dry-run option. Please choose between True and False."
        )

    # process pipeline options
    if args["ospecies"]:
        args["--pipeline-type"] = asp.OS_PIPELINE
        if args["--inp-file-type"] is None:
            args["--inp-file-type"] = "qc_log"
    elif args["ocompchem"]:
        args["--pipeline-type"] = asp.OC_PIPELINE
        if args["--inp-file-type"] is None:
            args["--inp-file-type"] = "qc_log"
    elif args["omops"]:
        args["--pipeline-type"] = asp.OMOPS_PIPELINE
        if args["--inp-file-type"] is None:
            args["--inp-file-type"] = "ominp_json"
    elif args["opsscan"]:
        args["--pipeline-type"] = asp.OPS_PIPELINE
        if args["--inp-file-type"] is None:
            args["--inp-file-type"] = "oc_json"
    else:
        args["pipeline_type"] = "Unknown"


if __name__ == "__main__":
    start()
