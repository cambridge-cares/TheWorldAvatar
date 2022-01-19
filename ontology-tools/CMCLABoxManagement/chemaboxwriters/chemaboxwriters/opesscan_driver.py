import docopt
from chemaboxwriters.app import write_abox
from typing import Any, Dict

__doc__: str = """aboxwriter
Usage:
    opesscan <fileOrDir>  [(--os-iris=<iri> --os-atoms-iris=<iris> --oc-atoms-ids=<ids>)]
                          [--inp-file-type=<type>]
                          [--qc-log-ext=<ext>]
                          [--out-dir=<dir>]
                          [--log-file-name=<name>]
                          [--log-file-dir=<dir>]
                          [--no-file-logging]
                          [--fs-upload-subdirs=<subdirs>]
                          [--ts-upload-nmsp=<nmsp>]
                          [--dry-run=<dry_run>]
                          [--disable-uploads]
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
--qc-log-ext=<ext>             Extensions of the quantum
                               calculation log files,
                               if not specified, defaults to
                               ".log,.g03,.g09,.g16"
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
--disable-uploads              Disables file server and triple store
                               uploads. Differes from the --dry-run
                               option in that it does require uploaders
                               env variables to be set to run the
                               pipeline.
--fs-upload-subdirs=<subdirs>  Replaces any default file server
                               subdirs used when uploading files.
                               Use it as follows:
                                 - Set subdirs for all handlers
                                     <subdir>
                                 - Set subdirs for a handler
                                   all its sub-handlers (if any)
                                     <handler1>:<subdir>
                                 - Set subdirs for a nested handler
                                     <handler1>.<handler2>:<subdir>
                                 - Set subdirs for multiple handlers
                                   at once
                                     <handler1>:<subdir1>,<handler2>:<subdir2>
                               To see handlers default subdirs
                               use the --info option.
--ts-upload-nmsp=<nmsp>        Replaces any default triple store
                               namespaces used when uploading triples.
                               Use it as follows:
                                 - Set nmsp for all handlers
                                     <nmsp>
                                 - Set nmsp for a handler
                                   all its sub-handlers (if any)
                                     <handler1>:<nmsp>
                                 - Set nmsp for a nested handler
                                     <handler1>.<handler2>:<nmsp>
                                 - Set nmsps for multiple handlers at once
                                     <handler1>:<nmsp1>,<handler2>:<nmsp2>
                               To see handlers default nmsp
                               use the --info option.
--info                         Prints the pipeline's info. Do not run it.  
"""

def start():
    try:
        args = docopt.docopt(__doc__)
    except docopt.DocoptExit: # type: ignore
        raise docopt.DocoptExit('Error: opesscan called with wrong arguments.') # type: ignore

    handlerFuncKwargs: Dict[str, Any] = {}
    if args['--os-iris'] is not None:
        handlerFuncKwargs = {
            'OC_JSON_TO_OPS_JSON':{
                'os_iris': args['--os-iris'],
                'os_atoms_iris': args['--os-atoms-iris'],
                'oc_atoms_pos': args['--oc-atoms-ids']
            }
        }

    if args["--dry-run"].upper() == "TRUE":
        dry_run = True
    elif args["--dry-run"].upper() == "FALSE": 
        dry_run = False
    else:
        raise docopt.DocoptExit('Error: incorrect --dry-run option. Please choose between True and False.') #type: ignore

    write_abox(
        pipeline_type = 'ops',
        fileOrDir = args['<fileOrDir>'],
        inpFileType = args['--inp-file-type'],
        qcLogExt = args['--qc-log-ext'],
        outDir = args['--out-dir'],
        handlerKwargs = handlerFuncKwargs,
        log_file_dir = args['--log-file-dir'],
        log_file_name = args['--log-file-name'],
        no_file_logging = args['--no-file-logging'],
        fs_upload_subdirs = args['--fs-upload-subdirs'],
        ts_upload_nmsp = args['--ts-upload-nmsp'],
        dry_run = dry_run,
        disable_uploads=args["--disable-uploads"],
        info = args['--info']
    )

if __name__ == '__main__':
    start()