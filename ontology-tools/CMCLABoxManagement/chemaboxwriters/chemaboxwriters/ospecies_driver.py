import docopt
from chemaboxwriters.app import write_abox

__doc__: str = """aboxwriter
Usage:
    ospecies <fileOrDir>  [--inp-file-type=<type>]
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
--inp-file-type=<type>         Types of the allowed input files
                               to the ospecies abox writer:
                                - quantum calculation log            [default: qc_log]
                                - quantum calculation json           [qc_json]
                                - ontospecies meta json              [os_json]
                                - ontospecies meta csv               [os_csv]
--qc-log-ext=<ext>             Extensions of the quantum
                               calculation log files, defaults
                               to ".log, .g09" if not specified
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
--info                         Prints the pipeline's info without running it.
"""

def start():
    try:
        args = docopt.docopt(__doc__)
    except docopt.DocoptExit: # type: ignore
        raise docopt.DocoptExit('Error: ospecies called with wrong arguments.') # type: ignore

    if args["--dry-run"].upper() == "TRUE":
        dry_run = True
    elif args["--dry-run"].upper() == "FALSE": 
        dry_run = False
    else:
        raise docopt.DocoptExit('Error: incorrect --dry-run option. Please choose between True and False.') #type: ignore

    write_abox(
        pipeline_type = 'om',
        fileOrDir=args['<fileOrDir>'],
        inpFileType=args['--inp-file-type'],
        outDir=args['--out-dir'],
        qcLogExt=args['--qc-log-ext'],
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