from docopt import docopt, DocoptExit
from chemaboxwriters.ontopesscan import write_abox

doc = """aboxwriter
Usage:
    opesscan <fileOrDir>  [(--os-iris=<iri> --os-atoms-iris=<iris> --oc-atoms-ids=<ids>)]
                          [--inp-file-type=<type>]
                          [--qc-log-ext=<ext>]
                          [--out-dir=<dir>]
                          [--out-base-name=<name>]

Options:
--os-iris=<iri>         OntoSpecies iri associated with the
                        scan points
--os-atoms-iris=<iris>  Comma separated iris of ontospecies
                        atoms defining the scan coordinate
--oc-atoms-ids=<ids>    Positions of atoms in ontocompchem
                        scan point geometries (index starts
                        from one), e.g. "1,2"
--inp-file-type=<type>  Types of the allowed input files
                        to the opesscan abox writer. There
                        are two input file categories:
                        * Input files requiring extra
                          species/atoms iris and positions
                          input:
                          - quantum calculation log           [default: qc_log]
                          - quantum calculation json          [qc_json]
                          - ontocompchem meta json            [oc_json]
                        * Input files not requiring extra
                          input:
                          - ontopesscan meta json             [ops_json]
                          - ontopesscan meta csv              [csv]
--qc-log-ext=<ext>      Extensions of the quantum
                        calculation log files,
                        if not specified, defaults to
                        ".log,.g03,.g09,.g16"
--out-dir=<dir>         Output directory to write the
                        abox files to. If not provided
                        defaults to the directory of the
                        input file
--out-base-name=<name>  Base name of the produced output
                        files. If not provided, defaults
                        to the input file base name.
"""

def start():
    try:
        args = docopt(doc)
    except DocoptExit:
        raise DocoptExit('Error: opesscan called with wrong arguments.')


    handlerFuncKwargs={}
    if args['--os-iris'] is not None:
        handlerFuncKwargs={
            'OC_JSON_TO_OPS_JSON':{'os_iris': args['--os-iris'], \
                                   'os_atoms_iris': args['--os-atoms-iris'], \
                                   'oc_atoms_pos': args['--oc-atoms-ids']}}

    write_abox(fileOrDir=args['<fileOrDir>'],
               inpFileType=args['--inp-file-type'], \
               qcLogExt=args['--qc-log-ext'], \
               outDir=args['--out-dir'], \
               outBaseName=args['--out-base-name'], \
               OPS_handlerFuncKwargs=handlerFuncKwargs)

if __name__ == '__main__':
    start()