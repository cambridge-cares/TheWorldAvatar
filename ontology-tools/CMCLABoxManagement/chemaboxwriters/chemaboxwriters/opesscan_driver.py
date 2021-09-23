from docopt import docopt, DocoptExit
from chemaboxwriters.ontopesscan import write_abox
import json

doc = """aboxwriter
Usage:
    opesscan <fileOrDir>  [(--os-iris=<iri> --os-atoms-iris=<iris> --oc-atoms-ids=<ids>)]
                          [--inp-file-type=<type>]
                          [--qc-log-ext=<ext>]
                          [--out-dir=<dir>]
                          [--out-base-name=<name>]
                          [--ops-handlers-args=<handlargs>]
                          [--oc-handlers-args=<handlargs>]

Options:
--os-iris=<iri>                  OntoSpecies iri associated with the
                                 scan points
--os-atoms-iris=<iris>           Comma separated iris of ontospecies
                                 atoms defining the scan coordinate
--oc-atoms-ids=<ids>             Positions of atoms in ontocompchem
                                 scan point geometries (index starts
                                 from one), e.g. "1,2"
--inp-file-type=<type>           Types of the allowed input files
                                 to the opesscan abox writer. There
                                 are two input file categories:
                                 * Input files requiring extra
                                   species/atoms iris and positions
                                   input:
                                   - quantum calculation log                  [default: qc_log]
                                   - quantum calculation json                 [qc_json]
                                   - ontocompchem meta json                   [oc_json]
                                 * Input files not requiring extra
                                   input:
                                   - ontopesscan meta json                    [ops_json]
                                   - ontopesscan meta csv                     [csv]
--qc-log-ext=<ext>               Extensions of the quantum
                                 calculation log files,
                                 if not specified, defaults to
                                 ".log,.g03,.g09,.g16"
--out-dir=<dir>                  Output directory to write the
                                 abox files to. If not provided
                                 defaults to the directory of the
                                 input file
--out-base-name=<name>           Base name of the produced output
                                 files. If not provided, defaults
                                 to the input file base name.
--ops-handlers-args=<handlargs>  Any supported ops scan stage handlers'       [default: {}]
                                 arguments. The arguments are passed
                                 as a json formatted string, where
                                 the first level keys are handlers
                                 names followed by their arguments
                                 names and values, e.g:
                                 {\\"OC_JSON_TO_OPS_JSON\\": {\\"random_id\\": \\"123\\"}}
                                 with all double quotes escaped.
--oc-handlers-args=<handlargs>   Any supported ontocompchem stage handlers'   [default: {}]
                                 arguments. The same syntax as we with
                                 ops scan handlers args.
"""

def start():
    try:
        args = docopt(doc)
    except DocoptExit:
        raise DocoptExit('Error: opesscan called with wrong arguments.')

    try:
        OPS_handlerFuncKwargs = json.loads(args['--ops-handlers-args'])
    except json.decoder.JSONDecodeError:
        print('Error: Wrong --ops-handlers-args input.')
        return

    try:
        OC_handlerFuncKwargs = json.loads(args['--oc-handlers-args'])
    except json.decoder.JSONDecodeError:
        print('Error: Wrong --oc-handlers-args input.')
        return

    if args['--os-iris'] is not None:
        if 'OC_JSON_TO_OPS_JSON' not in OPS_handlerFuncKwargs:
            OPS_handlerFuncKwargs['OC_JSON_TO_OPS_JSON'] = {}
        if 'os_iris' not in OPS_handlerFuncKwargs['OC_JSON_TO_OPS_JSON']:
            OPS_handlerFuncKwargs['OC_JSON_TO_OPS_JSON']['os_iris'] = args['--os-iris']
        if 'os_atoms_iris' not in OPS_handlerFuncKwargs['OC_JSON_TO_OPS_JSON']:
            OPS_handlerFuncKwargs['OC_JSON_TO_OPS_JSON']['os_atoms_iris'] = args['--os-atoms-iris']
        if 'oc_atoms_pos' not in OPS_handlerFuncKwargs['OC_JSON_TO_OPS_JSON']:
            OPS_handlerFuncKwargs['OC_JSON_TO_OPS_JSON']['oc_atoms_pos'] = args['--oc-atoms-ids']

    write_abox(
                fileOrDir=args['<fileOrDir>'],
                inpFileType=args['--inp-file-type'],
                qcLogExt=args['--qc-log-ext'],
                outDir=args['--out-dir'],
                outBaseName=args['--out-base-name'],
                OPS_handlerFuncKwargs=OPS_handlerFuncKwargs,
                OC_handlerFuncKwargs=OC_handlerFuncKwargs
                )

if __name__ == '__main__':
    start()