from docopt import docopt, DocoptExit
from chemaboxwriters.ontocompchem import write_ocompchem_abox

doc = """aboxwriter
Usage:
    aboxwriter ocompchem <fileOrDir>   [--oc-inp-file-type=<type>]
                                       [--qc-log-ext=<ext>]
                                       [--out-dir=<dir>]
    aboxwriter ospecies <fileOrDir>    [--os-inp-file-type=<type>]
                                       [--qc-log-ext=<ext>]
                                       [--out-dir=<dir>]
    aboxwriter opesscan <fileOrDir>    (--os-iri=<iri> --os-atoms-iris=<iris> --oc-atoms-ids=<ids>
                                        | --ops-inp-file-type=<type>)
                                       [--out-dir=<dir>]

Options:
--oc-inp-file-type=<type>   Types of the allowed input files
                            to the ocompchem abox writer:
                             - quantum calculation log         [default: qc_log]
                             - quantum calculation json        [qc_json]
                             - ontocompchem meta json          [oc_json]
                             - ontocompchem meta csv           [csv]
--os-inp-file-type=<type>   Types of the allowed input files
                            to the ospecies abox writer:
                             - quantum calculation log         [default: qc_log]
                             - quantum calculation json        [qc_json]
                             - ontospecies meta json           [os_json]
                             - ontospecies meta csv            [csv]
--ops-inp-file-type=<type>  Types of the allowed input files
                            to the opesscan abox writer:
                             - ontocompchem meta json, this    [default: oc_json]
                               option is explicitly set if
                               the opesscan command is run
                               with --os-iri, --os-atoms-iris,
                               and --oc-atoms-ids arguments
                             - ontopesscan meta json           [ops_json]
                             - ontopesscan meta csv            [csv]
--qc-log-ext=<ext>          Extensions of the quantum
                            calculation log files               [default: .log, .g09]
--out-dir=<dir>             Output directory to write the abox
                            files to                            [default: ./]
--os-iri=<iri>              OntoSpecies iri associated with the
                            scan points
--os-atoms-iri=<iris>       Comma separated iris of ontospecies
                            atoms defining the scan coordinate
--oc-atoms-ids=<ids>        Positions of atoms in ontocompchem
                            scan point geometries (index starts
                            from zero), e.g. "0,4"
"""

def start():
    try:
        args = docopt(doc)
    except DocoptExit:
        raise DocoptExit('Error: chemutils called with wrong arguments.')

    if args['ocompchem']:
        write_ocompchem_abox(fileOrDir=args['<fileOrDir>'], inpFileType=args['--oc-inp-file-type'], \
                                     outDir=args['--out-dir'], qcLogExt=args['--qc-log-ext'])
    elif args['ospecies']:
        pass
    else:
        pass

    print('done')
if __name__ == '__main__':
    start()