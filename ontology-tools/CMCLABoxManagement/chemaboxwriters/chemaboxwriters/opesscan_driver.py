from docopt import docopt, DocoptExit
#from chemaboxwriters.ontopesscan import write_opesscan_abox

doc = """aboxwriter
Usage:
    opesscan <fileOrDir>  (--os-iri=<iri> --os-atoms-iris=<iris> --oc-atoms-ids=<ids>
                            | --inp-file-type=<type>)
                          [--out-dir=<dir>]

Options:
--inp-file-type=<type>  Types of the allowed input files
                        to the opesscan abox writer:
                         - ontocompchem meta json, this       [default: oc_json]
                           option is explicitly set if
                           the opesscan command is run
                           with --os-iri, --os-atoms-iris,
                           and --oc-atoms-ids arguments
                         - ontopesscan meta json              [ops_json]
                         - ontopesscan meta csv               [csv]
--out-dir=<dir>         Output directory to write the abox    [default: ./]
                        files to
--os-iri=<iri>          OntoSpecies iri associated with the
                        scan points
--os-atoms-iri=<iris>   Comma separated iris of ontospecies
                        atoms defining the scan coordinate
--oc-atoms-ids=<ids>    Positions of atoms in ontocompchem
                        scan point geometries (index starts
                        from zero), e.g. "0,4"
"""

def start():
    try:
        args = docopt(doc)
    except DocoptExit:
        raise DocoptExit('Error: chemutils called with wrong arguments.')

    pass

if __name__ == '__main__':
    start()