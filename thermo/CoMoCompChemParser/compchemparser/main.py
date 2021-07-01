from compchemparser.app import runParser, runScan
from docopt import docopt, DocoptExit

__doc__ = """ccparse

Usage:
    ccparse parse <logFileOrDir> [-n --logExt=<LOG_EXT>]
    ccparse scan <scanJsonDir> --osid=<ONTOSPEC_ID> --osc=<ONTOSPEC_SCAN_COORD> --occ=<ONTOCOMP_SCAN_COORD>

Options:
    -n                              Suppress parser command output files (json and csv)
    --logExt=<LOG_EXT>              Log files file extension for log file directory input [default: *.log]
    --osid=<ONTOSPEC_ID>            OntoSpecies id (iri or inchi)
    --osc=<ONTOSPEC_SCAN_COORD>     Scan coordinate w.r.t OntoSpecies atoms order (e.g. "1,4,5")
    --occ=<ONTOCOMP_SCAN_COORD>     Scan coordinate w.r.t OntoCompChem atoms order (e.g. "3,6,1")
"""

def main():
    try:
        args = docopt(__doc__)
    except DocoptExit:
        raise DocoptExit('Error: parser called with wrong arguments.')


    if args['parse']:
        runParser(args)
    elif args['scan']:
        runScan(args)

if __name__ == '__main__':
    main()