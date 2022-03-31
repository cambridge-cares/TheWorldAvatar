
import pesfit.app as app
from docopt import docopt, DocoptExit
import sys

__doc__="""pesfit
Usage:
    pesfit  (--opesIRI=<ONTO_PES_SCAN_IRI>)
            (--conf-file=<CONF-FILE-NAME>)

Options: 
--opesIRI=<ONTO_PES_SCAN_IRI>           OntoPESScan IRI
--conf-file=<CONF-FILE-NAME-OR-PATH>    configuration file name or path
"""

def main():
    try:
        args = docopt(__doc__,version='1.0.0rc2')
    except DocoptExit:
        raise DocoptExit('Error: pesfit called with wrong arguments.')

    app.pesfit_wrapper(args)

if __name__ == "__main__":
   main()