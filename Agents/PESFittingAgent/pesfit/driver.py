
import pesfit.app as app
from docopt import docopt, DocoptExit
import sys

__doc__="""pesfit
Usage:
    pesfit  <ONTO_PES_SCAN_IRI>
"""

def main():
    try:
        args = docopt(__doc__)
    except DocoptExit:
        raise DocoptExit('Error: pesfit called with wrong arguments.')

    app.pesfit_wrapper(args)

if __name__ == "__main__":
   main()