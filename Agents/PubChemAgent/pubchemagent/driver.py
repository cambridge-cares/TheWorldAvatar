import pubchemagent.app as app
from docopt import docopt, DocoptExit
import sys

__doc__="""pubchemagent
Usage:
    pubchemagent  (--inchi=<inchi>)

Options:
--inchi=<inchi>          Inchi string
"""

def main():

    app.populate_elements()

    try:
        args = docopt(__doc__, version='1.0.0rc2')
    except DocoptExit:
        raise DocoptExit('Error: pubchemagent called with wrong arguments.')

    args = {**args}
    inchi = args['--inchi']
    data, IRI = app.query_with_inchi(inchi)
    print(IRI, '\n')
    print(data)
        
if __name__== '__main__':
    main()
