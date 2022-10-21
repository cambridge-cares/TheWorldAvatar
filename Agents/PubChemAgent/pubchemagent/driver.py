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
    try:
        args = docopt(__doc__, version='1.0.0rc2')
    except DocoptExit:
        raise DocoptExit('Error: pubchemagent called with wrong arguments.')

    args = {**args}
    inchi = args['--inchi']
    data, source = app.query_with_inchi(inchi)
    print(source, '\n')
    print(data)

    #    for inchi in ['InChI=1S/C6H6/c1-2-4-6-5-3-1/h1-6H', 
    #              'InChI=1S/C10H10/c1-2-3-7-10-8-5-4-6-9-10/h4-6,8-9H,2H2,1H3', 
    #              'InChI=1S/C10H10/c1-2-8-5-6-9-4-3-7(1)10(8)9/h1-10H',
    #              'InChI=1S/C6H8O6/c7-1-2(8)5-3(9)4(10)6(11)12-5/h2,5,7-10H,1H2/t2-,5+/m0/s1',
    #              'InChI=1S/C9H10O3/c1-6-3-4-8(10)7(5-6)9(11)12-2/h3-5,10H,1-2H3',
    #              'InChI=1S/C3H6O/c1-3(2)4/h1-2H3']:

    # A test INSERT function
    # insert_with_inchi('inchi')

        
if __name__== '__main__':
    main()
