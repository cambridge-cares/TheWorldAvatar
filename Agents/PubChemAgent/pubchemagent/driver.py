import pubchemagent.app as app
from docopt import docopt, DocoptExit
import time

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
    
    print('Species: ' + inchi)
    start_time = time.time()
    try: 
        app.species_instantiation(inchi)
        print("--- Total: %s seconds ---" % (time.time() - start_time))
    except:
        print('Error: pubchemagent failed to instantiate.')   
        
if __name__== '__main__':
    main()
