import pubchemagent.app as app
from docopt import docopt, DocoptExit
import sys
import time

__doc__="""pubchemagent
Usage:
    pubchemagent  (--inchi=<inchi>)

Options:
--inchi=<inchi>          Inchi string
"""

def main():

    #for el in range(1,41):
    #    app.element_instantiation(el)

    #try:
    #    args = docopt(__doc__, version='1.0.0rc2')
    #except DocoptExit:
    #    raise DocoptExit('Error: pubchemagent called with wrong arguments.')

    #args = {**args}
    #inchi = args['--inchi']
    #app.species_instantiation(inchi)

    inchi = 'inchi string'
    a = 0
    a_stop = 100
    with open('inchi_list.txt') as f:
        while inchi:
            inchi = f.readline().replace("\"","").replace("\n", "")
            a = a+1
            if a>=a_stop:
                print('Species ' + str(a) + ': ' + inchi)
                start_time = time.time()
                app.species_instantiation(inchi)
                print("--- Total: %s seconds ---" % (time.time() - start_time))

    #for inchi in [  'InChI=1S/C10H10/c1-2-8-5-6-9-4-3-7(1)10(8)9/h1-10H']:
    #    app.species_instantiation(inchi)
    
        
if __name__== '__main__':
    main()
