from docopt import docopt, DocoptExit
from MOP_to_omJSON.mopcsv_operations.omjson_operations import mopCSVOperations

doc = """moptojson
Usage: moptojson <cbucsv> <mopcsv> 

Options:
-h --help  Show this screen.

"""

def start():
    try:
        args = docopt(doc)
    except DocoptExit:
        raise DocoptExit('Error: cbutojson called with wrong arguments.')

    cbuIRICSVFilePath = args['<cbucsv>']
    mopCSVFilePath = args['<mopcsv>']
    mopCSVOperations(cbuIRICSVFilePath, mopCSVFilePath)

if __name__ == '__main__':
    start()