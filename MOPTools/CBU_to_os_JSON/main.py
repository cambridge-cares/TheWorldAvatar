from docopt import docopt, DocoptExit
from CBU_to_os_JSON.cbuCSVtoJSON.osJSONwrapper import cbuOperations

doc = """cbutojson
Usage: cbutojson <csvfile> <xyzrelpath> [--outFilePath = <path>]

Options:
-h --help  Show this screen.
--outFilePath = <path> Output directory to write the
                       species JSON files to. 
                       By default, it writes to a folder 
                       in the run directory with the 
                       stem name osJSON_. 
"""

def start():
    try:
        args = docopt(doc)
    except DocoptExit:
        raise DocoptExit('Error: cbutojson called with wrong arguments.')
    cbuCSVFilePath = args['<csvfile>']
    xyzpathstem = args['<xyzrelpath>']
    outFilePath = args['--outFilePath']
    cbuOperations(cbuCSVFilePath,xyzpathstem,outFilePath)

if __name__ == '__main__':
    start()