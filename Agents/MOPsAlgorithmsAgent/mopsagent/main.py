from docopt import docopt, DocoptExit
from mopsagent.mopsoperations.assemblyModelAlgorithms import fromAssemblyModToGenBU
import json

doc = """mopsagent

Usage:
    mopsagent assemblyToBU <assemblyModelString>
"""

def start():
    try:
        args = docopt(doc)
    except DocoptExit:
        raise DocoptExit('Error: jpsdukg called with wrong arguments.')

    if args['assemblyToBU']:
        response = fromAssemblyModToGenBU(assemblyModelString=args['<assemblyModelString>'])
        ### You can disable this to see all paired units### print(response)

if __name__ == '__main__':
    start()