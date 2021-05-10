from episode.postprocessEpisode import postprocessEpisode
from docopt import docopt, DocoptExit

doc = """sensorpy.

Usage:
    sensorpy postprocessEpisode [--pathToConcFile OUTPUTPATH --simCRS CRSNAME]

    Options:
    --pathToFile       Absolute path to the concentration file
"""

def start():
    try:
        args = docopt(doc)
    except DocoptExit:
        raise DocoptExit('Error: sensorpy called with wrong arguments.')
    if (args['postprocessEpisode']):
        postprocessEpisode(outputPath=args['OUTPUTPATH'],crsName=args['CRSNAME'])

if __name__ == '__main__':
    start()