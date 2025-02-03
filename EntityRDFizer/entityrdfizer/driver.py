from docopt import docopt, DocoptExit
import entityrdfizer.app as app
import os


__doc__ = """csv2rdf

Usage:
    csv2rdf <csvFileOrDirPath> --csvType=<type> [--outDir=<dir>] [--csvTbox=<path>]

Options:
--csvType=<type>  Type of the csv file.
                  Choose between abox / tbox
--outDir=<dir>    Output directory path
--csvTbox=<path>  Path to the tbox ontology
"""

def main():
    #print( "doc = ", __doc__ )
    try:
        args = docopt(__doc__)
    except DocoptExit:
        raise DocoptExit('Error: parser called with wrong arguments.')

    app.csv2rdf_wrapper(
        csvFileOrDirPath = os.path.abspath(args['<csvFileOrDirPath>']),
        csvType = args['--csvType'],
        outDir = args['--outDir'],
        csvTbox = args['--csvTbox']
        )

if __name__ == '__main__':
    main()