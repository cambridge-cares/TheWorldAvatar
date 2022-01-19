from docopt import docopt, DocoptExit
import entityrdfizer.app as app
import os


__doc__ = """csv2rdf

Usage:
    csv2rdf <csvFileOrDirPath> --csvType=<type> [--outDir=<dir>]

Options:
--csvType=<type>  Type of the csv file.
                  Choose between abox/tbox   [default: abox]
--outDir=<dir>    Output directory path
"""

def main():
    try:
        args = docopt(__doc__)
    except DocoptExit:
        raise DocoptExit('Error: parser called with wrong arguments.')

    app.csv2rdf_wrapper(
        csvFileOrDirPath = os.path.abspath(args['<csvFileOrDirPath>']),
        csvType = args['--csvType'],
        outDir = args['--outDir'])

if __name__ == '__main__':
    main()