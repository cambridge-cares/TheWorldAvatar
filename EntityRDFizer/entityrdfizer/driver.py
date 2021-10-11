from docopt import docopt, DocoptExit
import entityrdfizer.app as app


__doc__ = """csv2rdf

Usage:
    csv2rdf <csvFileOrDirPath> --csvType=<type> [--outDir=<OUT_DIR>]

Options:
--csvType=<type>     Type of the csv file.
                     Choose between abox/tbox   [default: abox]
--outDir=<OUT_DIR>   Output directory path
"""

def main():
    try:
        args = docopt(__doc__)
    except DocoptExit:
        raise DocoptExit('Error: parser called with wrong arguments.')

    app.csv2rdf_wrapper(
        csvFileOrDirPath = args['csvFileOrDirPath'],
        csvType = args['csvType'],
        outDir = args['outDir'])

if __name__ == '__main__':
    main()