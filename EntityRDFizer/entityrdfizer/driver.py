from docopt import docopt, DocoptExit
import  os
import errno
import glob
from entityrdfizer import convertDir, convertFile

__doc__ = """csv2rdf

Usage:
    csv2rdf <csvFileOrDirPath> [--outDir=<OUT_DIR>]

Options:
--outDir=<OUT_DIR>   Output directory path
"""

def main():
    try:
        args = docopt(__doc__)
    except DocoptExit:
        raise DocoptExit('Error: parser called with wrong arguments.')

    if os.path.isfile(args['<csvFileOrDirPath>']):
        convertFile(args['<csvFileOrDirPath>'],args['--outDir'])
    elif os.path.isdir(args['<csvFileOrDirPath>']):
        convertDir(args['<csvFileOrDirPath>'],args['--outDir'])
    else:
        raise FileNotFoundError(
            errno.ENOENT, os.strerror(errno.ENOENT), args['<csvFileOrDirPath>'])

if __name__ == '__main__':
    main()