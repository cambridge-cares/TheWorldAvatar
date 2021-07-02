from docopt import docopt, DocoptExit
import  os
import errno
import glob
from entityrdfizer import convertDir, convertFile

__doc__ = """entrdifizer

Usage:
    entrdifizer <fileOrDirPath> [--outDir=<OUT_DIR>]

Options:
--outDir=<OUT_DIR>   Output directory path
"""

def main():
    try:
        args = docopt(__doc__)
    except DocoptExit:
        raise DocoptExit('Error: parser called with wrong arguments.')

    if os.path.isfile(args['<fileOrDirPath>']):
        convertFile(args['<fileOrDirPath>'],args['--outDir'])
    elif os.path.isdir(args['<fileOrDirPath>']):
        convertDir(args['<fileOrDirPath>'],args['--outDir'])
    else:
        raise FileNotFoundError(
            errno.ENOENT, os.strerror(errno.ENOENT), args['<fileOrDirPath>'])

if __name__ == '__main__':
    main()