from compchemparser.app import runParser
from docopt import docopt, DocoptExit

__doc__ = """ccparse

Usage:
    ccparse <logFileOrDir> [-n --logExt=<LOG_EXT>]

Options:
    -n                              Suppress parser command output files (json and csv)
    --logExt=<LOG_EXT>              Log files file extension for log file directory input [default: .log]
"""

def main():
    try:
        args = docopt(__doc__)
    except DocoptExit:
        raise DocoptExit('Error: parser called with wrong arguments.')

    runParser(args['<logFileOrDir>'],args["--logExt"], args['-n'])

if __name__ == '__main__':
    main()