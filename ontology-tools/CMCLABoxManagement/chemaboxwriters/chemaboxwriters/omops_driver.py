from docopt import docopt, DocoptExit
from chemaboxwriters.ontomops import write_abox

doc = """aboxwriter
Usage:
    omops <fileOrDir>  [--inp-file-type=<type>]
                       [--out-dir=<dir>]
                       [--out-base-name=<name>]

Options:
--inp-file-type=<type>  Types of the allowed input files
                        to the omops abox writer:
                         - omops input json file           [default: omops_inp_json]
                         - omops processed json file       [omops_json]
                         - omops meta csv                  [csv]
--out-dir=<dir>         Output directory to write the
                        abox files to. If not provided
                        defaults to the directory of the
                        input file
--out-base-name=<name>  Base name of the produced output
                        files. If not provided, defaults
                        to the input file base name.
"""

def start():
    try:
        args = docopt(doc)
    except DocoptExit:
        raise DocoptExit('Error: ospecies called with wrong arguments.')

    write_abox(fileOrDir=args['<fileOrDir>'], inpFileType=args['--inp-file-type'], \
                         outDir=args['--out-dir'], outBaseName=args['--out-base-name'])
if __name__ == '__main__':
    start()