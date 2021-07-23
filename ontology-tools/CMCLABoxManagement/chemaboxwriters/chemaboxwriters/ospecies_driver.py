from docopt import docopt, DocoptExit
from chemaboxwriters.ontospecies import write_ospecies_abox

doc = """aboxwriter
Usage:
    ospecies <fileOrDir>  [--inp-file-type=<type>]
                          [--qc-log-ext=<ext>]
                          [--out-dir=<dir>]

Options:
--inp-file-type=<type>  Types of the allowed input files
                        to the ospecies abox writer:
                         - quantum calculation log         [default: qc_log]
                         - quantum calculation json        [qc_json]
                         - ontospecies meta json           [os_json]
                         - ontospecies meta csv            [csv]
--qc-log-ext=<ext>      Extensions of the quantum          [default: .log, .g09]
                        calculation log files
--out-dir=<dir>         Output directory to write the      [default: ./]
                        abox files to
"""

def start():
    try:
        args = docopt(doc)
    except DocoptExit:
        raise DocoptExit('Error: ospecies called with wrong arguments.')

    write_ospecies_abox(fileOrDir=args['<fileOrDir>'], inpFileType=args['--inp-file-type'], \
                         outDir=args['--out-dir'], qcLogExt=args['--qc-log-ext'])
if __name__ == '__main__':
    start()