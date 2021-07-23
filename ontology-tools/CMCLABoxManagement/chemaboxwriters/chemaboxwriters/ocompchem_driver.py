from docopt import docopt, DocoptExit
from chemaboxwriters.ontocompchem import write_ocompchem_abox

doc = """aboxwriter
Usage:
    ocompchem <fileOrDir>   [--inp-file-type=<type>]
                            [--qc-log-ext=<ext>]
                            [--out-dir=<dir>]

Options:
--inp-file-type=<type>  Types of the allowed input files
                        to the ocompchem abox writer:
                         - quantum calculation log       [default: qc_log]
                         - quantum calculation json      [qc_json]
                         - ontocompchem meta json        [oc_json]
                         - ontocompchem meta csv         [csv]
--qc-log-ext=<ext>      Extensions of the quantum        [default: .log, .g09]
                        calculation log files
--out-dir=<dir>         Output directory to write the    [default: ./]
                        abox files to
"""

def start():
    try:
        args = docopt(doc)
    except DocoptExit:
        raise DocoptExit('Error: ocompchem called with wrong arguments.')

    write_ocompchem_abox(fileOrDir=args['<fileOrDir>'], inpFileType=args['--inp-file-type'], \
                                    outDir=args['--out-dir'], qcLogExt=args['--qc-log-ext'])
if __name__ == '__main__':
    start()