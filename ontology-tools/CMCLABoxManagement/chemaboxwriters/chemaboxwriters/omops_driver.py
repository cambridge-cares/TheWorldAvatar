from docopt import docopt, DocoptExit
from chemaboxwriters.ontomops import write_abox
import json

doc = """aboxwriter
Usage:
    omops <fileOrDir>  [--inp-file-type=<type>]
                       [--out-dir=<dir>]
                       [--out-base-name=<name>]
                       [--handlers-args=<handlargs>]

Options:
--inp-file-type=<type>       Types of the allowed input files
                             to the omops abox writer:
                              - omops input json file           [default: ominp_json]
                              - omops processed json file       [omops_json]
                              - omops meta csv                  [csv]
--out-dir=<dir>              Output directory to write the
                             abox files to. If not provided
                             defaults to the directory of the
                             input file
--out-base-name=<name>       Base name of the produced output
                             files. If not provided, defaults
                             to the input file base name.
--handlers-args=<handlargs>  Any supported stage handlers'         [default: {}]
                             arguments. The arguments are passed
                             as a json formatted string, where
                             the first level keys are handlers
                             names followed by their arguments
                             names and values, e.g:
                             {\\"OMINP_JSON_TO_OM_JSON\\": {\\"random_id\\": \\"123\\"}}
                             with all double quotes escaped.
"""

def start():
    try:
        args = docopt(doc)
    except DocoptExit:
        raise DocoptExit('Error: ospecies called with wrong arguments.')

    try:
        handlerFuncKwargs = json.loads(args['--handlers-args'])
    except json.decoder.JSONDecodeError:
        print('Error: Wrong --handlers-args input.')
        return

    write_abox(
                fileOrDir=args['<fileOrDir>'],
                inpFileType=args['--inp-file-type'],
                outDir=args['--out-dir'],
                outBaseName=args['--out-base-name'],
                handlerFuncKwargs=handlerFuncKwargs
                )
if __name__ == '__main__':
    start()