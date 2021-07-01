from compchemparser.app import run as runParser
from docopt import docopt, DocoptExit

__doc__ = """ccparse

Usage:
    ccparse parse <logFileOrDir> [-n --logExt=<LOG_EXT>]
    ccparse scan <scanJsonDir> --osid=<ONTOSPEC_ID> --osc=<ONTOSPEC_SCAN_COORD> --occ=<ONTOCOMP_SCAN_COORD>

Options:
    -n                              Suppress parser command output files (json and csv)
    --logExt=<LOG_EXT>              Log files file extension for log file directory input [default: *.log]
    --osid=<ONTOSPEC_ID>            OntoSpecies id (iri or inchi)
    --osc=<ONTOSPEC_SCAN_COORD>     Scan coordinate w.r.t OntoSpecies atoms order (e.g. "1,4,5")
    --occ=<ONTOCOMP_SCAN_COORD>     Scan coordinate w.r.t OntoCompChem atoms order (e.g. "3,6,1")
"""

def main():
    try:
        args = docopt(__doc__)
    except DocoptExit:
        raise DocoptExit('Error: parser called with wrong arguments.')


    if args['parse']:
        runParser(args)
    elif args['scan']:
        print("scan command under construction")

if __name__ == '__main__':
    main()



#def main():
#    # Process cmd args
#    argparser = argparse.ArgumentParser(description='Gaussian log file parser')
#    argparser.add_argument('-f', metavar='', help="path to Gaussian log file", required=True)
#    argparser.add_argument('-j', metavar='', help="generates JSON file(s)", default =False,required=False)
#    argparser.add_argument('-p', metavar='', help="specify output file path", default =False,required=False)
#    args = argparser.parse_args()
#    # check cmd line args for errors
#    if len(args.f) == 0:
#        utils.dienicely("Log file name must not be blank")
#    elif os.path.isfile(args.f) == False:
#        utils.dienicely("File: '"+args.f+"' doesn't exist.")
#
#    # run the code
#    run(args.f,args.j,args.p)
#    print('finished!')
#
# if __name__ == "__main__":

#    # try to launch the main func, in case of errors the traceback will provide details
#    try:
#        main()
#        utils.wait()
#    except:
#        traceback.print_exc()
#        utils.wait()