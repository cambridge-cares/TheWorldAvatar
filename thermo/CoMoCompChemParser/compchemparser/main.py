import sys, os
import getopt
import traceback
import compchemparser.helpers.utils as utils
from compchemparser.app import run

def usage():
    usagemsg = """ Usage:
    -h
    -f <log_file>

    """
    print(usagemsg)
    utils.codexit()

# Processes the cmd arguments
def main(argv):
    try:
        opts, args = getopt.getopt(argv,"hf:t:",["help", "log_file"])
    except getopt.GetoptError:
        usage()
    if not opts:
        usage()
    else:
        log_file = ''
        log_type = ''
        for opt, arg in opts:
            if opt in ("-h", "--help"):
                usage()
            elif opt in ("-f", "--log_file"):
                log_file = arg
            else:
                print("Unhandled option")
                usage()

        # check cmd line args for errors
        if len(log_file) == 0:
            utils.dienicely("Log file name must not be blank")
        elif os.path.isfile(log_file) == False:
            print
            utils.dienicely("File: '"+log_file+"' doesn't exist.")
        #if not app.correctLogType(log_type):
        #    utils.dienicely("Unrecognised log file type.")

        # run the code
        run(log_file)
        print('finished!')

if __name__ == "__main__":
   try:
       main(sys.argv[1:])
       utils.wait()
   except:
       traceback.print_exc()
       utils.wait()