import sys, os
import getopt
import traceback
import helpers.utils as utils
import app

def usage():
    usagemsg = """ Usage:
    -f <log_file> -t <log_type>

    """
    print(usagemsg)
    utils.codexit()

# Processes the cmd arguments
def main(argv):
    try:
        opts, args = getopt.getopt(argv,"hf:t:",["help", "log_file", "log_type"])
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
            elif opt in ("-t", "--log_type"):
                log_type = arg
            else:
                print("Unhandled option")
                usage()

        # check cmd line args for errors
        if len(log_file) == 0:
            utils.dienicely("Log file name must not be blank")
        elif os.path.isfile(os.path.join(log_file)) == False:
            utils.dienicely("File: '"+log_file+"' doesn't exist.")
        if not app.correctLogType(log_type):
            utils.dienicely("Unrecognised log file type.")

        # run the code
        app.run(log_file,log_type)
        print('finished!')

if __name__ == "__main__":
   try:
       main(sys.argv[1:])
       utils.wait()
   except:
       traceback.print_exc()
       utils.wait()