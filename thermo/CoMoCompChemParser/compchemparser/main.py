import sys, os
import traceback
import compchemparser.helpers.utils as utils
from compchemparser.app import run
import argparse

# Processes the cmd arguments
def main(args):
    # check cmd line args for errors
    if len(args.f) == 0:
        utils.dienicely("Log file name must not be blank")
    elif os.path.isfile(args.f) == False:
        print
        utils.dienicely("File: '"+args.f+"' doesn't exist.")
    #if not app.correctLogType(log_type):
    #    utils.dienicely("Unrecognised log file type.")

    # run the code
    run(args.f)
    print('finished!')

if __name__ == "__main__":
   argparser = argparse.ArgumentParser(description='Gaussian log file parser')
   argparser.add_argument('-f', metavar='', help="path to Gaussian log file", required=True)
   args = argparser.parse_args()
   try:
       main(args)
       utils.wait()
   except:
       traceback.print_exc()
       utils.wait()