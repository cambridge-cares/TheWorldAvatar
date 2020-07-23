import sys,os
import traceback
import compchemparser.helpers.utils as utils
from compchemparser.app import run
import argparse

def main():
    # Process cmd args
    argparser = argparse.ArgumentParser(description='Gaussian log file parser')
    argparser.add_argument('-f', metavar='', help="path to Gaussian log file", required=True)
    args = argparser.parse_args()
    # check cmd line args for errors
    if len(args.f) == 0:
        utils.dienicely("Log file name must not be blank")
    elif os.path.isfile(args.f) == False:
        utils.dienicely("File: '"+args.f+"' doesn't exist.")

    # run the code
    run(args.f)
    print('finished!')

if __name__ == "__main__":
   
   # try to launch the main func, in case of errors the traceback will provide details
   try:
       main()
       utils.wait()
   except:
       traceback.print_exc()
       utils.wait()