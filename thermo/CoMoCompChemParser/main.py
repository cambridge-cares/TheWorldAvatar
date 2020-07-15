from compchem_parser import species_file_writer
from compchem_parser import Arkane_inpfile_writer
from compchem_parser import Arkane_sys_call

import sys, os, getopt
import traceback
import msvcrt as m

def run_Arkane(filePath):
	species_file_writer(filePath)
	Arkane_inpfile_writer(filePath)
	Arkane_sys_call(filePath.replace(".log","_inp.py"))


def wait():
    m.getch()

def codexit():
    input("Press Enter to continue...")
    sys.exit()

def usage():
    usagemsg = """ Usage:
    -i <input_file>
    """
    print(usagemsg)
    codexit()

def printErrorCode(id, **kwargs):
    if id == 0:
        print(kwargs['file']+' filename must be non blank')
    elif id == 1:
        print('File: "'+ kwargs['file']+'" does not exist')
    codexit()

# Processes the cmd arguments
def main(argv):
    try:
        opts, args = getopt.getopt(argv,"hi:",["help", "inp_file"])
    except getopt.GetoptError:
        usage()
    if not opts:
        usage()
    else:
        inp_file = ''
        for opt, arg in opts:
            if opt in ("-h", "--help"):
                usage()
            elif opt in ("-i", "--inp_file"):
                inp_file = arg
            else:
                print("Unhandled option")
                usage()
        if len(inp_file) == 0: #or 
            # exists
            printErrorCode(0,**{'file':inp_file})
        elif os.path.isfile(inp_file) == False:
            printErrorCode(1, **{'file':inp_file})
        run_Arkane(inp_file)
        print('finished!')

if __name__ == "__main__":
   try:
       main(sys.argv[1:])
       wait()
   except:
       traceback.print_exc()
       wait()