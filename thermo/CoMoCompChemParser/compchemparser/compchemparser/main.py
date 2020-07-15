from compchem_parser import run_Arkane
import sys, os, getopt
import traceback

GAUSSIAN = 1
MOLPRO = 2

def run(inp_file,inp_type):
    run_Arkane(inp_file,inp_type)

def wait():
	pass
#    m.getch()

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
        opts, args = getopt.getopt(argv,"hi:l:",["help", "inp_file", "inp_type"])
    except getopt.GetoptError:
        usage()
    if not opts:
        usage()
    else:
        inp_file = ''
        inp_type = GAUSSIAN
        for opt, arg in opts:
            if opt in ("-h", "--help"):
                usage()
            elif opt in ("-i", "--inp_file"):
                inp_file = arg
            elif opt in ("-l", "--inp_type"):
                inp_type = arg
            else:
                print("Unhandled option")
                usage()
        if len(inp_file) == 0: #or 
            # exists
            printErrorCode(0,**{'file':inp_file})
        elif os.path.isfile(inp_file) == False:
            printErrorCode(1, **{'file':inp_file})

        run(inp_file,inp_type)
        print('finished!')

if __name__ == "__main__":
   try:
       main(sys.argv[1:])
       wait()
   except:
       traceback.print_exc()
       wait()