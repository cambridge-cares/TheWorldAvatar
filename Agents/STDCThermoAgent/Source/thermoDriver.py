import sys, os, getopt
import traceback
import chemspecies as chs


def codexit():
    input("Press Enter to continue...")
    sys.exit()

def usage():
    usagemsg = """ Usage:
    thermoDriver.py -x <xml_file> [OPTIONS] or
    thermoDriver.py -j <json_file> [OPTIONS]

    OPTIONS:
    -d <out_datfile> [optional]                    : output dat file to write fitted Nasa polynomials
    -c <out_csvfile> [optional]                    : output csv file to write thermodynamic data over
                                                     defined temeprature range using --tcsv
    --tfit <"TfitLow,TfitMid,TfitHigh"> [optional] : temperatures to use for fitting [optional]
    --tcsv <"T1,T2,...,Tn"> [optional]             : range of temperatures to use when writing
                                                     thermodynamic data to a file
    --href <"Tref,Href"> [optional]                : enthalpy reference point
    """
    print(usagemsg)
    codexit()

def printErrorCode(id, **kwargs):
    if id == 0:
        print(kwargs['file']+' filename must be non blank')
    elif id == 1:
        print('File: "'+ kwargs['file']+'" does not exist')
    codexit()

def main(argv):
    try:
        opts, args = getopt.getopt(argv,"hx:j:d:c:",["help", "xml_file=", "json_file", "out_datfile=", "out_csvfile=", "tfit=", \
            "tcsv=", "href="])
    except getopt.GetoptError:
        usage()
    if not opts:
        usage()
    else:
        xml_file = ''
        json_file = ''
        out_datfile=''
        out_csvfile=''
        tfit_range=[]
        tcsv_range=[]
        use_file = -1
        href=[]
        for opt, arg in opts:
            if opt in ("-h", "--help"):
                usage()
            elif opt in ("-x", "--xml_file"):
                xml_file = arg
                if use_file < 0:  use_file = 1
            elif opt in ("-j", "--json_file"):
                json_file = arg
                if use_file < 0:  use_file = 2
            elif opt in ("-d", "--out_datfile"):
                out_datfile = arg
            elif opt in ("-c", "--out_csvfile"):
                out_csvfile = arg
            elif opt in ("--tfit"):
                tfit_range = [float(a) for a in arg.split(',')]
            elif opt in ("--tcsv"):
                tcsv_range = [float(a) for a in arg.split(',')]
            elif opt in ("--href"):
                href = [float(a) for a in arg.split(',')]
            else:
                print("Unhandled option")
                usage()
        if use_file ==1: 
            inp_file = xml_file
        elif use_file ==2:
            inp_file = json_file
        else:
            inp_file = ''

        if len(inp_file) == 0: #or 
            # exists
            printErrorCode(0,**{'file':inp_file})
        elif os.path.isfile(inp_file) == False:
            printErrorCode(1, **{'file':inp_file})

        chs.RunThermoScriptFromCmd(use_file,inp_file,out_datfile,out_csvfile,tfit_range,tcsv_range,href)

if __name__ == "__main__":
   try:
       main(sys.argv[1:])
   except:
       traceback.print_exc()