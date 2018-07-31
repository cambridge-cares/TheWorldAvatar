import sys, os, getopt
import traceback
import chemspecies as chs
import jsonparser
import jsonwriter
import xmlparser
import datparser
import nasafitter
import nasawriter
import csvwriter
import msvcrt as m

# Controls the program flow
def RunThermoScriptFromCmd(use_file,inp_file,spec_name,out_datfile,out_csvfile,tfit_range,tcsv_range,href):
    # Read species data from selected file format
    if use_file == 1:
        Sp = xmlparser.readSpecXML(inp_file)
        if len(out_datfile.strip())==0:
            out_datfile = inp_file.replace('.xml','.dat')
    elif use_file == 2:
        Sp = jsonparser.readSpecJSON(inp_file)
        if len(out_datfile.strip())==0:
            out_datfile = inp_file.replace('.json','_nasa.json')
    else:
        Sp_list = datparser.readChemSpeciesFile(inp_file,spName=spec_name)
        Sp = Sp_list[0]
        out_datfile = inp_file.replace('.dat','_nasa.dat')

    # Check if enthalpy reference point is provided
    if len(href)>0:
        Sp.EnthRefTemp = href[0]
        Sp.EnthRef = href[1]
        Sp.EnthRefCorr = Sp.getSpHcorrSTHD()

    # Check if temperature ranges for fitting and output
    # are provided
    if len(tfit_range)!=0:
        Sp.FitTrangeNasa = tfit_range
    if len(tcsv_range)!=0:
        Sp.ToutCsv = tcsv_range

    # Fit and write nasa polynomials (all file formats)
    Sp = nasafitter.fitThermoDataNASA(Sp,tfit_range)

    if use_file == 2:
        # Write json file with calcualted data (only json file format)
        jsonwriter.writeJsonOutFile(Sp,out_datfile)
    else:
        # Write Nasa .dat file (xml and dat file formats)
        nasawriter.writeNasaDatFile(Sp,out_datfile)

    if len(out_csvfile.strip())!=0:
        # write csv file with calculated thermodynamic properties
        # compared to properties from existing Nasa polynomials 
        # (if provided) and with fitted Nasa polynomials
        inclNasaFlag = False
        if (Sp.LowNasa): inclNasaFlag=True
        csvwriter.writeThermoDatCsvFile(Sp,out_csvfile,T=Sp.ToutCsv,inclFitNasa=True,inclNasa=inclNasaFlag)

def wait():
    m.getch()

def codexit():
    input("Press Enter to continue...")
    sys.exit()

def usage():
    #hidden usage:  -m  <mol_file> -s SPECNAME read from species molecular data file
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

# Processes the cmd arguments
def main(argv):
    try:
        opts, args = getopt.getopt(argv,"hx:j:m:s:d:c:",["help", "xml_file=", "json_file", "mol_file", "spec_name", "out_datfile=", "out_csvfile=", "tfit=", \
            "tcsv=", "href="])
    except getopt.GetoptError:
        usage()
    if not opts:
        usage()
    else:
        xml_file = ''
        json_file = ''
        mol_file = ''
        out_datfile =''
        out_csvfile =''
        spec_name =''
        tfit_range =[]
        tcsv_range =[]
        use_file = -1
        href =[]
        for opt, arg in opts:
            if opt in ("-h", "--help"):
                usage()
            elif opt in ("-x", "--xml_file"):
                xml_file = arg
                if use_file < 0:  use_file = 1
            elif opt in ("-j", "--json_file"):
                json_file = arg
                if use_file < 0:  use_file = 2
            elif opt in ("-m", "--mol_file"):
                mol_file = arg
                if use_file < 0:  use_file = 3
            elif opt in ("-s", "--spec_name"):
                spec_name = arg
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
        elif use_file ==3:
            inp_file = mol_file
        else:
            inp_file = ''

        if len(inp_file) == 0: #or 
            # exists
            printErrorCode(0,**{'file':inp_file})
        elif os.path.isfile(inp_file) == False:
            printErrorCode(1, **{'file':inp_file})

        RunThermoScriptFromCmd(use_file,inp_file,spec_name,out_datfile,out_csvfile,tfit_range,tcsv_range,href)

if __name__ == "__main__":
   try:
       main(sys.argv[1:])
   except:
       traceback.print_exc()
       wait()