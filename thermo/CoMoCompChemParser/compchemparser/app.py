from compchemparser.ontocompchemdata.ontocompchemdata import OntoCompChemData
from compchemparser.aboxwriter import write_abox
from bz2 import __author__

from rdflib import Graph
from pathlib import Path
import random

import os
import errno
import glob

def run(args):

    if os.path.isfile(args['<logFileOrDir>']):
        parseLog(args['<logFileOrDir>'],args['-j'],args['-o'])
    elif os.path.isdir(args['<logFileOrDir>']):
        os.chdir(args['<logFileOrDir>'])

        for logFile in glob.glob(args["--logExt"]):
            parseLog(logFile,args['-j'],args['-o'])
    else:
        raise FileNotFoundError(
            errno.ENOENT, os.strerror(errno.ENOENT), args['<logFileOrDir>'])

def parseLog(logFile,output_json,output_owl):
    CompChemObj = OntoCompChemData(write_abox)
    CompChemObj.getData(logFile)


    #CompChemObj.uploadToKG()
    if CompChemObj.data:
        if output_json:
                CompChemObj.outputjson()
        if output_owl:
                CompChemObj.output_abox_csv()

            # CompChemObj.outputowl(ontocompchem_graph,file_name, r)
    else:
        print('No data to output/upload, check if log file is not empty or quantum job terminated correctly.')