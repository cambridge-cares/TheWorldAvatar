from compchemparser.ontocompchemdata.ontocompchemdata import OntoCompChemData
from bz2 import __author__

from rdflib import Graph
from pathlib import Path
import random

import os
import errno
import glob

def run(args):

    if os.path.isfile(args['<logFileOrDir>']):
        parseLog(args['<logFileOrDir>'])
    elif os.path.isdir(args['<logFileOrDir>']):
        os.chdir(args['<logFileOrDir>'])

        for logFile in glob.glob(args["--logExt"]):
            CompChemObj = OntoCompChemData()
            CompChemObj.getData(logFile)
    else:
        raise FileNotFoundError(
            errno.ENOENT, os.strerror(errno.ENOENT), args['<logFileOrDir>'])

def parseLog(logFile):
    CompChemObj = OntoCompChemData()
    CompChemObj.getData(logFile)


    #CompChemObj.uploadToKG()
    #if output_json:
    #    if file_path:
    #        if CompChemObj.data:
    #            CompChemObj.outputjson(True)
    #    elif CompChemObj.data:
    #         CompChemObj.outputjson(False)
    #         # CompChemObj.outputowl(ontocompchem_graph,file_name, r)
    #    else:
    #        print('No data to output/upload, check if log file is not empty or quantum job terminated correctly.')