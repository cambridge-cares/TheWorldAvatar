from compchemparser.ontocompchemdata.ontocompchemdata import OntoCompChemData
from compchemparser.aboxwriters import write_abox
from bz2 import __author__

from rdflib import Graph
from pathlib import Path
import random

import os
import errno
import glob

def run(args):

    if os.path.isfile(args['<logFileOrDir>']):
        parseLog(args['<logFileOrDir>'],args['-n'])

    elif os.path.isdir(args['<logFileOrDir>']):
        os.chdir(args['<logFileOrDir>'])

        for logFile in glob.glob(args["--logExt"]):
            parseLog(logFile,args['-n'])
    else:
        raise FileNotFoundError(
            errno.ENOENT, os.strerror(errno.ENOENT), args['<logFileOrDir>'])

def parseLog(logFile,suppressOutput):
    CompChemObj = OntoCompChemData(write_abox)
    CompChemObj.getData(logFile)

    if CompChemObj:
        if not suppressOutput:
            CompChemObj.outputjson()
            CompChemObj.output_abox_csv()
    else:
        print('No data to output/upload, check if log file is not empty or quantum job terminated correctly.')