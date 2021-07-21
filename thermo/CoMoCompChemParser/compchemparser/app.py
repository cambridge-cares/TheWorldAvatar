import os
from compchemparser.helpers.ccutils import CCPACKAGES, get_ccpackage
from compchemparser.helpers.utils import qc_log_to_json, getFilesWithExtensions, dienicely
from compchemparser.parsers.ccgaussian_parser import CcGaussianParser

def runParser(logFileOrDir, logExt='.log', suppressOutput=False):
    all_parsed_data = []
    files = getFilesWithExtensions(logFileOrDir,logExt.split(','))
    for file_ in files:
        parsed_data= parseLog(file_)
        all_parsed_data+=parsed_data

        if not suppressOutput:
            outDir=os.path.dirname(file_)
            baseName=os.path.basename(file_)
            qc_log_to_json(outDir=outDir, outFileBaseName=baseName, parsedJobsList=parsed_data)
    return parsed_data

def parseLog(logFile):
    # use cclib package "get_ccattr" utility to determine the log file type
    ccpackage= get_ccpackage(logFile)
    # at the moment only Gaussian log files are supported
    if ccpackage in CCPACKAGES:
        # set the parser
        parser = CcGaussianParser()
    else:
        dienicely("ERROR: Provided log fie is either incorrect or comes from an unsupported quantum chemistry package.")
    return parser.parse(logFile)