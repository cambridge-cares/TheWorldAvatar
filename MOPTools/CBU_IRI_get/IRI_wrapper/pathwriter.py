import os
from pathlib import Path

def io_dirs(cbuCSVFilePath):
    
    ## first file in current dir (with full path)
    dir = os.path.dirname(cbuCSVFilePath) ## dir of dir of file
    csvIRI_outNamePath = Path(cbuCSVFilePath).stem
    cbuIRICSVFilePath = dir+"\\CBU_SpeciesIRI"+csvIRI_outNamePath+".csv"
    return(cbuCSVFilePath, cbuIRICSVFilePath)