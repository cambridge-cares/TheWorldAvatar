import os
from pathlib import Path

def io_dirs(cbuCSVFilePath,xyzpathstem,outFilePath=None):  
    dir = os.path.dirname(cbuCSVFilePath) 
    xyzInputCBU = dir+"\\"+xyzpathstem
    json_outNamePath = Path(cbuCSVFilePath).stem
    if outFilePath:
        speciesJSONFilePath = outFilePath
    else:
        speciesJSONFilePath = dir+"\\osJSON_"+json_outNamePath
    try:
        os.makedirs(speciesJSONFilePath)
    except FileExistsError: 
        pass
    return(cbuCSVFilePath, xyzInputCBU, speciesJSONFilePath)