import os
from pathlib import Path

def io_dirs(cbuCSVFilePath):  
    dir = os.path.dirname(cbuCSVFilePath) 
    xyzInputCBU = dir
    json_outNamePath = Path(cbuCSVFilePath).stem
    speciesJSONFilePath = dir+"\\osJSON_"+json_outNamePath
    try:
        os.makedirs(speciesJSONFilePath)
    except FileExistsError: 
        pass
    return(cbuCSVFilePath, xyzInputCBU, speciesJSONFilePath)