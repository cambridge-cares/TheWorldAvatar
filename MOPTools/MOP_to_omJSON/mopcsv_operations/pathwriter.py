import os
from pathlib import Path

def io_dirs(cbuIRICSVFilePath, mopCSVFilePath): 
  
    dir = os.path.dirname(cbuIRICSVFilePath) ## dir of dir of file
    cbujson_outNamePath = Path(cbuIRICSVFilePath).stem
    mopjson_outNamePath = Path(mopCSVFilePath).stem
    cbuJSONFilePath = dir+"\\buJSON_"+cbujson_outNamePath+"\\"
    mopJSONFilePath = dir+"\\omJSON_"+mopjson_outNamePath+"\\"
    try:
        os.makedirs(cbuJSONFilePath)
    except FileExistsError:
        # directory already exists
        pass
    try:
        os.makedirs(mopJSONFilePath)
    except FileExistsError:
        # directory already exists
        pass
    return(cbuJSONFilePath, mopJSONFilePath)