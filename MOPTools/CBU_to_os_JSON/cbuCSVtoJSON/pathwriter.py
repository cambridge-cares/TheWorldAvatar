import os
from pathlib import Path

def io_dirs(cbuCSVFilePath):
    
    ## first file in current dir (with full path)
    dir = os.path.dirname(cbuCSVFilePath) ## dir of dir of file
    xyzInputCBU = dir
    json_outNamePath = Path(cbuCSVFilePath).stem
    speciesJSONFilePath = dir+"\\osJSON_"+json_outNamePath
    #print(newdir)
    try:
        os.makedirs(speciesJSONFilePath)
    except FileExistsError:
        # directory already exists
        pass
    return(cbuCSVFilePath, xyzInputCBU, speciesJSONFilePath)