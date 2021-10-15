from CBU_to_os_JSON.cbuCSVtoJSON.pathwriter import io_dirs
from CBU_to_os_JSON.cbuCSVtoJSON.converter import cbuCSVtoQCJSON

def cbuOperations(cbuCSVFilePath,xyzpathstem,outFilePath=None):
    """This function allocates input/ouput paths and submits data for conversion."""
    if outFilePath:
        args = io_dirs(cbuCSVFilePath,xyzpathstem,outFilePath)
    else:
        args = io_dirs(cbuCSVFilePath,xyzpathstem)
    xyzInputCBU = args[1] # Allocates the folder with the xyz files
    speciesJSONFilePath = args[2] # Allocates output path folder 
    cbuCSVtoQCJSON(cbuCSVFilePath, xyzInputCBU, speciesJSONFilePath)
    