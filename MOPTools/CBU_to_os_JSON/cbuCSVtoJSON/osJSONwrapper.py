from cbuCSVtoJSON.pathwriter import io_dirs
from cbuCSVtoJSON.converter import cbuCSVtoJSON

def cbuOperations(cbuCSVFilePath):
    """This function allocates input/ouput paths and submits data for conversion."""
    args = io_dirs(cbuCSVFilePath) 
    xyzInputCBU = args[1] # Allocates the folder with the xyz files
    speciesJSONFilePath = args[2] # Allocates output path folder 
    cbuCSVtoJSON(cbuCSVFilePath, xyzInputCBU, speciesJSONFilePath)
    