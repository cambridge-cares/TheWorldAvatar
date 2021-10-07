from cbuCSVtoJSON.pathwriter import io_dirs
from cbuCSVtoJSON.converter import cbuCSVtoJSON

def cbuOperations(cbuCSVFilePath):
    #print(cbuCSVFilePath)
    args = io_dirs(cbuCSVFilePath)
    xyzInputCBU = args[1]
    speciesJSONFilePath = args[2]
    cbuCSVtoJSON(cbuCSVFilePath, xyzInputCBU, speciesJSONFilePath)
    
#cbuCSVFilePath='C:\\Users\\ak2332\\Documents\\SandboxPrograms\\CBUtestdir\\CBUs_InChi.csv'
#cbuOperations(cbuCSVFilePath)