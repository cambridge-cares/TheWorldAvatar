from IRI_wrapper.pathwriter import io_dirs
from IRI_wrapper.iri_writer import updateIRIcsv

def cbuIRIOperations(cbuCSVFilePath):
    args = io_dirs(cbuCSVFilePath)
    cbuIRICSVFilePath = args[1]
    updateIRIcsv(cbuCSVFilePath, cbuIRICSVFilePath)
