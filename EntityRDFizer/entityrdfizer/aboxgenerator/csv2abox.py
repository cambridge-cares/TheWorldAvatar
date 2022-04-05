from entityrdfizer.aboxgenerator.ABoxTemplateCSVFilesToRDF import convert as convertDir
from entityrdfizer.aboxgenerator.ABoxTemplateCSVFileToRDF import convert_into_rdf as convertFile
import os


def run_abox_generator(
    csvFileOrDirPath,
    outDir
):
    if os.path.isfile(csvFileOrDirPath):
        convertFile(csvFileOrDirPath,outDir)
    elif os.path.isdir(csvFileOrDirPath):
        convertDir(csvFileOrDirPath,outDir)
    else:
        print(f"File or directory {csvFileOrDirPath} does not exists")