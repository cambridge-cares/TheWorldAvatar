from entityrdfizer.aboxgenerator.ABoxTemplateCSVFilesToRDF import convert as convertDir
from entityrdfizer.aboxgenerator.ABoxTemplateCSVFileToRDF import convert_into_rdf as convertFile
import os


def run_abox_generator(
    csvFileOrDirPath,
    outDir,
    csvTbox
):
    if os.path.isfile(csvFileOrDirPath):
        convertFile(csvFileOrDirPath,outDir,csvTbox)
    elif os.path.isdir(csvFileOrDirPath):
        convertDir(csvFileOrDirPath,outDir,csvTbox)
    else:
        print(f"File or directory {csvFileOrDirPath} does not exists")