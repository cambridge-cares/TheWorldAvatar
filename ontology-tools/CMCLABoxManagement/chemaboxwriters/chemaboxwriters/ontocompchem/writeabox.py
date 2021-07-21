from chemaboxwriters.ontocompchem.pipeline import assemblePipeline
from chemaboxwriters.ontocompchem.stageenums import aboxStages
from chemutils.ioutils.ioutils import getFilesWithExtensions
from compchemparser.helpers.utils import getRefName
import os

def write_ocompchem_abox(fileOrDir, inpFileType, outDir, qcLogExt):
    files = getFilesWithExtensions(fileOrDir,qcLogExt.split(','))
    try:
        inpFileType = aboxStages[inpFileType.upper()]
    except KeyError as e:
        raise KeyError('Error: Wrong --oc-inp-file-type="' + inpFileType+'"') from e

    pipeline = assemblePipeline()
    for file_ in files:
        if outDir is None: outDir=os.path.dirname(file_)
        outBaseName=os.path.basename(file_)
        write_abox(file_, inpFileType, pipeline, outDir, outBaseName)

def write_abox(inFile, inStage, pipeline, outDir, outBaseName):
    output, finalStage = pipeline.execute(inFile, inStage, outDir, outBaseName)