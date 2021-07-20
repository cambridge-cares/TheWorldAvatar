from chemaboxwriters.ontocompchem.pipeline import assemblePipeline
from chemaboxwriters.ontocompchem.stageenums import aboxStages
from chemutils.ioutils.ioutils import getFilesWithExtensions

def write_ocompchem_abox(fileOrDir, inpFileType, outDir, qcLogExt):
    files = getFilesWithExtensions(fileOrDir,qcLogExt.split(','))
    try:
        inpFileType = aboxStages[inpFileType.upper()]
    except KeyError as e:
        raise KeyError('Error: Wrong --oc-inp-file-type="' + inpFileType+'"') from e

    pipeline = assemblePipeline()
    for file_ in files:
        write_abox(file_, inpFileType, pipeline)

def write_abox(inFile, inStage, pipeline):
    output, finalStage = pipeline.execute(inFile, inStage.upper())