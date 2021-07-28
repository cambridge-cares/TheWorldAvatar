import os
from chemaboxwriters.common.stageenums import aboxStages
from chemutils.ioutils.ioutils import getFilesWithExtensions, fileExists
from chemaboxwriters.ontocompchem import OC_pipeline

def write_abox(fileOrDir, inpFileType, pipeline=OC_pipeline, qcLogExt=None, outDir=None, outBaseName=None,
                handlerFuncKwargs={}):
    try:
        inStage = aboxStages[inpFileType.upper()]
    except KeyError as e:
        raise KeyError('Error: Wrong --inp-file-type="' + inpFileType+'"') from e

    if qcLogExt is None:
        qcLogExt = ".log,.g09"

    if inStage == aboxStages.QC_LOG:
        fileExt = qcLogExt.split(',')
    elif inStage == aboxStages.CSV or inStage == aboxStages.OWL:
        fileExt = '.oc.'+inStage.name.lower()
    else:
        fileExt = ['.'+inStage.name.lower().replace('_','.')]
    files = getFilesWithExtensions(fileOrDir,fileExt)

    if handlerFuncKwargs:
        for handlerName, funcKwargs in handlerFuncKwargs.items():
            pipeline.handlers[handlerName].set_handler_func_kwargs(funcKwargs)

    outDirNotSet = outDir is None
    outBaseNameNotSet = outBaseName is None
    for file_ in files:
        if outDirNotSet: outDir=os.path.dirname(file_)
        if outBaseNameNotSet:
            if fileExists(file_): outBaseName=os.path.basename(file_)
            else: outBaseName='file'
        outPath = os.path.join(outDir,outBaseName)
        pipeline.execute(file_, inStage, outPath)
    return pipeline