import os
from chemaboxwriters.common.stageenums import aboxStages
from chemutils.ioutils.ioutils import getFilesWithExtensions, fileExists
from chemaboxwriters.ontopesscan import OPS_pipeline

def write_abox(fileOrDir, inpFileType, pipeline=OPS_pipeline, outDir=None, outBaseName=None,
                handlerFuncKwargs={}):
    try:
        inStage = aboxStages[inpFileType.upper()]
    except KeyError as e:
        raise KeyError('Error: Wrong --inp-file-type="' + inpFileType+'"') from e


    fileExt = ['.'+inStage.name.lower().replace('_','.')]
    files = getFilesWithExtensions(fileOrDir,fileExt)

    if handlerFuncKwargs:
        for handlerName, funcKwargs in handlerFuncKwargs.items():
            pipeline.handlers[handlerName].set_handler_func_kwargs(funcKwargs)

    outDirNotSet = outDir is None
    outBaseNameNotSet = outBaseName is None
    if inStage == aboxStages.OC_JSON:
        if outDirNotSet: outDir=os.path.dirname(files[0])
        if outBaseNameNotSet:
            if fileExists(files[0]): outBaseName=os.path.basename(files[0])
            else: outBaseName='file'
            outPath = os.path.join(outDir,outBaseName)
            pipeline.execute(files, inStage, outPath)
    else:
        for file_ in files:
            if outDirNotSet: outDir=os.path.dirname(file_)
            if outBaseNameNotSet:
                if fileExists(file_): outBaseName=os.path.basename(file_)
                else: outBaseName='file'
            outPath = os.path.join(outDir,outBaseName)
            pipeline.execute(file_, inStage, outPath)
    return pipeline