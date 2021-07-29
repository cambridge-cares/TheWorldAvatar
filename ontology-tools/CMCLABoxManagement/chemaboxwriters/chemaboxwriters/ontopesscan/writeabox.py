from chemaboxwriters.ontocompchem.pipeline import OC_pipeline
from chemaboxwriters.common.base import NotSupportedStage
from chemaboxwriters.common.stageenums import aboxStages
from chemutils.ioutils.ioutils import getFilesWithExtensions, fileExists
from chemaboxwriters.ontopesscan import OPS_pipeline
from chemaboxwriters.ontocompchem import write_abox as write_oc_abox
from chemaboxwriters.common.commonfunc import get_inStage, get_stage_files
import textwrap
import os

def write_abox(fileOrDir, inpFileType, pipeline=OPS_pipeline,
               qcLogExt=".log,.g03,.g09,.g16", outDir=None, outBaseName=None,
               OPS_handlerFuncKwargs={}, OC_handlerFuncKwargs={}):

    try:
        inStage = get_inStage(inpFileType)
        if inStage not in OPS_pipeline.supportedStages:

            write_oc_abox(fileOrDir, inpFileType, qcLogExt=qcLogExt, outDir=outDir, outBaseName=outBaseName,
                        handlerFuncKwargs=OC_handlerFuncKwargs)
            inpFileType = aboxStages.OC_JSON.name.lower()

        inStage = get_inStage(inpFileType)
        files = get_stage_files(fileOrDir, inStage, fileExtPrefix='ops', qcLogExt=qcLogExt)

        if OPS_handlerFuncKwargs:
            for handlerName, funcKwargs in OPS_handlerFuncKwargs.items():
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

    except NotSupportedStage:
        supportedOCStagesNames = [stage.name.lower() for stage in OC_pipeline.supportedStages]
        supportedOPSStagesNames = [stage.name.lower() for stage in OPS_pipeline.supportedStages]
        print(textwrap.dedent(f"""
            Error: The requested --inp-file-type='{inpFileType}'
                   is not supported by the current pipeline.
                   Please choose one of the following stages:
                   {list(set().union(supportedOCStagesNames, supportedOPSStagesNames))}"""))
    except FileNotFoundError:
        print(textwrap.dedent(f"""
            Error: Provided directory or file path is either empty or does not
                   contain the required '{inpFileType}' files."""))
    return pipeline