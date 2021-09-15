from chemaboxwriters.common.base import NotSupportedStage
from chemaboxwriters.common.stageenums import aboxStages
from chemutils.ioutils.ioutils import fileExists
from chemaboxwriters.ontopesscan import assemble_ops_pipeline
from chemaboxwriters.ontocompchem import assemble_oc_pipeline
from chemaboxwriters.ontocompchem import write_abox as write_oc_abox
from chemaboxwriters.common.commonfunc import get_inStage, get_stage_files
import textwrap
import os

def write_abox(fileOrDir, inpFileType, OPS_pipeline=None,
               OC_pipeline=None,
               qcLogExt=None, outDir=None, outBaseName=None,
               OPS_handlerFuncKwargs={}, OC_handlerFuncKwargs={}):

    try:
        if OPS_pipeline is None: OPS_pipeline = assemble_ops_pipeline()
        if OC_pipeline is None: OC_pipeline = assemble_oc_pipeline()

        inStage = get_inStage(inpFileType)
        if inStage not in OPS_pipeline.supportedStages or inStage==aboxStages.OC_JSON:

            OC_pipeline = write_oc_abox(fileOrDir, inpFileType, qcLogExt=qcLogExt, pipeline=OC_pipeline,
                          outDir=outDir, outBaseName=outBaseName,
                          handlerFuncKwargs=OC_handlerFuncKwargs)
            inpFileType = aboxStages.OC_JSON.name.lower()
            if not os.path.isdir(fileOrDir): fileOrDir = os.path.dirname(fileOrDir)

        inStage = get_inStage(inpFileType)
        files = get_stage_files(fileOrDir, inStage, fileExtPrefix='ops', qcLogExt=qcLogExt)

        if OPS_handlerFuncKwargs:
            for handlerName, funcKwargs in OPS_handlerFuncKwargs.items():
                OPS_pipeline.handlers[handlerName].set_handler_func_kwargs(funcKwargs)

        outDirNotSet = outDir is None
        outBaseNameNotSet = outBaseName is None
        if inStage == aboxStages.OC_JSON:
            if outDirNotSet: outDir=os.path.dirname(files[0])
            if outBaseNameNotSet:
                if fileExists(files[0]): outBaseName=os.path.basename(files[0])
                else: outBaseName='file'
                outPath = os.path.join(outDir,outBaseName)
                OPS_pipeline.execute(files, inStage, outPath)
        else:
            for file_ in files:
                if outDirNotSet: outDir=os.path.dirname(file_)
                if outBaseNameNotSet:
                    if fileExists(file_): outBaseName=os.path.basename(file_)
                    else: outBaseName='file'
                outPath = os.path.join(outDir,outBaseName)
                OPS_pipeline.execute([file_], inStage, outPath)

        OPS_pipeline.writtenFiles+=OC_pipeline.writtenFiles

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
    return OPS_pipeline