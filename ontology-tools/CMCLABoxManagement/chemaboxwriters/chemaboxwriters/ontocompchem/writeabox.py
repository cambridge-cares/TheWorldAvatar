from chemaboxwriters.common.base import NotSupportedStage
from chemaboxwriters.ontocompchem.pipeline import assemble_oc_pipeline
from chemaboxwriters.common.commonfunc import get_inStage, get_stage_files
from chemaboxwriters.common.commonvars import CC_LOG_EXT
import os
import textwrap

def write_abox(fileOrDir, inpFileType, pipeline=None,
               qcLogExt=None, outDir=None,
               handlerFuncKwargs={}):
    try:
        if qcLogExt is None: qcLogExt = CC_LOG_EXT
        if pipeline is None: pipeline = assemble_oc_pipeline()
        inStage = get_inStage(inpFileType)
        files = get_stage_files(fileOrDir, inStage, qcLogExt=qcLogExt)

        if handlerFuncKwargs:
            pipeline.set_func_kwargs(handlerFuncKwargs)

        pipeline.run(files, inStage, outDir)

    except NotSupportedStage:
        supportedStagesNames = [stage.name.lower() for stage in pipeline.inStages]
        print(textwrap.dedent(f"""
            Error: The requested --inp-file-type='{inpFileType}'
                   is not supported by the current pipeline.
                   Please choose one of the following options:
                   {supportedStagesNames}"""))
    except FileNotFoundError:
        print(textwrap.dedent(f"""
            Error: Provided directory or file path is either empty or does not
                   contain the required '{inpFileType}' files."""))
    return pipeline