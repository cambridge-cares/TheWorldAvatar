from chemaboxwriters.common.base import NotSupportedStage
from chemutils.ioutils.ioutils import fileExists
from chemaboxwriters.ontomops.pipeline import assemble_omops_pipeline
from chemaboxwriters.common.commonfunc import get_inStage, get_stage_files
import textwrap
import os

def write_abox(fileOrDir, inpFileType, pipeline=None,
               outDir=None, outBaseName=None,
               handlerFuncKwargs={}):
    try:
        if pipeline is None: pipeline = assemble_omops_pipeline()
        inStage = get_inStage(inpFileType)
        files = get_stage_files(fileOrDir, inStage, fileExtPrefix='om', qcLogExt='')

        if handlerFuncKwargs:
            pipeline.set_stage_func_kwargs(handlerFuncKwargs)

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