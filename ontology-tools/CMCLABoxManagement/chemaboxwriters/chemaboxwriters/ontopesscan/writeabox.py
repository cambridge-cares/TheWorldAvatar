from chemaboxwriters.common.base import NotSupportedStage
from chemaboxwriters.common.stageenums import aboxStages
from chemutils.ioutils.ioutils import fileExists
from chemaboxwriters.ontopesscan import assemble_ops_pipeline
from chemaboxwriters.common.commonfunc import get_inStage, get_stage_files
import textwrap
import os


def write_abox(fileOrDir, inpFileType, pipeline=None,
               outDir=None, outBaseName=None,
               handlerFuncKwargs={}):
    try:
        if pipeline is None: pipeline = assemble_ops_pipeline()
        inStage = get_inStage(inpFileType)
        files = get_stage_files(fileOrDir, inStage, qcLogExt='')

        if handlerFuncKwargs:
            pipeline.set_handler_func_kwargs(handlerFuncKwargs)

        pipeline.run(files, inStage, outDir)

        #outDirNotSet = outDir is None
        #outBaseNameNotSet = outBaseName is None
        #for file_ in files:
        #    if outDirNotSet: outDir=os.path.dirname(file_)
        #    if outBaseNameNotSet:
        #        if fileExists(file_): outBaseName=os.path.basename(file_)
        #        else: outBaseName='file'
        #    outPath = os.path.join(outDir,outBaseName)
        #    pipeline.run([file_], inStage, outPath)

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