from chemaboxwriters.common.stageenums import aboxStages
from chemaboxwriters.common.base import NotSupportedStage
from chemutils.ioutils.ioutils import getFilesWithExtensions
import textwrap

def get_inStage(inpFileType):
    try:
        inStage = aboxStages[inpFileType.upper()]
    except KeyError as e:
        raise NotSupportedStage
    return inStage

def get_stage_files(fileOrDir,inStage,fileExtPrefix,qcLogExt):

    if inStage == aboxStages.QC_LOG:
        fileExt = qcLogExt.split(',')
    elif inStage == aboxStages.CSV or inStage == aboxStages.OWL:
        fileExt = '.'+fileExtPrefix+'.'+inStage.name.lower()
    else:
        fileExt = ['.'+inStage.name.lower().replace('_','.')]

    files = getFilesWithExtensions(fileOrDir,fileExt)
    if not files:
        raise FileNotFoundError(textwrap.dedent(f"""
            Error: Provided directory or file path is either empty or does not
                   contain the required '{inStage.name.lower()}' files."""))
    return files