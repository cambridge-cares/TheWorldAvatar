from chemaboxwriters.common.stageenums import aboxStages
from chemaboxwriters.common.base import NotSupportedStage
from chemaboxwriters.common.commonvars import CC_LOG_EXT
from chemutils.ioutils.ioutils import getFilesWithExtensions
from entityrdfizer.ABoxTemplateCSVFileToRDF import convert_csv_string_into_rdf
import textwrap

def get_inStage(inpFileType):
    try:
        inStage = aboxStages[inpFileType.upper()]
    except KeyError as e:
        raise NotSupportedStage
    return inStage

def get_file_ext(inStage,fileExtPrefix,qcLogExt=None):
    fileExt=[]
    if inStage == aboxStages.QC_LOG:
        if qcLogExt is None: qcLogExt=CC_LOG_EXT
        fileExt = qcLogExt.split(',')
    elif inStage == aboxStages.CSV or inStage == aboxStages.OWL:
        fileExt = ['.'+fileExtPrefix+'.'+inStage.name.lower()]
    else:
        fileExt = ['.'+inStage.name.lower().replace('_','.')]
    return fileExt

def get_stage_files(fileOrDir,inStage,fileExtPrefix,qcLogExt):

    fileExt = get_file_ext(inStage,fileExtPrefix,qcLogExt)
    files = getFilesWithExtensions(fileOrDir,fileExt)
    if not files:
        raise FileNotFoundError(textwrap.dedent(f"""
            Error: Provided directory or file path is either empty or does not
                   contain the required '{inStage.name.lower()}' files."""))
    return files

def csv2rdf_wrapper(csv_string):
    return [convert_csv_string_into_rdf(csv_string)]