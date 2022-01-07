from chemaboxwriters.common.stageenums import aboxStages
from chemaboxwriters.common.commonvars import CC_LOG_EXT
from entityrdfizer.aboxgenerator.ABoxTemplateCSVFileToRDF import convert_csv_string_into_rdf
import os
import glob
from enum import Enum
from typing import List, Optional

def get_stage_files(
    fileOrDir: str,
    inStage: Enum,
    qcLogExt: Optional[str] = None
    )->List[str]:

    fileExt = get_file_extensions_by_stage(inStage, qcLogExt)
    files = get_files_by_extensions(fileOrDir, fileExt)
    return files

def get_file_extensions_by_stage(
    inStage: Enum,
    qcLogExt: Optional[str] = None
    )->List[str]:

    fileExt=[]
    if inStage == aboxStages.QC_LOG:
        if qcLogExt is None: qcLogExt=CC_LOG_EXT
        fileExt = qcLogExt.split(',')
    else:
        fileExt = ['.'+inStage.name.lower().replace('_','.')]
    return fileExt

def get_files_by_extensions(
    fileOrDir: str,
    fileExtList: List[str]
    )->List[str]:

    files = []
    if os.path.isfile(fileOrDir):
        files = [fileOrDir]
    elif os.path.isdir(fileOrDir):
        for fileExt in fileExtList:
            files+=glob.glob(os.path.join(fileOrDir,'*'+fileExt))
    return files

def readFile(file_path:str)->str:
    with open(file_path, "r") as file_handle:
        file_content=file_handle.read()
    return file_content

def csv2rdf_wrapper(file_path:str)->List[str]:
    csv_string = readFile(file_path)
    return [convert_csv_string_into_rdf(csv_string)]

def getRefName(
    filepath: str,
    jobIndex: int,
    numJobs: int,
    extension: str
    )->str:

    if numJobs > 1:
        refName = filepath + '_' + str(jobIndex+1)+extension
    else:
        refName = filepath + extension
    return refName