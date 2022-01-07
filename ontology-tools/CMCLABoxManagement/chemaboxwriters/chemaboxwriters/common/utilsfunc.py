import chemaboxwriters.common.globals as globals
from chemaboxwriters.app_exceptions.app_exceptions import UnsupportedStage
from entityrdfizer.aboxgenerator.ABoxTemplateCSVFileToRDF import convert_csv_string_into_rdf
import os
import glob
from enum import Enum
from typing import List, Optional
import uuid
import logging

def config_logging(
    log_file_dir: Optional[str],
    log_file_name: str,
    no_file_logging: bool) -> None:

    if log_file_dir is None: log_file_dir = os.getcwd()
    log_file = os.path.join(log_file_dir, log_file_name)

    logHandlers= []
    logHandlers.append(logging.StreamHandler())
    if not no_file_logging:
        logHandlers.append(logging.FileHandler(filename=log_file, mode='w'))

    logging.basicConfig(level=logging.DEBUG,
                        format='%(asctime)s [%(threadName)s] [%(levelname)s] %(message)s',
                        handlers=logHandlers)

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
    if inStage == globals.aboxStages.QC_LOG:
        if qcLogExt is None: qcLogExt=globals.CC_LOG_EXT
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

def fileExists(path: str)->bool:
    return os.path.isfile(os.path.abspath(path))

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

def get_random_id():
    return str(uuid.uuid4()) #Get a randomly generated identifier for creation of the ABox.

def stage_name_to_enum(
    inpFileType: str
    )->Enum:
    try:
        inStage = globals.aboxStages[inpFileType.upper()]
    except KeyError:
        raise UnsupportedStage
    return inStage