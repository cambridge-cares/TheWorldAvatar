from chemaboxwriters.common.base import get_handler, StageHandler
from compchemparser.helpers.utils import jsonStringToFile
from chemaboxwriters.common.stageenums import aboxStages
from compchemparser.app import parseLog
from chemaboxwriters.common.commonfunc import csv2rdf_wrapper
import chemutils.ioutils.ioutils as ioutils
from chemaboxwriters.common.commonvars import CC_LOG_EXT

from enum import Enum
from typing import Optional, List, Dict, Any, Callable

def get_qc_log_to_qc_json_handler(
    name: Optional[str] = None,
    fileExt: Optional[str] = None,
    unroll_input: bool = True,
    *args,
    **kwargs
    )->StageHandler:

    if name is None: name = 'QC_LOG_TO_QC_JSON'
    if fileExt is None: fileExt = '.qc.json'

    QC_LOG_TO_QC_JSON = get_handler(
                                name=name,
                                inStages=[aboxStages.QC_LOG],
                                outStage=aboxStages.QC_JSON,
                                handlerFunc=parseLog,
                                fileWriter=jsonStringToFile,
                                fileExt=fileExt,
                                unroll_input = unroll_input,
                                *args,
                                **kwargs
                             )
    return QC_LOG_TO_QC_JSON

def get_json_to_csv_handler(
    inStages: List[Enum],
    outStage: Enum,
    handlerFunc: Callable,
    fileWriter: Optional[Callable] = None,
    fileWriterKwargs: Optional[Dict[str,Any]] = None,
    name: Optional[str] = None,
    fileExt: Optional[str] = None,
    unroll_input: bool = True,
    *args,
    **kwargs
    )->StageHandler:

    if name is None: name = 'JSON_TO_CSV'
    if fileExt is None: fileExt = '.csv'
    if fileWriter is None:
        fileWriter = ioutils.writeFile
        if fileWriterKwargs is None:
            fileWriterKwargs={'newline':''}
        else:
            fileWriterKwargs['newline'] = ''

    JSON_TO_CSV = get_handler(
                                name= name,
                                inStages=inStages,
                                outStage=outStage,
                                handlerFunc=handlerFunc,
                                fileWriter=fileWriter,
                                fileWriterKwargs=fileWriterKwargs,
                                fileExt=fileExt,
                                unroll_input = unroll_input,
                                *args,
                                **kwargs
                            )
    return JSON_TO_CSV

def get_csv_to_owl_handler(
    inStages: List[Enum],
    outStage: Enum,
    name: Optional[str] = None,
    fileExt: Optional[str] = None,
    unroll_input: bool = True,
    *args,
    **kwargs
    )->StageHandler:

    if name is None: name = 'CSV_TO_OWL'
    if fileExt is None: fileExt = '.owl'


    CSV_TO_OWL = get_handler(
                            name=name,
                            inStages=inStages,
                            outStage=outStage,
                            handlerFunc=csv2rdf_wrapper,
                            fileWriter=ioutils.writeFile,
                            fileExt=fileExt,
                            fileWriterKwargs={'newline':''},
                            unroll_input = unroll_input,
                            *args,
                            **kwargs
                      )
    return CSV_TO_OWL

