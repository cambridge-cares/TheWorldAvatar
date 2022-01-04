from chemaboxwriters.common.commonfunc import getRefName
from chemaboxwriters.app_exceptions.app_exceptions import NotSupportedStage
from chemaboxwriters.common.commonfunc import get_file_extensions
from enum import Enum
import os
from typing import Callable, Dict, List, Tuple, Optional, Any, Union

class StageHandler:
    """
    Generic wrapper to create concrete handlers for abox writing stages.
    """
    def __init__(
        self,
        name: str,
        inStages: List[Enum],
        outStage: Enum,
        handlerFunc: Callable,
        fileWriter: Callable,
        fileExt: str,
        handlerFuncKwargs: Optional[Dict[str,Any]] = None,
        fileWriterKwargs: Optional[Dict[str,Any]] = None,
        unroll_input: bool=True):

        self.name = name
        self.inStages= inStages
        self.outStage= outStage
        self.handlerFunc= handlerFunc
        self.fileWriter= fileWriter
        self.fileExt= fileExt
        if handlerFuncKwargs is None: handlerFuncKwargs = {}
        self.handlerFuncKwargs= handlerFuncKwargs
        if fileWriterKwargs is None: fileWriterKwargs = {}
        self.fileWriterKwargs= fileWriterKwargs
        self.writtenFiles=[]
        self.unroll_input=unroll_input

    def _run(
            self,
            _input: List[str],
            input_type: Enum,
            outDir: str,
            *args, **kwargs)-> Tuple[List[str], Enum]:
        output= self.handle_input(_input, input_type, outDir)
        self.writtenFiles.extend(output)
        return output, self.outStage

    def handle_input(
        self,
        _input: List[str],
        input_type: Enum,
        outDir: str)->List[str]:

        _output = []
        out_paths: List[str] = []
        if self.unroll_input:
            for inp in _input:
                output = self.handlerFunc(inp, **self.handlerFuncKwargs)
                _output.extend(output)
        else:
            _output = self.handlerFunc(_input, **self.handlerFuncKwargs)

        out_paths = self.get_outpaths(_input, outDir)

        output = self.write_output(_output, out_paths, len(_input))
        return output

    def get_outpaths(
        self,
        _input: List[str],
        outDir: Optional[str]
        )->List[str]:

        out_paths = []

        #if self.inpFileExt is None:
        #    inpfileExt = get_file_extensions(inStage=input_type)

        for inp in _input:
            outFileDir = outDir
            if outFileDir is None: outFileDir = os.path.dirname(inp)
            inp_splitted = inp.split('.')
            outFileBaseName = inp_splitted[0]
            #if '.' in inp:
            #    inp_splitted = inp.split('.')
            #    outFileBaseName = '.'.join(inp_splitted[0])
            #else:
            #    outFileBaseName = inp

            #file_ext = ''
            #for ext in inpfileExt:
            #    if ext in inp[-len(ext):]:
            #        file_ext = ext
            #        break

            #outFileBaseName = ''.join(inp.split(file_ext)[:-1])
            out_paths.append(os.path.join(outFileDir,outFileBaseName))
        return out_paths

    def write_output(
            self,
            _output: List[str],
            output_paths: List[str],
            _input_len: int,
            )->List[str]:

        writtenFiles = []
        output_len = len(_output)
        jobNum = output_len if _input_len < output_len else 1
        for jobId, output in enumerate(_output):
            out_path = output_paths[jobId] \
                       if jobId < len(output_paths) \
                       else output_paths[0]
            refOutPath = getRefName(out_path, jobId, jobNum, self.fileExt)
            self.fileWriter(refOutPath, output, **self.fileWriterKwargs)
            writtenFiles.append(refOutPath)
        return writtenFiles

    def set_file_ext(
        self,
        fileExt: str)->Any:

        self.fileExt = fileExt
        return self

    def set_handler_func_kwargs(
        self,
        funcKwargs: Dict[str, Any]
        )->Any:

        self.handlerFuncKwargs = funcKwargs
        return self

class Pipeline:
    """
    The Pipeline interface declares a method for building the chain of handlers.
    It also declares a method for executing a request.
    """
    def __init__(
        self,
        name: Optional[str] = None,
        collate_inputs_at_stages: Optional[List[Enum]] = None,
        outStage: Optional[Enum] = None,
        ):

        if name is None: name = ''
        self.name = name

        self.handlers: Dict[str, Union[StageHandler, 'Pipeline']] = {}
        self.writtenFiles: List[str] = []
        self.inStages: List[Enum] = []
        self.collate_inputs_at_stages = collate_inputs_at_stages
        self.outStage = outStage
        self.outStageAutoSet = outStage is None
        self.outStageOutput: Optional[List[str]] = None

    def add_handler(
        self,
        handler: Union[StageHandler, 'Pipeline'],
        handlerName: Optional[str] = None
        )-> Any:

        if handlerName is None:
            handlerName = handler.name

        self.handlers[handlerName]=handler
        self.inStages.extend([x for x in handler.inStages
                             if x not in self.inStages])

        if self.outStageAutoSet:
            self.outStage = handler.outStage
        return self

    def set_handler_func_kwargs(
        self,
        funcKwargs: Dict[str, Any]
        )->Any:

        for handler_name, funcKwargs in funcKwargs.items():
            handler = self.handlers[handler_name]
            handler.set_handler_func_kwargs(funcKwargs)
        return self

    def run(
        self,
        inputs: List[str],
        inputType: Enum,
        outDir: str,
        )->None:

        if inputType not in self.inStages:
            requestedStage=inputType.name.lower()
            raise NotSupportedStage(f"Error: Stage: '{requestedStage}' is not supported.")

        unroll_input = True

        if self.collate_inputs_at_stages is not None:
            if inputType in self.collate_inputs_at_stages:
                unroll_input = False

        if unroll_input:
            for _input in inputs:
                self._run([_input], inputType, outDir)
        else:
            self._run(inputs, inputType, outDir)

    def _run(
        self,
        inputs: List[str],
        inputType: Enum,
        outDir: str,
        )->Tuple[List[str], Enum]:

        outStageOutput: List[str] = inputs
        outStage: Enum = self.outStage if self.outStage is not None else inputType

        if inputType == outStage:
            outStageOutput = inputs

        for handler in self.handlers.values():
            if inputType in handler.inStages:
                inputs, inputType = handler._run(inputs, inputType, outDir)
                self.writtenFiles.extend(handler.writtenFiles)
                handler.writtenFiles = []

                if inputType == outStage:
                    outStageOutput = inputs

        self.outStageOutput = outStageOutput
        self.outStage = outStage
        return self.outStageOutput, self.outStage

    def get_written_files(
        self,
        )->List[str]:

        return self.writtenFiles

def get_pipeline(
    name: str = '',
    handlers: List[StageHandler] = [],
    collate_inputs_at_stages: Optional[List[Enum]]=None,
    )->Pipeline:

    pipeline = Pipeline(
        name = name,
        collate_inputs_at_stages=collate_inputs_at_stages)
    for handler in handlers:
        pipeline.add_handler(handler)
    return pipeline

def get_handler(
        name: str,
        inStages: List[Enum],
        outStage: Enum,
        handlerFunc: Callable,
        fileWriter: Callable,
        fileExt: str,
        handlerFuncKwargs: Optional[Dict[str,Any]] = None,
        fileWriterKwargs: Optional[Dict[str,Any]] = None,
        unroll_input: bool=True
        )->StageHandler:

    handler = StageHandler(
        name=name,
        inStages=inStages,
        outStage=outStage,
        handlerFunc=handlerFunc,
        fileWriter=fileWriter,
        fileWriterKwargs=fileWriterKwargs,
        handlerFuncKwargs=handlerFuncKwargs,
        fileExt=fileExt,
        unroll_input=unroll_input)

    return handler