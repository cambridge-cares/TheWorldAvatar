from chemaboxwriters.common.commonfunc import getRefName
from chemaboxwriters.app_exceptions.app_exceptions import NotSupportedStage
from chemaboxwriters.common.commonfunc import get_file_extensions
from enum import Enum
import os
from typing import Callable, Dict, List, Tuple, Optional, Any

class Stage:
    def __init__(
        self,
        name: str,
        inStage: Enum,
        outStage: Enum,
        stageFunc: Callable,
        fileWriter: Callable,
        fileExt: str,
        inpFileExt: Optional[List[str]] = None,
        stageFuncKwargs: Dict[str,Any] = {},
        fileWriterKwargs: Dict[str,Any] = {},
        unrollListInput: bool=True):

        self.name = name
        self.inStage= inStage
        self.outStage= outStage
        self.stageFunc= stageFunc
        self.fileWriter= fileWriter
        self.fileExt= fileExt
        self.stageFuncKwargs= stageFuncKwargs
        self.fileWriterKwargs= fileWriterKwargs
        self.writtenFiles=[]
        self.unrollListInput=unrollListInput
        self.inpFileExt= inpFileExt

    def run(
            self,
            stage_input: List[str],
            outDir: str,
            *args, **kwargs)-> Tuple[List[str], Enum]:
        output= self.handle_input(stage_input, outDir)
        return output, self.outStage

    def handle_input(
        self,
        stage_input: List[str],
        outDir: str)->List[str]:

        stage_output = []
        out_paths: List[str] = []
        if self.unrollListInput:
            for inp in stage_input:
                output = self.stageFunc(inp, **self.stageFuncKwargs)
                stage_output.extend(output)
        else:
            stage_output = self.stageFunc(stage_input, **self.stageFuncKwargs)

        out_paths = self.get_outpaths(stage_input, outDir)

        output = self.write_output(stage_output, out_paths)
        return output

    def get_outpaths(
        self,
        stage_input: List[str],
        outDir: Optional[str]
    )->List[str]:

        outpaths = []

        if self.inpFileExt is None:
            inpfileExt = get_file_extensions(inStage=self.inStage)
        else:
            inpfileExt = self.inpFileExt

        for inp in stage_input:
            outFileDir = outDir
            if outFileDir is None: outFileDir = os.path.dirname(inp)

            file_ext = ''
            for ext in inpfileExt:
                if ext in inp[-len(ext):]:
                    file_ext = ext
                    break

            outFileBaseName = ''.join(inp.split(file_ext)[:-1])
            outpaths.append(os.path.join(outFileDir,outFileBaseName))

        return outpaths

    def write_output(
            self,
            stage_output: List[str],
            output_paths: List[str])->List[str]:

        writtenFiles = []
        jobNum = len(stage_output)
        for jobId, output in enumerate(stage_output):
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

class StageHandler:
    """
    Generic wrapper to create concrete handlers for abox writing stages.
    """
    def __init__(
        self,
        name: str,
        outStage: Optional[Enum] = None):

        self.name: str = name
        self.stages: Dict[str, Stage] = {}
        self.outStageAutoSet: bool =True
        self.inStages: List[Enum]= []
        self.outStage: Optional[Enum] = outStage
        self.writtenFiles: List[str] = []
        self.outStageOutput: Optional[List[str]] = None

    def add_stage(
        self,
        stage: Stage,
        stageName: Optional[str] = None
        )->Any:

        if stageName is None:
            stageName = stage.name

        self.stages[stageName]=stage
        self.inStages.extend([x for x in [stage.inStage]
                             if x not in self.inStages])
        if self.outStageAutoSet:
            self.outStage= stage.outStage
        return self

    def run(
        self,
        inputs: List[str],
        inputType: Enum,
        outDir: str,
        )-> Tuple[List[str], Enum]:

        outStageOutput: List[str] = inputs
        outStage: Enum = self.outStage if self.outStage is not None else inputType

        if inputType == outStage:
            outStageOutput = inputs

        for stage in self.stages.values():
            if inputType == stage.inStage:
                inputs, inputType = stage.run(inputs, outDir)
                self.writtenFiles.extend(stage.writtenFiles)
                stage.writtenFiles = []

                if inputType == outStage:
                    outStageOutput = inputs

        self.outStageOutput = outStageOutput
        self.outStage = outStage
        return self.outStageOutput, self.outStage

    def set_stage_func_kwargs(
        self,
        funcKwargs: Dict[str, Any]
        )->Any:

        for stage_name, stageKwargs in funcKwargs.items():
            stage = self.stages[stage_name]
            stage.stageFuncKwargs = stageKwargs
        return self

    def set_file_ext(
        self,
        stage_name: str,
        fileExt: str)->Any:

        if stage_name in self.stages:
            self.stages[stage_name].set_file_ext(fileExt)
        return self

class Pipeline:
    """
    The Pipeline interface declares a method for building the chain of handlers.
    It also declares a method for executing a request.
    """
    def __init__(
        self,
        fileExtPrefix: str = ''
        ):

        self.handlers: Dict[str, StageHandler] = {}
        self.writtenFiles: List[str] = []
        self.inStages: List[Enum] = []
        self.fileExtPrefix = fileExtPrefix

    def add_handler(
        self,
        handler: StageHandler,
        handlerName: Optional[str] = None
        )-> Any:

        if handlerName is None:
            handlerName = handler.name

        self.handlers[handlerName]=handler
        self.inStages.extend([x for x in handler.inStages
                             if x not in self.inStages])
        return self

    def set_stage_func_kwargs(
        self,
        funcKwargs: Dict[str, Any]
        )->Any:

        for handler_name, funcKwargs in funcKwargs.items():
            handler = self.handlers[handler_name]
            handler.set_stage_func_kwargs(funcKwargs)
        return self

    def run(
        self,
        inputs: List[str],
        inputType: Enum,
        outDir: str,
        )->List[str]:

        if inputType not in self.inStages:
            requestedStage=inputType.name.lower()
            raise NotSupportedStage(f"Error: Stage: '{requestedStage}' is not supported.")

        for handler in self.handlers.values():
            if inputType in handler.inStages:
                inputs, inputType = handler.run(inputs, inputType, outDir)
                self.writtenFiles.extend(handler.writtenFiles)
                handler.writtenFiles = []
        return self.writtenFiles