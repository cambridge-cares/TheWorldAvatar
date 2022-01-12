from chemaboxwriters.common.utilsfunc import getRefName, stage_name_to_enum
from chemaboxwriters.app_exceptions.app_exceptions import UnsupportedStage, IncorrectHandlerParameter
from pyuploader import get_uploader
from enum import Enum
import os
from typing import Callable, Dict, List, Tuple, Optional, Any, Union
import logging
import re

logger = logging.getLogger(__name__)


_FS_UPLOADS = 'fs_uploads'
_TS_UPLOADS = 'ts_uploads'
FILE_SERVER = 'file server'
TRIPLE_STORE = 'triple store'

UPLOADS_SPECS_PATTERN = re.compile(r'\w+?((\.\w+?)+?:\w+?|(:\w+?)?)$')

class File_Triple_Uploaders:
    """
    The class provides functionality for file and triple store upload calls.
    It also helps to manage what has been uploaded.
    """
    def __init__(
            self,
            name: str = 'default',
            fs_uploader_kwargs: Optional[Dict] = None,
            ts_uploader_kwargs: Optional[Dict] = None
        )->None:


        _fs_uploader_kwargs = {}
        _ts_uploader_kwargs = {}
        self.name = name
        if fs_uploader_kwargs is not None: _fs_uploader_kwargs = fs_uploader_kwargs
        if ts_uploader_kwargs is not None: _ts_uploader_kwargs = ts_uploader_kwargs

        self.fs_uploader = get_uploader(uploader_type='fs_uploader', **_fs_uploader_kwargs)
        self.ts_uploader = get_uploader(uploader_type='ts_uploader', **_ts_uploader_kwargs)
        self.fs_uploads: Dict[str,str] = {}
        self.ts_uploads: Dict[str,str] = {}

    def get_fs_uploads(self)->Dict[str,str]:
        return self.fs_uploads

    def get_ts_uploads(self)->Dict[str,str]:
        return self.ts_uploads

    def fs_upload(
            self,
            files: List[str],
            uploaders_kwargs: Optional[Dict[str,Any]] = None
        )->None:
        
        _uploaders_kwargs = {}
        if uploaders_kwargs is not None: _uploaders_kwargs = uploaders_kwargs

        for f in files:
            if f in self.fs_uploads:
                logging.info(f"File: {f}, already uploaded to the file server in this pipeline session. Skipping..")
            else:
                logging.info(f"Uploading file: {f}")
                file_ext = f.split('.')[-1]
                output = self.fs_uploader.upload(
                    file_or_dir = f,
                    file_ext = file_ext,
                    **_uploaders_kwargs)

                if 'dry_run' in _uploaders_kwargs:
                    self.fs_uploads[f] = 'new_location'
                else:
                    for key, val in output.items():
                        self.fs_uploads[key] = val

    def ts_upload(
            self,
            files: List[str],
            uploaders_kwargs: Optional[Dict] = None
        )->None:

        _uploaders_kwargs = {}
        if uploaders_kwargs is not None: _uploaders_kwargs = uploaders_kwargs

        ts_upload_nmsp = _uploaders_kwargs.pop('ts_upload_nmsp', None)
        if ts_upload_nmsp is not None:
            url = self.ts_uploader._get_url()
            if url[-1] != '/': url = f"{url}/"
            if ts_upload_nmsp[0] == '/': ts_upload_nmsp = f"{ts_upload_nmsp[1:]}"
            url = f"{url}{ts_upload_nmsp}"

            _uploaders_kwargs['url'] = url

        for f in files:
            if f in self.ts_uploads:
                logging.info(f"File: {f}, already uploaded to the triple store in this pipeline session. Skipping..")
            else:
                logging.info(f"Uploading file: {f}")
                file_ext = f.split('.')[-1]                
                output = self.ts_uploader.upload(
                    file_or_dir = f,
                    file_ext = file_ext,
                    **_uploaders_kwargs)

                if 'dry_run' in _uploaders_kwargs:
                    self.ts_uploads[f] = 'new_location'
                else:
                    for key, val in output.items():
                        self.ts_uploads[key] = val

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
            ts_upload_nmsp: Optional[str] = None,
            fs_upload_subdirs: Optional[str] = None,
            fileWriterKwargs: Optional[Dict[str,Any]] = None,
            unroll_input: bool = True,
            pass_uploaders_ref_as_arg: bool = False,
            upload_inputs_to_fs: bool = False,
            upload_inputs_to_ts: bool = False,
            upload_outputs_to_ts: bool = False,
            upload_outputs_to_fs: bool = False
        )->None:

        self.name = name
        self.inStages= inStages
        self.outStage= outStage
        self.handlerFunc= handlerFunc
        self.fileWriter= fileWriter
        self.fileExt= fileExt
        if fileWriterKwargs is None: fileWriterKwargs = {}
        self.fileWriterKwargs= fileWriterKwargs
        self.writtenFiles=[]
        self.unroll_input=unroll_input
        self.pass_uploaders_ref_as_arg = pass_uploaders_ref_as_arg
        self.fs_upload_subdirs = fs_upload_subdirs
        self.ts_upload_nmsp = ts_upload_nmsp
        self.upload_inputs_to_fs = upload_inputs_to_fs
        self.upload_inputs_to_ts = upload_inputs_to_ts
        self.upload_outputs_to_ts = upload_outputs_to_ts
        self.upload_outputs_to_fs = upload_outputs_to_fs

    def _run(
            self,
            _input: List[str],
            outDir: Optional[str],
            uploaders: Optional[File_Triple_Uploaders] = None,
            handlerKwargs: Optional[Dict[str,Any]] = None,
            dry_run: bool = True,
            disable_uploads: bool = False,
            pipeline: Optional['Pipeline'] = None,
            *args, **kwargs
        )-> Tuple[List[str], Enum]:


        uploaders_kwargs: Dict[str,Any] = {}
        uploaders_kwargs['dry_run'] = dry_run
        if pipeline is not None: uploaders_kwargs['subdirs'] = pipeline.name

        output= self.handle_input(
                        _input = _input,
                        outDir = outDir,
                        uploaders = uploaders,
                        handlerKwargs = handlerKwargs,
                        uploaders_kwargs = uploaders_kwargs,
                        disable_uploads = disable_uploads)
        self.writtenFiles.extend(output)
        return output, self.outStage

    def handle_input(
            self,
            _input: List[str],
            uploaders_kwargs: Dict[str,Any],
            outDir: Optional[str],
            uploaders: Optional[File_Triple_Uploaders] = None,
            handlerKwargs: Optional[Dict[str,Any]] = None,
            disable_uploads: bool = False
        )->List[str]:

        _handlerKwargs= {}
        if handlerKwargs is not None: _handlerKwargs = handlerKwargs
        if uploaders is not None and not disable_uploads:
            uploaders_kwargs['fs_upload_subdirs'] = _handlerKwargs.get('fs_upload_subdirs', self.fs_upload_subdirs)
            uploaders_kwargs['ts_upload_nmsp'] = _handlerKwargs.get('ts_upload_nmsp', self.ts_upload_nmsp)         

            if self.upload_inputs_to_fs:
                uploaders.fs_upload(
                    _input,
                    uploaders_kwargs = uploaders_kwargs)
            if self.upload_inputs_to_ts:
                uploaders.ts_upload(
                    _input,
                    uploaders_kwargs = uploaders_kwargs)

            if self.pass_uploaders_ref_as_arg:
                _handlerKwargs[_FS_UPLOADS] = uploaders.get_fs_uploads()
                _handlerKwargs[_TS_UPLOADS] = uploaders.get_ts_uploads()

        _output = []
        out_paths: List[str] = []

        if self.unroll_input:
            for inp in _input:
                output = self.handlerFunc(inp, **_handlerKwargs)
                _output.extend(output)
        else:
            _output = self.handlerFunc(_input, **_handlerKwargs)

        out_paths = self.get_out_paths(_input, outDir)

        output = self.write_output(_output, out_paths, len(_input))

        if uploaders is not None and not disable_uploads:
            if self.upload_outputs_to_fs:
                uploaders.fs_upload(
                    output,
                    uploaders_kwargs = uploaders_kwargs)
            if self.upload_outputs_to_ts:
                uploaders.ts_upload(
                    output,
                    uploaders_kwargs = uploaders_kwargs)

        return output

    def get_out_paths(
        self,
        _input: List[str],
        outDir: Optional[str]
        )->List[str]:

        out_paths = []

        for inp in _input:
            outFileDir = outDir
            if outFileDir is None: outFileDir = os.path.dirname(inp)
            inp_splitted = inp.split('.')
            outFileBaseName = inp_splitted[0]

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

    def add_uploaders_spec(
            self,
            fs_upload_subdirs: Optional[str] = None,
            ts_upload_nmsp: Optional[str] = None,
            pass_uploaders_ref_as_arg: Optional[bool] = None,
            upload_inputs_to_fs: Optional[bool] = None,
            upload_inputs_to_ts: Optional[bool] = None,
            upload_outputs_to_ts: Optional[bool] = None,
            upload_outputs_to_fs: Optional[bool] = None
        )->None:

        if fs_upload_subdirs is not None: self.fs_upload_subdirs = fs_upload_subdirs
        if ts_upload_nmsp is not None: self.ts_upload_nmsp = ts_upload_nmsp
        if upload_inputs_to_fs is not None: self.upload_inputs_to_fs = upload_inputs_to_fs
        if upload_inputs_to_ts is not None: self.upload_inputs_to_ts = upload_inputs_to_ts
        if upload_outputs_to_fs is not None: self.upload_outputs_to_fs = upload_outputs_to_fs
        if upload_outputs_to_ts is not None: self.upload_outputs_to_ts = upload_outputs_to_ts
        if pass_uploaders_ref_as_arg is not None: self.pass_uploaders_ref_as_arg = pass_uploaders_ref_as_arg
        
    def set_fs_upload_subdirs(
            self,
            fs_upload_subdirs: str,
            handlerKwargs: Dict[str,Any]
        )->None:

       handlerKwargs['fs_upload_subdirs']  = fs_upload_subdirs


    def set_ts_upload_nmsp(
            self,
            ts_upload_nmsp: str,
            handlerKwargs: Dict[str,Any]
        )->None:

       handlerKwargs['ts_upload_nmsp'] = ts_upload_nmsp

    def info(
            self,
            handlerKwargs: Optional[Dict[str,Any]] = None
        )->None:
        self.__str__(handlerKwargs = handlerKwargs)

    def __str__(
            self,
            handlerKwargs: Optional[Dict[str,Any]] = None
        )->None:

        fs_upload_subdirs = self.fs_upload_subdirs
        ts_upload_nmsp = self.ts_upload_nmsp
        if handlerKwargs is not None:
            fs_upload_subdirs = handlerKwargs.pop('fs_upload_subdirs', self.fs_upload_subdirs)
            ts_upload_nmsp = handlerKwargs.pop('ts_upload_nmsp', self.ts_upload_nmsp)

        logger.info(f"Handler name: {self.name}")
        logger.info(f"Handler in stage(s): {[stage.name.lower() for stage in self.inStages]}")
        logger.info(f"Handler out stage: {self.outStage.name.lower()}")
        logger.info(f"Handler out file extension: {self.fileExt}")
        logger.info(f"Handler unrolls input: {self.unroll_input}")
        logger.info(f"Handler passes uploaders ref to the handling function: {self.pass_uploaders_ref_as_arg}")
        logger.info(f"Handler uploads to file server subdir: {fs_upload_subdirs}")
        logger.info(f"Handler uploads to triple store namespace: {ts_upload_nmsp}")
        logger.info(f"Handler uploads inputs to file server: {self.upload_inputs_to_fs}")
        logger.info(f"Handler uploads inputs to triple store: {self.upload_inputs_to_ts}")
        logger.info(f"Handler uploads outputs to file server: {self.upload_outputs_to_fs}")
        logger.info(f"Handler uploads outputs to triple store: {self.upload_outputs_to_ts}")
        
        fs_upload_subdirs = self.fs_upload_subdirs
        

        if handlerKwargs is not None:
            this_handler_kwargs = handlerKwargs.pop(self.name, None)
            if this_handler_kwargs:
                logger.info(f"Handler kwargs:")
                for key, value in this_handler_kwargs.items():
                    logger.info(f"{key}:{value}")

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
        uploaders: Optional[File_Triple_Uploaders] = None,
        fs_upload_subdirs: Optional[str] = None,
        ts_upload_nmsp: Optional[str] = None
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
        if uploaders is not None:
            self.uploaders = uploaders
        else:
            self.uploaders = get_file_triple_uploaders(name)
            
        self.fs_upload_subdirs = fs_upload_subdirs
        self.ts_upload_nmsp = ts_upload_nmsp

    def add_handler(
            self,
            handler: Union[StageHandler, 'Pipeline'],
            handlerName: Optional[str] = None,
            pass_uploaders_ref_as_arg: Optional[bool] = None,
            fs_upload_subdirs: Optional[str] = None,
            ts_upload_nmsp: Optional[str] = None,
            upload_inputs_to_fs: Optional[bool] = None,
            upload_inputs_to_ts: Optional[bool] = None,
            upload_outputs_to_fs: Optional[bool] = None,
            upload_outputs_to_ts: Optional[bool] = None
        )-> Any:

        if handlerName is None:
            handlerName = handler.name

        logger.info(f"Adding {handlerName} handler.")

        self.handlers[handlerName]=handler
        self.inStages.extend([x for x in handler.inStages
                             if x not in self.inStages])

        if self.outStageAutoSet:
            self.outStage = handler.outStage

        if fs_upload_subdirs is None: fs_upload_subdirs = self.fs_upload_subdirs
        if ts_upload_nmsp is None: ts_upload_nmsp = self.ts_upload_nmsp

        handler.add_uploaders_spec(
            fs_upload_subdirs = fs_upload_subdirs,
            ts_upload_nmsp = ts_upload_nmsp,
            pass_uploaders_ref_as_arg = pass_uploaders_ref_as_arg,
            upload_inputs_to_fs = upload_inputs_to_fs,
            upload_inputs_to_ts = upload_inputs_to_ts,
            upload_outputs_to_ts = upload_outputs_to_ts,
            upload_outputs_to_fs = upload_outputs_to_fs)
        return self

    def add_uploaders_spec(
            self,
            fs_upload_subdirs: Optional[str] = None,
            ts_upload_nmsp: Optional[str] = None,
            *args,
            **kwargs
        )->None:

        for _, handler in self.handlers.items():
            handler.add_uploaders_spec(
                fs_upload_subdirs=self.fs_upload_subdirs,
                ts_upload_nmsp=self.ts_upload_nmsp,
                *args,
                **kwargs)

    def run(
            self,
            inputs: List[str],
            inputType: Enum,
            outDir: Optional[str],
            handlerKwargs: Optional[Dict[str,Any]] = None,
            dry_run: bool = True,
            disable_uploads: bool = False
        )->None:

        logger.info(f"Running the {self.name} pipeline.")

        if inputType not in self.inStages:
            requestedStage=inputType.name.lower()
            raise UnsupportedStage(f"Error: Stage: '{requestedStage}' is not supported.")

        logger.info(f"Input stage set to: {inputType}.")
        unroll_input = True

        if self.collate_inputs_at_stages is not None:
            if inputType in self.collate_inputs_at_stages:
                unroll_input = False

        inputs = [os.path.abspath(_inp) for _inp in inputs]

        if unroll_input:
            for _input in inputs:
                self._run(
                        inputs = [_input],
                        inputType = inputType,
                        outDir = outDir,
                        handlerKwargs = handlerKwargs,
                        uploaders = self.uploaders,
                        disable_uploads = disable_uploads,
                        dry_run = dry_run
                    )
        else:
                self._run(
                        inputs = inputs,
                        inputType = inputType,
                        outDir = outDir,
                        handlerKwargs = handlerKwargs,
                        uploaders = self.uploaders,
                        disable_uploads = disable_uploads,
                        dry_run = dry_run
                    )

    def _run(
            self,
            inputs: List[str],
            inputType: Enum,
            outDir: Optional[str],
            handlerKwargs: Optional[Dict[str,Any]] = None,
            uploaders: Optional[File_Triple_Uploaders] = None,
            disable_uploads: bool = False,
            dry_run: bool = True,
            *args,
            **kwargs
        )->Tuple[List[str], Enum]:

        outStageOutput: List[str] = inputs
        outStage: Enum = self.outStage if self.outStage is not None else inputType

        _uploaders = uploaders if uploaders is not None else self.uploaders

        if inputType == outStage:
            outStageOutput = inputs

        for handler in self.handlers.values():
            if inputType in handler.inStages:
                logger.info(f"Executing the {handler.name} handler on the follwoing inputs {inputs}.")

                _handlerKwargs = handlerKwargs.get(handler.name, None) \
                                 if handlerKwargs is not None \
                                 else None

                inputs, inputType = handler._run(
                                                inputs,
                                                inputType = inputType,
                                                outDir = outDir,
                                                handlerKwargs=_handlerKwargs,
                                                uploaders = _uploaders,                                                
                                                dry_run = dry_run,
                                                disable_uploads = disable_uploads,
                                                pipeline = self)
                self.writtenFiles.extend(handler.writtenFiles)
                handler.writtenFiles = []

                if inputType == outStage:
                    outStageOutput = inputs

                logger.info(f"Input stage set to: {inputType}.")

        self.outStageOutput = outStageOutput
        self.outStage = outStage
        return self.outStageOutput, self.outStage

    def get_written_files(
        self,
        )->List[str]:

        return self.writtenFiles


    def set_fs_upload_subdirs(
            self,
            fs_upload_subdirs: str,
            handlerKwargs: Dict[str,Any],
            inner_call: bool = False
        )->None:

        fs_upload_subdirs_ = fs_upload_subdirs.split(',')
        for subdirs in fs_upload_subdirs_:
            upload_specs_match = re.match(UPLOADS_SPECS_PATTERN,subdirs)
            if upload_specs_match is None:
                raise IncorrectHandlerParameter(f"Incorrect --fs-upload-subdirs option: {subdirs}. Please check the syntax.")

            if '.' not in subdirs and ':' not in subdirs:
                for _, handler in self.handlers.items():
                    if handler.name not in handlerKwargs: handlerKwargs[handler.name] = {}
                    if isinstance(handler, Pipeline):
                        handler.set_fs_upload_subdirs(subdirs, handlerKwargs = handlerKwargs[handler.name], inner_call=True)
                    else:
                        handler.set_fs_upload_subdirs(subdirs, handlerKwargs = handlerKwargs[handler.name])
            elif '.' not in subdirs and ':' in subdirs:
                subdirs_ = subdirs.split(':')
                handler_name, handler_subdir = subdirs_[0], subdirs_[1]
                handler = self.handlers[handler_name]
                if handler.name not in handlerKwargs: handlerKwargs[handler.name] = {}
                if isinstance(handler, Pipeline):
                    handler.set_fs_upload_subdirs(handler_subdir, handlerKwargs = handlerKwargs[handler.name], inner_call=True)
                else:
                    handler.set_fs_upload_subdirs(handler_subdir, handlerKwargs = handlerKwargs[handler.name])
            else:
                subdirs_ = subdirs.split(':')
                handler_names = subdirs_[0].split('.')
                handler_name = handler_names.pop(0)
                handler_subdir = f"{'.'.join(handler_names)}:{subdirs_[1]}"
                handler = self.handlers[handler_name]
                if handler.name not in handlerKwargs: handlerKwargs[handler.name] = {}
                if isinstance(handler, Pipeline):
                    handler.set_fs_upload_subdirs(handler_subdir, handlerKwargs = handlerKwargs[handler.name], inner_call=True)
                else:
                    handler.set_fs_upload_subdirs(handler_subdir, handlerKwargs = handlerKwargs[handler.name])

    def set_ts_upload_nmsp(
            self,
            ts_upload_nmsp: str,
            handlerKwargs: Dict[str,Any],
            inner_call: bool = False
        )->None:

        ts_upload_nmsp_ = ts_upload_nmsp.split(',')
        for nmsp in ts_upload_nmsp_:
            upload_specs_match = re.match(UPLOADS_SPECS_PATTERN,nmsp)
            if upload_specs_match is None:
                raise IncorrectHandlerParameter(f"Incorrect --ts-upload-nmsp option: {nmsp}. Please check the syntax.")

            if '.' not in nmsp and ':' not in nmsp:
                for _, handler in self.handlers.items():                    
                    if handler.name not in handlerKwargs: handlerKwargs[handler.name] = {}
                    if isinstance(handler, Pipeline):
                        handler.set_ts_upload_nmsp(nmsp, handlerKwargs = handlerKwargs[handler.name], inner_call=True)
                    else:
                        handler.set_ts_upload_nmsp(nmsp, handlerKwargs = handlerKwargs[handler.name])
            elif '.' not in nmsp and ':' in nmsp:
                nmsp_ = nmsp.split(':')
                handler_name, handler_nmsp = nmsp_[0], nmsp_[1]
                handler = self.handlers[handler_name]
                if handler.name not in handlerKwargs: handlerKwargs[handler.name] = {}
                if isinstance(handler, Pipeline):
                    handler.set_ts_upload_nmsp(handler_nmsp, handlerKwargs = handlerKwargs[handler.name], inner_call=True)
                else:
                    handler.set_ts_upload_nmsp(handler_nmsp, handlerKwargs = handlerKwargs[handler.name])
            else:
                nmsp_ = nmsp.split(':')
                handler_names = nmsp_[0].split('.')
                handler_name = handler_names.pop(0)
                handler_nmsp = f"{'.'.join(handler_names)}:{nmsp_[1]}"
                handler = self.handlers[handler_name]
                if handler.name not in handlerKwargs: handlerKwargs[handler.name] = {}
                if isinstance(handler, Pipeline):
                    handler.set_ts_upload_nmsp(handler_nmsp, handlerKwargs = handlerKwargs[handler.name], inner_call=True)
                else:
                    handler.set_ts_upload_nmsp(handler_nmsp, handlerKwargs = handlerKwargs[handler.name])


    def info(
            self,
            handlerKwargs: Optional[Dict[str,Any]] = None,
            inner_call: bool = False
        )->None:
        self.__str__(handlerKwargs = handlerKwargs, inner_call = inner_call)

    def __str__(
            self,
            handlerKwargs: Optional[Dict[str,Any]] = None,
            inner_call: bool = False
        )->None:

        stage_handler_txt = "-------------------- STAGE HANDLER --------------------"
        if inner_call: stage_handler_txt = "----------------- INNER STAGE HANDLER -----------------"

        if not inner_call: logger.info("#################### PIPELINE INFO ####################")
        logger.info(f"Pipeline name: {self.name}")
        logger.info(f"Pipeline handlers num: {len(self.handlers)}")
        for i, (_, handler) in enumerate(self.handlers.items()):
            if isinstance(handler, StageHandler):
                logger.info(stage_handler_txt)
                logger.info(f"Handler : {i}")
                if handlerKwargs is not None:
                    handler.info(handlerKwargs = handlerKwargs.get(handler.name, None))
                else:
                    handler.info()
            else:
                logger.info("=================== PIPELINE HANDLER ===================")
                logger.info(f"Handler : {i}")
                if handlerKwargs is not None:
                    handler.info(handlerKwargs = handlerKwargs.get(handler.name, None), inner_call = True)
                else:
                    handler.info(inner_call = True)

def get_pipeline(
        name: str = '',
        outStage: Optional[str] = None,
        collate_inputs_at_stages: Optional[List[str]]=None,
        uploaders: Optional[File_Triple_Uploaders] = None,
        fs_upload_subdirs: Optional[str] = None,
        ts_upload_nmsp: Optional[str] = None
    )->Pipeline:

    outStageEnum = None
    if outStage is not None: outStageEnum = stage_name_to_enum(outStage)

    collate_inputs_at_stages_enums = None
    if collate_inputs_at_stages is not None:
        collate_inputs_at_stages_enums = [stage_name_to_enum(stage) for stage in collate_inputs_at_stages]

    pipeline = Pipeline(
                    name = name,
                    outStage = outStageEnum,
                    collate_inputs_at_stages=collate_inputs_at_stages_enums,
                    uploaders = uploaders,
                    fs_upload_subdirs = fs_upload_subdirs,
                    ts_upload_nmsp = ts_upload_nmsp
               )
    return pipeline

def get_handler(
        inStages: List[str],
        outStage: str,
        handlerFunc: Callable,
        fileWriter: Callable,
        name: Optional[str] = None,
        fileExt: Optional[str] = None,
        fileWriterKwargs: Optional[Dict[str,Any]] = None,
        unroll_input: bool=True,
        fs_upload_subdirs: Optional[str] = None,
        ts_upload_nmsp: Optional[str] = None,
        pass_uploaders_ref_as_arg: bool = False,
        upload_inputs_to_fs: bool = False,
        upload_inputs_to_ts: bool = False,
        upload_outputs_to_ts: bool = False,
        upload_outputs_to_fs: bool = False
    )->StageHandler:

    inStageEnums = [stage_name_to_enum(stage) for stage in inStages]
    outStageEnum = stage_name_to_enum(outStage)

    if name is None: name = f"{'_'.join(inStages)}_TO_{outStage}"
    if fileExt is None: fileExt = f".{outStage.replace('_','.')}".lower()

    handler = StageHandler(
        name=name,
        inStages=inStageEnums,
        outStage=outStageEnum,
        handlerFunc=handlerFunc,
        fileWriter=fileWriter,
        fileWriterKwargs=fileWriterKwargs,
        fileExt=fileExt,
        unroll_input=unroll_input,
        pass_uploaders_ref_as_arg = pass_uploaders_ref_as_arg,
        fs_upload_subdirs = fs_upload_subdirs,
        ts_upload_nmsp = ts_upload_nmsp,
        upload_inputs_to_fs = upload_inputs_to_fs,
        upload_inputs_to_ts = upload_inputs_to_ts,
        upload_outputs_to_ts = upload_outputs_to_ts,
        upload_outputs_to_fs = upload_outputs_to_fs)

    return handler

def get_file_triple_uploaders(name)->File_Triple_Uploaders:
    return File_Triple_Uploaders(name)


