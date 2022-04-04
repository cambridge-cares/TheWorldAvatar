from chemaboxwriters.common.base import Pipeline
import chemaboxwriters.app_exceptions.app_exceptions as app_exceptions
from chemaboxwriters.ontocompchem.pipeline import assemble_oc_pipeline
from chemaboxwriters.ontospecies.pipeline import assemble_os_pipeline
from chemaboxwriters.ontomops.pipeline import assemble_omops_pipeline
from chemaboxwriters.ontopesscan.pipeline import assemble_ops_pipeline
from chemaboxwriters.common.utilsfunc import get_stage_files, stage_name_to_enum, config_logging
import chemaboxwriters.common.globals as globals
import logging
import textwrap
from typing import Optional, Dict, Any, Union

logger = logging.getLogger(__name__)

def write_abox(
        pipeline_type: str,
        fileOrDir: str,
        inpFileType: str ,
        qcLogExt: Optional[str] = None,
        outDir: Optional[str] = None,        
        handlerKwargs: Optional[Dict[str, Any]] = None,
        log_file_dir: Optional[str] = None,
        log_file_name: Optional[str] = None,
        no_file_logging: bool = False,
        fs_upload_subdirs: Optional[str] = None,
        ts_upload_nmsp: Optional[str] = None,
        dry_run: bool = True,
        info: bool = False,
        disable_uploads: bool = False,
        *args,
        **kwargs
    )->Union[Pipeline,None]:
    
    pipeline = None
    try:
        pipeline = _write_abox(
                        pipeline_type = pipeline_type,
                        fileOrDir = fileOrDir,
                        inpFileType = inpFileType ,
                        qcLogExt = qcLogExt,
                        outDir = outDir,
                        handlerKwargs = handlerKwargs,
                        log_file_dir = log_file_dir,
                        log_file_name = log_file_name,
                        no_file_logging = no_file_logging,
                        fs_upload_subdirs = fs_upload_subdirs,
                        ts_upload_nmsp = ts_upload_nmsp,
                        dry_run = dry_run,
                        info = info,
                        disable_uploads = disable_uploads,
                        *args,
                        **kwargs
                    )
        logger.info("Abox writer finished successfully.")
    except Exception as e:
        logger.error("Abox writer failed. Please check the log for a more detailed error description.")
        logger.exception(e)
    return pipeline

def _write_abox(
        pipeline_type: str,
        fileOrDir: str,
        inpFileType: str ,
        qcLogExt: Optional[str] = None,
        outDir: Optional[str] = None,        
        handlerKwargs: Optional[Dict[str, Any]] = None,
        log_file_dir: Optional[str] = None,
        log_file_name: Optional[str] = None,
        no_file_logging: bool = False,
        fs_upload_subdirs: Optional[str] = None,
        ts_upload_nmsp: Optional[str] = None,
        dry_run: bool = True,
        info: bool = False,
        disable_uploads: bool = False,
        *args,
        **kwargs)->Pipeline:

    if log_file_name is None:
        log_file_name = f"{pipeline_type}_pipeline.aboxlog"

    if info: no_file_logging = True
    config_logging(
        log_file_dir=log_file_dir,
        log_file_name=log_file_name,
        no_file_logging=no_file_logging
        )

    pipeline = assemble_pipeline(pipeline_type=pipeline_type)

    if handlerKwargs is None: handlerKwargs = {}
    if fs_upload_subdirs is not None:
        pipeline.set_fs_upload_subdirs(fs_upload_subdirs, handlerKwargs = handlerKwargs)
    if ts_upload_nmsp is not None:
        pipeline.set_ts_upload_nmsp(ts_upload_nmsp, handlerKwargs = handlerKwargs)

    if info:
        pipeline.info(handlerKwargs = handlerKwargs)
        return pipeline

    inStage = stage_name_to_enum(inpFileType)
    if inStage not in pipeline.inStages:
        supportedStagesNames = [stage.name.lower() for stage in pipeline.inStages]
        logger.error(textwrap.dedent(f"""
            Error: The requested --inp-file-type='{inpFileType}'
                   is not supported by the current pipeline.
                   Please choose one of the following options:
                   {supportedStagesNames}"""))
        raise app_exceptions.UnsupportedStage

    files = get_stage_files(fileOrDir, inStage, qcLogExt=qcLogExt)

    if not files:
        logger.warning(f"""No {inStage.name.lower()} files to process. Directory / file path is either empty or does not exists.""")
        return pipeline

    else:
        pipeline.run(
                    inputs = files,
                    inputType= inStage,
                    outDir = outDir,
                    handlerKwargs = handlerKwargs,
                    dry_run = dry_run,
                    disable_uploads=disable_uploads
                )

    return pipeline

def assemble_pipeline(
        pipeline_type: str
    )->Pipeline:


    if pipeline_type.upper() == globals.ONTO_COMP_CHEM_TAG: return assemble_oc_pipeline()
    if pipeline_type.upper() == globals.ONTO_SPECIES_TAG: return assemble_os_pipeline()
    if pipeline_type.upper() == globals.ONTO_PESSCAN_TAG: return assemble_ops_pipeline()
    if pipeline_type.upper() == globals.ONTO_MOPS_TAG: return assemble_omops_pipeline()

    logger.error(textwrap.dedent(f"""
        Error: The requested pipeline ='{pipeline_type}' is not supported.
               Please choose one of the following options: {globals.SUPPORTED_PIPELINES}"""))
    raise app_exceptions.UnsupportedPipeline
    