from chemaboxwriters.common.pipeline import Pipeline
import chemaboxwriters.app_exceptions.app_exceptions as app_exceptions
from chemaboxwriters.ontocompchem.pipeline import assemble_oc_pipeline, OC_PIPELINE
from chemaboxwriters.ontospecies.pipeline import assemble_os_pipeline, OS_PIPELINE
from chemaboxwriters.ontomops.pipeline import assemble_omops_pipeline, OMOPS_PIPELINE
from chemaboxwriters.ontopesscan.pipeline import assemble_ops_pipeline, OPS_PIPELINE
import chemaboxwriters.common.utilsfunc as utilsfunc
import os
import logging
import textwrap
from typing import Optional, Dict, Any, Union

logger = logging.getLogger(__name__)


def write_abox(
    pipeline_type: str,
    file_or_dir: str,
    input_file_type: str,
    file_ext: Optional[str] = None,
    out_dir: Optional[str] = None,
    handler_kwargs: Optional[Dict[str, Any]] = None,
    log_file_dir: Optional[str] = None,
    log_file_name: Optional[str] = None,
    no_file_logging: bool = False,
    dry_run: bool = True,
    info: bool = False,
    *args,
    **kwargs,
) -> Union[Pipeline, None]:

    pipeline = None
    try:
        pipeline = _write_abox(
            pipeline_type=pipeline_type,
            file_or_dir=file_or_dir,
            input_file_type=input_file_type,
            file_ext=file_ext,
            out_dir=out_dir,
            handler_kwargs=handler_kwargs,
            log_file_dir=log_file_dir,
            log_file_name=log_file_name,
            no_file_logging=no_file_logging,
            info=info,
            *args,
            **kwargs,
        )
        logger.info("Abox writer finished successfully.")
    except Exception as e:
        logger.error(
            "Abox writer failed. Please check the log for a more detailed error description."
        )
        logger.exception(e)
    return pipeline


def _write_abox(
    pipeline_type: str,
    file_or_dir: str,
    input_file_type: str,
    file_ext: Optional[str] = None,
    out_dir: Optional[str] = None,
    handler_kwargs: Optional[Dict[str, Any]] = None,
    log_file_dir: Optional[str] = None,
    log_file_name: Optional[str] = None,
    no_file_logging: bool = False,
    dry_run: bool = True,
    info: bool = False,
    *args,
    **kwargs,
) -> Pipeline:

    if log_file_name is None:
        log_file_name = f"{pipeline_type}_pipeline.aboxlog"

    # if info:
    #    no_file_logging = True

    utilsfunc.config_logging(
        log_file_dir=log_file_dir,
        log_file_name=log_file_name,
        no_file_logging=no_file_logging,
    )

    pipeline = assemble_pipeline(pipeline_type=pipeline_type)

    # if info:
    #    pipeline.info(handlerKwargs=handlerKwargs)
    #    return pipeline

    in_stage = utilsfunc.stage_name_to_enum(input_file_type)
    if in_stage not in pipeline.in_stages:
        supported_stages = [stage.name.lower() for stage in pipeline.in_stages]
        logger.error(
            textwrap.dedent(
                f"""
            Error: The requested --inp-file-type='{input_file_type}'
                   is not supported by the current pipeline.
                   Please choose one of the following options:
                   {supported_stages}"""
            )
        )
        raise app_exceptions.UnsupportedStage

    input_file_paths = utilsfunc.get_stage_files(
        file_or_dir=file_or_dir, in_stage=in_stage, file_ext=file_ext
    )

    if not input_file_paths:
        logger.warning(
            f"""No {in_stage.name.lower()} files to process. Directory / file path is either empty or does not exists."""
        )
        return pipeline

    if out_dir is None:
        out_dir = os.path.dirname(input_file_paths[0])

    pipeline.run(
        inputs=input_file_paths,
        input_type=in_stage,
        out_dir=out_dir,
        dry_run=dry_run,
        handler_kwargs=handler_kwargs,
    )

    return pipeline


def assemble_pipeline(pipeline_type: str) -> Pipeline:

    if pipeline_type.upper() == OC_PIPELINE:
        return assemble_oc_pipeline()
    if pipeline_type.upper() == OS_PIPELINE:
        return assemble_os_pipeline()
    if pipeline_type.upper() == OPS_PIPELINE:
        return assemble_ops_pipeline()
    if pipeline_type.upper() == OMOPS_PIPELINE:
        return assemble_omops_pipeline()

    SUPPORTED_PIPELINES = [OC_PIPELINE, OS_PIPELINE, OPS_PIPELINE, OMOPS_PIPELINE]

    logger.error(
        textwrap.dedent(
            f"""
        Error: The requested pipeline ='{pipeline_type}' is not supported.
               Please choose one of the following options: {SUPPORTED_PIPELINES}"""
        )
    )
    raise app_exceptions.UnsupportedPipeline
