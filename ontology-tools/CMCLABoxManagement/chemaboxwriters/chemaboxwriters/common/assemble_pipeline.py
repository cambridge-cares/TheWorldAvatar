import chemaboxwriters.app_exceptions.app_exceptions as app_exc
from chemaboxwriters.common.pipeline import Pipeline
from chemaboxwriters.ontocompchem.pipeline import assemble_oc_pipeline, OC_PIPELINE
from chemaboxwriters.ontospecies.pipeline import assemble_os_pipeline, OS_PIPELINE
from chemaboxwriters.ontomops.pipeline import assemble_omops_pipeline, OMOPS_PIPELINE
from chemaboxwriters.ontopesscan.pipeline import assemble_ops_pipeline, OPS_PIPELINE
from typing import Optional, Dict


def assemble_pipeline(
    pipeline_type: str,
    config_file: Optional[str] = None,
    config_dict: Optional[Dict] = None,
) -> Pipeline:
    """Factory function that assembles each of the supported pipelines."""

    if pipeline_type.upper() == OC_PIPELINE.upper():
        pipeline = assemble_oc_pipeline()
    elif pipeline_type.upper() == OS_PIPELINE.upper():
        pipeline = assemble_os_pipeline()
    elif pipeline_type.upper() == OMOPS_PIPELINE.upper():
        pipeline = assemble_omops_pipeline()
    elif pipeline_type.upper() == OPS_PIPELINE.upper():
        pipeline = assemble_ops_pipeline()
    else:
        raise app_exc.UnsupportedPipeline("Selected pipeline type is not supported")

    if config_dict is not None:
        pipeline.configure_from_dict(config_dict=config_dict)
    elif config_file is not None:
        pipeline.configure_from_file(config_file=config_file)
    return pipeline
