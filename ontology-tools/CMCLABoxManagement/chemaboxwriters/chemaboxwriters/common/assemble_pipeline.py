import chemaboxwriters.app_exceptions.app_exceptions as app_exc
from chemaboxwriters.common.pipeline import Pipeline
from chemaboxwriters.ontocompchem.pipeline import assemble_oc_pipeline, OC_PIPELINE
from chemaboxwriters.ontospecies.pipeline import assemble_os_pipeline, OS_PIPELINE
from chemaboxwriters.ontomops.pipeline import assemble_omops_pipeline, OMOPS_PIPELINE
from chemaboxwriters.ontopesscan.pipeline import assemble_ops_pipeline, OPS_PIPELINE
import chemaboxwriters.common.endpoints_config as endp_conf
from typing import Optional, Dict


def assemble_pipeline(
    pipeline_type: str,
    config_file: Optional[str] = None,
    endpoints_config: Optional[Dict] = None,
) -> Pipeline:

    if endpoints_config is None:
        if config_file is not None:
            endpoints_config = endp_conf.get_endpoints_config_file(
                config_file=config_file
            )
        else:
            endpoints_config = {}

    endpoints_proxy = endp_conf.get_endpoints_proxy()

    if pipeline_type.upper() == OC_PIPELINE.upper():
        endpoints_config = endp_conf.pre_process_endpoints_config(
            endpoints_config=endpoints_config, config_key=OC_PIPELINE
        )

        pipeline = assemble_oc_pipeline(
            endpoints_config=endpoints_config, endpoints_proxy=endpoints_proxy
        )
    elif pipeline_type.upper() == OS_PIPELINE.upper():
        endpoints_config = endp_conf.pre_process_endpoints_config(
            endpoints_config=endpoints_config, config_key=OS_PIPELINE
        )
        pipeline = assemble_os_pipeline(
            endpoints_config=endpoints_config, endpoints_proxy=endpoints_proxy
        )
    elif pipeline_type.upper() == OMOPS_PIPELINE.upper():
        endpoints_config = endp_conf.pre_process_endpoints_config(
            endpoints_config=endpoints_config, config_key=OMOPS_PIPELINE
        )
        pipeline = assemble_omops_pipeline(
            endpoints_config=endpoints_config, endpoints_proxy=endpoints_proxy
        )
    elif pipeline_type.upper() == OPS_PIPELINE.upper():
        endpoints_config = endp_conf.pre_process_endpoints_config(
            endpoints_config=endpoints_config, config_key=OPS_PIPELINE
        )
        pipeline = assemble_ops_pipeline(
            endpoints_config=endpoints_config, endpoints_proxy=endpoints_proxy
        )
    else:
        raise app_exc.UnsupportedPipeline(f"Selected pipeline type is not supported")
    return pipeline
