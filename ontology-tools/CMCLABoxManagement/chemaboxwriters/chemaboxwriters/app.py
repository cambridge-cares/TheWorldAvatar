from chemaboxwriters.common.pipeline import Pipeline
import chemaboxwriters.app_exceptions.app_exceptions as app_exceptions
import chemaboxwriters.common.utilsfunc as utilsfunc
import os
import logging
import textwrap
from typing import Optional

logger = logging.getLogger(__name__)


def write_abox(
    pipeline: Pipeline,
    file_or_dir: str,
    input_file_type: str,
    file_ext: Optional[str] = None,
    out_dir: Optional[str] = None,
    dry_run: bool = True,
    *args,
    **kwargs,
) -> None:

    in_stage = input_file_type
    if in_stage not in pipeline.in_stages:
        supported_stages = [stage for stage in pipeline.in_stages]
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
            (
                f"No {in_stage} files to process. "
                "Directory / file path is either empty or does not exists."
            )
        )
        return

    if out_dir is None:
        out_dir = os.path.dirname(input_file_paths[0])

    pipeline.run(
        inputs=input_file_paths, input_type=in_stage, out_dir=out_dir, dry_run=dry_run
    )
