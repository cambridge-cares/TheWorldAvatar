import pytest
import os
from chemaboxwriters.common.assemble_pipeline import assemble_pipeline
import chemaboxwriters.common.aboxconfig as abconf
from chemaboxwriters.ontocompchem.pipeline import OC_PIPELINE
from chemaboxwriters.ontospecies.pipeline import OS_PIPELINE
from chemaboxwriters.ontomops.pipeline import OMOPS_PIPELINE
from chemaboxwriters.ontopesscan.pipeline import OPS_PIPELINE
from typing import List, Dict

THIS_DIR = os.path.dirname(os.path.abspath(__file__))
TEST_CONFIG_FILES = os.path.join(THIS_DIR, "refData", "test_config_files")
TEST_RESULTS = "test_results"

PIPELINES = [
    OC_PIPELINE,
    OS_PIPELINE,
    OMOPS_PIPELINE,
    OPS_PIPELINE,
]


@pytest.mark.parametrize(
    "endpoints_config_file, pipeline_types",
    [
        ("test_config_1.yml", PIPELINES),
        ("test_config_2.yml", [OC_PIPELINE]),
    ],
)
def test_abox_writer_setup(endpoints_config_file: str, pipeline_types: List[str]):
    print("========================================================")
    print("TEST CONFIG FILE: ", endpoints_config_file)
    print()
    print()

    config_file = os.path.join(TEST_CONFIG_FILES, endpoints_config_file)

    for pipeline_type in pipeline_types:

        pipeline = assemble_pipeline(pipeline_type=pipeline_type)

        pipeline.configure_from_file(config_file=config_file)

        test_results = pipeline._read_config_file(config_file=config_file)
        test_results = test_results[TEST_RESULTS][pipeline_type]

        for handler_name_ref, handler_results_ref in test_results[
            abconf.HANDLERS_CONFIG_KEY
        ].items():

            handler_kwargs = handler_results_ref.pop(abconf.HANDLER_KWARGS, {})
            handler_test = pipeline.get_handler_by_name(handler_name_ref.upper())
            assert handler_test.endpoints_config is not None  # type: ignore
            assert handler_test.endpoints_config == handler_results_ref  # type: ignore
            assert handler_test._handler_kwargs == handler_kwargs  # type: ignore

    print("")

    print("========================================================")
    print()
    print()
