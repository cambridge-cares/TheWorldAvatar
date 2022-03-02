import shutil
import chemaboxwriters.common.utilsfunc as utilsfunc
import chemaboxwriters.common.endpoints_proxy as endp
from pyuploader.uploaders.uploader import Uploader
import pytest
import shutil
import re
import os
from chemaboxwriters.common.handler import Handler
from chemaboxwriters.ontocompchem.pipeline import get_pipeline
from typing import List, Optional, Dict
from enum import Enum

TEST_INPUT_TYPES = Enum("TEST_INPUT_TYPES", "TEST1 TEST2 TEST3")

THIS_DIR = os.path.dirname(os.path.abspath(__file__))
TEST_FILE_SOURCES = os.path.join(THIS_DIR, "refData", "test_uploads_sources")
ABOX_CONFIG_FILE = os.path.join(
    THIS_DIR, "refData", "test_config_files", "test_config_3.yml"
)


class Test_Handler(Handler):
    def _handle_input(
        self,
        inputs: List[str],
        out_dir: str,
        input_type: Enum,
        dry_run: bool,
        triple_store_uploads: Optional[Dict] = None,
        file_server_uploads: Optional[Dict] = None,
        **handler_kwargs,
    ) -> List[str]:

        outputs: List[str] = []
        for file_path in inputs:
            out_file_path = utilsfunc.get_out_file_path(
                input_file_path=file_path,
                file_extension=self._out_stage.name.lower(),
                out_dir=out_dir,
            )
            shutil.copyfile(file_path, out_file_path)
            outputs.append(out_file_path)
        return outputs


def cleanup_test_data(writtenFiles: List[str]) -> None:

    for file in writtenFiles:
        os.remove(file)


def dummy_upload_func(
    url: str,
    auth_file: str,
    file_path: str,
    *args,
    **kwargs,
) -> str:

    subdirs = kwargs.get("subdirs")
    file_name = os.path.basename(file_path)
    ret_string = f"{url}@{file_name}"
    if subdirs is not None:
        if not subdirs.endswith("/"):
            subdirs = f"{subdirs}/"
        ret_string = f"{url}@{subdirs}/{file_name}"

    return ret_string


def test_abox_uploads(clean_tests):
    print("========================================================")
    pipeline_name = "test_pipeline"
    dummy_inputs = [
        os.path.join(TEST_FILE_SOURCES, "input_1.log"),
        os.path.join(TEST_FILE_SOURCES, "input_2.log"),
    ]

    fs_uploads_results = {
        dummy_inputs[0]: "fs_endpoint@input_1.log",
        dummy_inputs[1]: "fs_endpoint@input_2.log",
        os.path.join(TEST_FILE_SOURCES, "input_1.test2"): "fs_endpoint@input_1.test2",
        os.path.join(TEST_FILE_SOURCES, "input_2.test2"): "fs_endpoint@input_2.test2",
    }
    ts_uploads_results = {
        os.path.join(TEST_FILE_SOURCES, "input_1.test3"): "ts_endpoint@input_1.test3",
        os.path.join(TEST_FILE_SOURCES, "input_2.test3"): "ts_endpoint@input_2.test3",
    }

    dummy_uploader = Uploader(
        upload_file_func=dummy_upload_func,
        uploader_name="dummy_uploader",
        supported_file_ext="all",
        default_no_auth=True,
    )

    endpoints_proxy = endp.get_endpoints_proxy(
        file_server_uploader=dummy_uploader, triple_store_uploader=dummy_uploader
    )

    handlers: List[Handler] = [
        Test_Handler(
            name="test_handler_1",
            in_stage=TEST_INPUT_TYPES.TEST1,
            out_stage=TEST_INPUT_TYPES.TEST2,
        ),
        Test_Handler(
            name="test_handler_2",
            in_stage=TEST_INPUT_TYPES.TEST2,
            out_stage=TEST_INPUT_TYPES.TEST3,
        ),
    ]

    pipeline = get_pipeline(
        name=pipeline_name,
        handlers=handlers,
        endpoints_proxy=endpoints_proxy,
    )

    pipeline.configure_from_file(config_file=ABOX_CONFIG_FILE)

    pipeline.run(
        inputs=dummy_inputs,
        input_type=TEST_INPUT_TYPES.TEST1,
        out_dir=TEST_FILE_SOURCES,
        dry_run=False,
    )

    if clean_tests:
        cleanup_test_data(pipeline.written_files)

    assert pipeline._file_server_uploads == fs_uploads_results
    assert pipeline._triple_store_uploads == ts_uploads_results

    print("========================================================")
    print()
    print()
