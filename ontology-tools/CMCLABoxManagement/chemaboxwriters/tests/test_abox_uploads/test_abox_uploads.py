from chemaboxwriters.app import write_abox
import pytest
import os
from chemaboxwriters.common.pipeline import Pipeline
import chemaboxwriters.common.assemble_pipeline as asp
from chemaboxwriters.ontocompchem.pipeline import OC_PIPELINE
from chemaboxwriters.ontospecies.pipeline import OS_PIPELINE
from chemaboxwriters.ontomops.pipeline import OMOPS_PIPELINE
from chemaboxwriters.ontopesscan.pipeline import OPS_PIPELINE
from pytest_mock import MockerFixture
from typing import Callable, Dict, Optional, List


THIS_DIR = os.path.dirname(os.path.abspath(__file__))
ABOX_CONFIG_FILE = os.path.join(THIS_DIR, "test_config_files", "abox_config.yml")
REF_DIR = os.path.join(THIS_DIR, "..", "refData")
OCOMPCHEM_REF_DIR = os.path.join(REF_DIR, "ontocompchem")
OSPECIES_REF_DIR = os.path.join(REF_DIR, "ontospecies")
OPSSCAN_REF_DIR = os.path.join(REF_DIR, "ontopesscan")
OMOPS_REF_DIR = os.path.join(REF_DIR, "ontomops")


class DummyPubchemComp:
    def __init__(self, cid: int, synonyms: List[str]):
        self.cid = cid
        self.synonyms = synonyms


def check_uploads(
    pipeline: Pipeline, inp_file_type: str, fs_num_uploads: int, ts_num_uploads: int
) -> None:

    fs_uploads = pipeline._file_server_uploads
    ts_uploads = pipeline._triple_store_uploads
    assert len(fs_uploads) == fs_num_uploads
    assert len(ts_uploads) == ts_num_uploads

    inp_stage = inp_file_type

    for handler in pipeline._handlers.values():
        if handler._in_stage == inp_stage:

            fs_upload_configs = handler.get_file_server_upload_configs()
            ts_upload_configs = handler.get_triple_store_upload_configs()

            _check_uploads_exists(fs_uploads, fs_upload_configs)
            _check_uploads_exists(ts_uploads, ts_upload_configs)

            inp_stage = handler._out_stage


def _check_uploads_exists(uploads: Dict, upload_configs: Optional[Dict]) -> None:
    if upload_configs is not None:
        upload_file_types = upload_configs["upload_file_types"]
        url = _construct_full_url(upload_configs)

        file_type_in_uploads = []
        for file_type in upload_file_types:
            for _, upload_entry in uploads.items():
                if upload_entry["input_type"] == file_type:
                    file_type_in_uploads.append(file_type)
                    upload_url = (
                        f"{'/'.join(upload_entry['location'].split('/')[:-1])}/"
                    )
                    assert upload_url == url
        assert set(file_type_in_uploads) == set(upload_file_types)


def _construct_full_url(upload_configs: Dict) -> str:
    url = upload_configs["url"]
    subdirs = upload_configs["subdirs"]
    if not url.endswith("/"):
        url = f"{url}/"
    if subdirs is not None:
        if not subdirs.endswith("/"):
            subdirs = f"{subdirs}/"
        url = f"{url}{subdirs}"
    return url


@pytest.mark.parametrize(
    "inp_file_or_dir, inp_file_type, fs_num_uploads, ts_num_uploads",
    [
        (
            os.path.join("OC_qc_log_single_log_scan_test", "ethane_scan_rigid.g09"),
            "qc_log",
            5,
            4,
        ),
    ],
)
def test_ocompchem_abox_uploads(
    inp_file_or_dir: str,
    inp_file_type: str,
    fs_num_uploads: int,
    ts_num_uploads: int,
    clean_tests: bool,
    cleanup_test_data: Callable,
):
    print("========================================================")
    print("TEST INPUT FILE: ", inp_file_or_dir)
    print("TEST INPUT FILE TYPE: ", inp_file_type)
    print()
    print()

    inp_file_or_dir = os.path.join(OCOMPCHEM_REF_DIR, inp_file_or_dir)
    pipeline = asp.assemble_pipeline(
        pipeline_type=OC_PIPELINE, config_file=ABOX_CONFIG_FILE
    )

    write_abox(
        pipeline=pipeline,
        file_or_dir=inp_file_or_dir,
        input_file_type=inp_file_type,
        dry_run=False,
    )

    check_uploads(pipeline, inp_file_type, fs_num_uploads, ts_num_uploads)

    if clean_tests:
        cleanup_test_data(pipeline.written_files)

    print("========================================================")
    print()
    print()


@pytest.mark.parametrize(
    "inp_file_or_dir, inp_file_type, fs_num_uploads, ts_num_uploads",
    [
        ("OS_qc_log_test\\h2o_opt_n_g09.log", "qc_log", 0, 1),
    ],
)
def test_ospecies_abox_uploads(
    inp_file_or_dir: str,
    inp_file_type: str,
    fs_num_uploads: int,
    ts_num_uploads: int,
    mocker: MockerFixture,
    clean_tests: bool,
    cleanup_test_data: Callable,
):
    print("========================================================")
    print("TEST INPUT FILE: ", inp_file_or_dir)
    print("TEST INPUT FILE TYPE: ", inp_file_type)
    print()
    print()

    inp_file_or_dir = os.path.join(OSPECIES_REF_DIR, inp_file_or_dir)
    mocker.patch(
        "chemaboxwriters.ontospecies.handlers.qc_json_handler.pcp.get_compounds",
        return_value=[DummyPubchemComp(cid=1111, synonyms=["1111-11-1"])],
    )

    pipeline = asp.assemble_pipeline(
        pipeline_type=OS_PIPELINE, config_file=ABOX_CONFIG_FILE
    )

    write_abox(
        pipeline=pipeline,
        file_or_dir=inp_file_or_dir,
        input_file_type=inp_file_type,
        dry_run=False,
    )

    check_uploads(pipeline, inp_file_type, fs_num_uploads, ts_num_uploads)

    if clean_tests:
        cleanup_test_data(pipeline.written_files)

    print("========================================================")
    print()
    print()


@pytest.mark.parametrize(
    "inp_file_or_dir, inp_file_type, fs_num_uploads, ts_num_uploads",
    [
        ("OPS_oc_json_angle_test", "oc_json", 0, 1),
    ],
)
def test_opsscan_abox_uploads(
    inp_file_or_dir: str,
    inp_file_type: str,
    fs_num_uploads: int,
    ts_num_uploads: int,
    clean_tests: bool,
    cleanup_test_data: Callable,
):
    print("========================================================")
    print("TEST INPUT DIR: ", inp_file_or_dir)
    print("TEST INPUT FILE TYPE: ", inp_file_type)
    print()
    print()

    inp_file_or_dir = os.path.join(OPSSCAN_REF_DIR, inp_file_or_dir)

    pipeline = asp.assemble_pipeline(
        pipeline_type=OPS_PIPELINE, config_file=ABOX_CONFIG_FILE
    )

    write_abox(
        pipeline=pipeline,
        file_or_dir=inp_file_or_dir,
        input_file_type=inp_file_type,
        dry_run=False,
    )

    check_uploads(pipeline, inp_file_type, fs_num_uploads, ts_num_uploads)

    if clean_tests:
        cleanup_test_data(pipeline.written_files)

    print("========================================================")
    print()
    print()


@pytest.mark.parametrize(
    "inp_file_or_dir, inp_file_type, fs_num_uploads, ts_num_uploads",
    [
        ("OM_om_json_test\\example.ominp_json", "ominp_json", 1, 1),
    ],
)
def test_omops_abox_uploads(
    inp_file_or_dir: str,
    inp_file_type: str,
    fs_num_uploads: int,
    ts_num_uploads: int,
    clean_tests: bool,
    cleanup_test_data: Callable,
):
    print("========================================================")
    print("TEST INPUT FILE: ", inp_file_or_dir)
    print("TEST INPUT FILE TYPE: ", inp_file_type)
    print()
    print()

    inp_file_or_dir = os.path.join(OMOPS_REF_DIR, inp_file_or_dir)

    pipeline = asp.assemble_pipeline(
        pipeline_type=OMOPS_PIPELINE, config_file=ABOX_CONFIG_FILE
    )

    write_abox(
        pipeline=pipeline,
        file_or_dir=inp_file_or_dir,
        input_file_type=inp_file_type,
        dry_run=False,
    )

    check_uploads(pipeline, inp_file_type, fs_num_uploads, ts_num_uploads)

    if clean_tests:
        cleanup_test_data(pipeline.written_files)

    print("========================================================")
    print()
    print()
