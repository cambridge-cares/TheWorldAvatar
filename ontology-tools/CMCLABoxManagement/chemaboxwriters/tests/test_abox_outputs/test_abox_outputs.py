from chemaboxwriters.app import write_abox
from chemaboxwriters.common.utilsfunc import readFile, fileExists
import chemaboxwriters.common.uploaders as uploaders
import pytest
import shutil
import re
import os
import chemaboxwriters.common.assemble_pipeline as asp
from chemaboxwriters.ontocompchem.pipeline import OC_PIPELINE
from chemaboxwriters.ontospecies.pipeline import OS_PIPELINE
from chemaboxwriters.ontomops.pipeline import OMOPS_PIPELINE
from chemaboxwriters.ontopesscan.pipeline import OPS_PIPELINE
from pytest_mock import MockerFixture
from typing import List, Dict, Callable

# NOTE
#
# In all the tests the resulting owl files are not checked. This is because
# it is enough to check the csv files. As long as csv files are the same then,
# thanks to the entityrdfizer, all the owl files should be the same.
# Owl files, however, are checked for their existance.
#

THIS_DIR = os.path.dirname(os.path.abspath(__file__))
ABOX_CONFIG_FILE = os.path.join(THIS_DIR, "test_config_files", "abox_config.yml")
REF_DIR = os.path.join(THIS_DIR, "..", "refData")
OCOMPCHEM_REF_DIR = os.path.join(REF_DIR, "ontocompchem")
OSPECIES_REF_DIR = os.path.join(REF_DIR, "ontospecies")
OPSSCAN_REF_DIR = os.path.join(REF_DIR, "ontopesscan")
OMOPS_REF_DIR = os.path.join(REF_DIR, "ontomops")


OPS_bond_handler_kwargs = {
    "OC_JSON_TO_OPS_JSON": {
        "os_iris": "Species_11-111-111",
        "os_atoms_iris": "Atom_11-11-111_C1,Atom_11-11-111_C2",
        "oc_atoms_pos": "1,2",
        "random_id": "OPStestID-111-111-11",
    }
}
OPS_angle_handler_kwargs = {
    "OC_JSON_TO_OPS_JSON": {
        "os_iris": "Species_11-111-111",
        "os_atoms_iris": "Atom_11-11-111_C1,Atom_11-11-111_O1,Atom_11-11-111_H1",
        "oc_atoms_pos": "2,3,9",
        "random_id": "OPStestID-111-111-11",
    }
}

OPS_dihedral_angle_handler_kwargs = {
    "OC_JSON_TO_OPS_JSON": {
        "os_iris": "Species_11-111-111",
        "os_atoms_iris": (
            "Atom_11-11-111_H1,Atom_11-11-111_C1," "Atom_11-11-111_C2,Atom_11-11-111_H2"
        ),
        "oc_atoms_pos": "4,1,2,8",
        "random_id": "OPStestID-111-111-11",
    }
}


class DummyPubchemComp:
    def __init__(self, cid: int, synonyms: List[str]):
        self.cid = cid
        self.synonyms = synonyms


class Dummy_Uploader:
    def upload(self, file_ext: str, file_or_dir: str, dry_run: bool) -> Dict[str, str]:
        if dry_run:
            return {}
        return {file_or_dir: f"dummy_location/{os.path.basename(file_or_dir)}"}


def _stub_get_fs_uploader(upload_file_types: List[str], *args, **kwargs):

    return uploaders._get_uploader_client(
        uploader=Dummy_Uploader(),  # type: ignore
        uploader_type=uploaders.FS_UPLOADER,
        upload_file_types=upload_file_types,
        *args,
        **kwargs,
    )


def _stub_get_ts_uploader(upload_file_types: List[str], *args, **kwargs):

    return uploaders._get_uploader_client(
        uploader=Dummy_Uploader(),  # type: ignore
        uploader_type=uploaders.TS_UPLOADER,
        upload_file_types=upload_file_types,
        *args,
        **kwargs,
    )


def compare_results(
    pipeline: asp.Pipeline,
    regenerate_result: bool,
    regenerate_all_results: bool,
    file_exts: List[str],
) -> None:

    files_to_check_content = []
    files_to_check_existance = []

    for file in pipeline.written_files:
        check_content = False
        for fileExt in file_exts:
            match = re.search(r"\.{}$".format(fileExt), file)
            if match is not None:
                check_content = True
                break

        if check_content:
            files_to_check_content.append(file)
        else:
            files_to_check_existance.append(file)

    if regenerate_result or regenerate_all_results:
        for file in files_to_check_content:
            shutil.copy2(file, file + "_ref")

    for file in files_to_check_content:
        targetFile = sorted(readFile(file).split("\n"))
        refFile = sorted(readFile(file + "_ref").split("\n"))

        for tarLine, refLine in zip(targetFile, refFile):
            assert tarLine == refLine

        assert len(targetFile) == len(refFile)

    for file in files_to_check_existance:
        assert fileExists(file) is True


@pytest.mark.parametrize(
    "inp_file_or_dir, inp_file_type,  \
     regenerate_result",
    [
        ("OC_oc_csv_test", "oc_csv", False),
        ("OC_oc_json_test", "oc_json", False),
        ("OC_qc_json_test", "qc_json", False),
        ("OC_qc_log_multi_job_test", "qc_log", False),
        ("OC_qc_log_multi_log_scan_test", "qc_log", False),
        (
            os.path.join("OC_qc_log_single_log_scan_test", "ethane_scan_rigid.g09"),
            "qc_log",
            False,
        ),
    ],
)
def test_ocompchem_abox_writer(
    inp_file_or_dir: str,
    inp_file_type: str,
    regenerate_result: bool,
    clean_tests: bool,
    cleanup_test_data: Callable,
    mocker: MockerFixture,
    regenerate_all_results: bool = False,
):
    print("========================================================")
    print("TEST INPUT FILE: ", inp_file_or_dir)
    print("TEST INPUT FILE TYPE: ", inp_file_type)
    print()
    print()

    mocker.patch(
        "chemaboxwriters.common.uploaders.get_triple_store_uploader",
        side_effect=_stub_get_ts_uploader,
    )
    mocker.patch(
        "chemaboxwriters.common.uploaders.get_file_server_uploader",
        side_effect=_stub_get_fs_uploader,
    )

    inp_file_or_dir = os.path.join(OCOMPCHEM_REF_DIR, inp_file_or_dir)
    pipeline = asp.assemble_pipeline(
        pipeline_type=OC_PIPELINE, config_file=ABOX_CONFIG_FILE
    )
    pipeline.set_handlers_parameters(
        handlers_params_config={
            "QC_JSON_TO_OC_JSON": {
                "random_id": "testID-111-111-111",
                "ontospecies_IRI": "test_species_iri",
            }
        }
    )

    write_abox(
        pipeline=pipeline,
        file_or_dir=inp_file_or_dir,
        input_file_type=inp_file_type,
        dry_run=False,
    )

    fileExts = ["oc_json", "oc_csv"]
    compare_results(
        pipeline, regenerate_result, regenerate_all_results, file_exts=fileExts
    )

    if clean_tests:
        cleanup_test_data(pipeline.written_files)

    print("========================================================")
    print()
    print()


@pytest.mark.parametrize(
    "inp_file_or_dir, inp_file_type,  \
                          regenerate_result",
    [
        ("OS_qc_log_test", "qc_log", False),
        ("OS_qc_json_test", "qc_json", False),
        ("OS_os_json_test", "os_json", False),
        ("OS_os_csv_test", "os_csv", False),
    ],
)
def test_ospecies_abox_writer(
    inp_file_or_dir: str,
    inp_file_type: str,
    regenerate_result: bool,
    clean_tests: bool,
    cleanup_test_data: Callable,
    mocker: MockerFixture,
    regenerate_all_results: bool = False,
):
    print("========================================================")
    print("TEST INPUT FILE: ", inp_file_or_dir)
    print("TEST INPUT FILE TYPE: ", inp_file_type)
    print()
    print()

    mocker.patch(
        "chemaboxwriters.ontospecies.handlers.qc_json_handler.pcp.get_compounds",
        return_value=[DummyPubchemComp(cid=1111, synonyms=["1111-11-1"])],
    )
    mocker.patch(
        "chemaboxwriters.common.uploaders.get_triple_store_uploader",
        side_effect=_stub_get_ts_uploader,
    )
    mocker.patch(
        "chemaboxwriters.common.uploaders.get_file_server_uploader",
        side_effect=_stub_get_fs_uploader,
    )

    inp_file_or_dir = os.path.join(OSPECIES_REF_DIR, inp_file_or_dir)
    pipeline = asp.assemble_pipeline(
        pipeline_type=OS_PIPELINE, config_file=ABOX_CONFIG_FILE
    )

    pipeline.set_handlers_parameters(
        handlers_params_config={
            "QC_JSON_TO_OS_JSON": {"random_id": "testID-111-111-111"}
        }
    )

    write_abox(
        pipeline=pipeline,
        file_or_dir=inp_file_or_dir,
        input_file_type=inp_file_type,
        dry_run=False,
    )

    fileExts = ["os_json", "os_csv"]
    compare_results(
        pipeline, regenerate_result, regenerate_all_results, file_exts=fileExts
    )

    if clean_tests:
        cleanup_test_data(pipeline.written_files)

    print("========================================================")
    print()
    print()


@pytest.mark.parametrize(
    "inp_file_or_dir, inp_file_type, handler_kwargs, \
                          regenerate_result",
    [
        ("OPS_oc_json_bond_test", "oc_json", OPS_bond_handler_kwargs, False),
        ("OPS_oc_json_angle_test", "oc_json", OPS_angle_handler_kwargs, False),
        (
            "OPS_oc_json_dihedral_test",
            "oc_json",
            OPS_dihedral_angle_handler_kwargs,
            False,
        ),
        ("OPS_ops_json_test", "ops_json", {}, False),
        ("OPS_ops_csv_test", "ops_csv", {}, False),
    ],
)
def test_opsscan_abox_writer(
    inp_file_or_dir: str,
    inp_file_type: str,
    handler_kwargs: Dict,
    regenerate_result: bool,
    clean_tests: bool,
    cleanup_test_data: Callable,
    mocker: MockerFixture,
    regenerate_all_results: bool = False,
):
    print("========================================================")
    print("TEST INPUT FILE: ", inp_file_or_dir)
    print("TEST INPUT FILE TYPE: ", inp_file_type)
    print()
    print()

    mocker.patch(
        "chemaboxwriters.common.uploaders.get_triple_store_uploader",
        side_effect=_stub_get_ts_uploader,
    )
    mocker.patch(
        "chemaboxwriters.common.uploaders.get_file_server_uploader",
        side_effect=_stub_get_fs_uploader,
    )

    inp_file_or_dir = os.path.join(OPSSCAN_REF_DIR, inp_file_or_dir)
    pipeline = asp.assemble_pipeline(
        pipeline_type=OPS_PIPELINE, config_file=ABOX_CONFIG_FILE
    )
    pipeline.set_handlers_parameters(handlers_params_config=handler_kwargs)

    write_abox(
        pipeline=pipeline,
        file_or_dir=inp_file_or_dir,
        input_file_type=inp_file_type,
        dry_run=False,
    )

    fileExts = ["ops_json", "ops_csv"]
    compare_results(
        pipeline, regenerate_result, regenerate_all_results, file_exts=fileExts
    )

    if clean_tests:
        cleanup_test_data(pipeline.written_files)

    print("========================================================")
    print()
    print()


@pytest.mark.parametrize(
    "inp_file_or_dir, inp_file_type,  \
                          regenerate_result",
    [
        ("OM_om_json_test", "ominp_json", False),
    ],
)
def test_omops_abox_writer(
    inp_file_or_dir: str,
    inp_file_type: str,
    regenerate_result: bool,
    clean_tests: bool,
    cleanup_test_data: Callable,
    mocker: MockerFixture,
    regenerate_all_results: bool = False,
):
    print("========================================================")
    print("TEST INPUT FILE: ", inp_file_or_dir)
    print("TEST INPUT FILE TYPE: ", inp_file_type)
    print()
    print()

    mocker.patch(
        "chemaboxwriters.common.uploaders.get_triple_store_uploader",
        side_effect=_stub_get_ts_uploader,
    )
    mocker.patch(
        "chemaboxwriters.common.uploaders.get_file_server_uploader",
        side_effect=_stub_get_fs_uploader,
    )

    inp_file_or_dir = os.path.join(OMOPS_REF_DIR, inp_file_or_dir)
    pipeline = asp.assemble_pipeline(
        pipeline_type=OMOPS_PIPELINE, config_file=ABOX_CONFIG_FILE
    )
    pipeline.set_handlers_parameters(
        handlers_params_config={
            "OMINP_JSON_TO_OM_JSON": {"random_id": "testID-111-111-111"}
        }
    )

    write_abox(
        pipeline=pipeline,
        file_or_dir=inp_file_or_dir,
        input_file_type=inp_file_type,
        dry_run=False,
    )

    fileExts = ["om_json", "om_csv"]
    compare_results(
        pipeline, regenerate_result, regenerate_all_results, file_exts=fileExts
    )

    if clean_tests:
        cleanup_test_data(pipeline.written_files)

    print("========================================================")
    print()
    print()
