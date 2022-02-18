from chemaboxwriters.app import _write_abox
from chemaboxwriters.common.utilsfunc import readFile, fileExists
import pytest
import shutil
import re
import os
from chemaboxwriters.ontocompchem.pipeline import OC_PIPELINE
from chemaboxwriters.ontospecies.pipeline import OS_PIPELINE
from chemaboxwriters.ontomops.pipeline import OMOPS_PIPELINE
from chemaboxwriters.ontopesscan.pipeline import OPS_PIPELINE
from typing import List, Dict, Any

THIS_DIR = os.path.dirname(os.path.abspath(__file__))

OCOMPCHEM_REF_DIR = os.path.join(THIS_DIR, "refData", "ontocompchem")
OSPECIES_REF_DIR = os.path.join(THIS_DIR, "refData", "ontospecies")
OPSSCAN_REF_DIR = os.path.join(THIS_DIR, "refData", "ontopesscan")
OMOPS_REF_DIR = os.path.join(THIS_DIR, "refData", "ontomops")


class DummyPubchemComp:
    def __init__(self, cid, synonyms):
        self.cid = cid
        self.synonyms = synonyms


def compare_results(pipeline, regenerate_result, regenerate_all_results, file_exts):

    files_to_check_content = []
    files_to_check_existance = []

    for file in pipeline.written_files:
        check_content = False
        for fileExt in file_exts:
            match = re.search(f"\.{fileExt}$", file)
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
        targetFile = readFile(file).split("\n")
        refFile = readFile(file + "_ref").split("\n")

        assert len(targetFile) == len(refFile)

        for tarLine, refLine in zip(targetFile, refFile):
            assert tarLine == refLine

    for file in files_to_check_existance:
        assert fileExists(file) == True


def cleanup_test_data(writtenFiles: List[str]) -> None:

    for file in writtenFiles:
        os.remove(file)


@pytest.mark.parametrize(
    "inp_file_or_dir, inp_file_type,  \
                          regenerate_result",
    [
        ("OC_oc_csv_test", "oc_csv", False),
        ("OC_oc_json_test", "oc_json", False),
        ("OC_qc_json_test", "qc_json", False),
        ("OC_qc_log_multi_job_test", "qc_log", False),
        ("OC_qc_log_multi_log_scan_test", "qc_log", False),
        ("OC_qc_log_single_log_scan_test\\ethane_scan_rigid.g09", "qc_log", False),
    ],
)
def test_ocompchem_abox_writer(
    inp_file_or_dir,
    inp_file_type,
    regenerate_result,
    clean_tests,
    mocker,
    regenerate_all_results=False,
):
    print("========================================================")
    print("TEST INPUT FILE: ", inp_file_or_dir)
    print("TEST INPUT FILE TYPE: ", inp_file_type)
    print()
    print()

    inp_file_or_dir = os.path.join(OCOMPCHEM_REF_DIR, inp_file_or_dir)
    handler_kwargs = {"QC_JSON_TO_OC_JSON": {"random_id": "testID-111-111-111"}}
    mocker.patch(
        "chemaboxwriters.kgoperations.querytemplates.get_species_iri",
        return_value="test_species_iri",
    )

    pipeline = _write_abox(
        pipeline_type=OC_PIPELINE,
        file_or_dir=inp_file_or_dir,
        input_file_type=inp_file_type,
        handler_kwargs=handler_kwargs,
        no_file_logging=True,
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
    inp_file_or_dir,
    inp_file_type,
    regenerate_result,
    clean_tests,
    mocker,
    regenerate_all_results=False,
):
    print("========================================================")
    print("TEST INPUT FILE: ", inp_file_or_dir)
    print("TEST INPUT FILE TYPE: ", inp_file_type)
    print()
    print()

    inp_file_or_dir = os.path.join(OSPECIES_REF_DIR, inp_file_or_dir)
    handler_kwargs = {"QC_JSON_TO_OS_JSON": {"random_id": "testID-111-111-111"}}

    mocker.patch(
        "chemaboxwriters.ontospecies.handlers.qc_json_handler.pcp.get_compounds",
        return_value=[DummyPubchemComp(cid=1111, synonyms=["1111-11-1"])],
    )

    pipeline = _write_abox(
        pipeline_type=OS_PIPELINE,
        file_or_dir=inp_file_or_dir,
        input_file_type=inp_file_type,
        handler_kwargs=handler_kwargs,
        no_file_logging=True,
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
    "inp_file_or_dir, inp_file_type, \
                          regenerate_result",
    [
        ("OPS_oc_json_test", "oc_json", False),
        ("OPS_qc_json_test\\co2_cbsapno_g09.qc_json", "qc_json", False),
        ("OPS_qc_log_test", "qc_log", False),
        ("OPS_qc_log_angle_test", "qc_log", False),
        ("OPS_qc_log_dihedral_test", "qc_log", False),
    ],
)
def test_opsscan_abox_writer(
    inp_file_or_dir,
    inp_file_type,
    regenerate_result,
    clean_tests,
    regenerate_all_results=False,
):
    print("========================================================")
    print("TEST INPUT FILE: ", inp_file_or_dir)
    print("TEST INPUT FILE TYPE: ", inp_file_type)
    print()
    print()

    inp_file_or_dir = os.path.join(OPSSCAN_REF_DIR, inp_file_or_dir)

    handler_kwargs: Dict[str, Any]

    if "angle" in inp_file_or_dir:
        handler_kwargs = {
            "OC_JSON_TO_OPS_JSON": {
                "os_iris": "Species_11-111-111",
                "os_atoms_iris": "Atom_11-11-111_C1,Atom_11-11-111_O1,Atom_11-11-111_H1",
                "oc_atoms_pos": "2,3,9",
                "random_id": "OPStestID-111-111-11",
            }
        }
    elif "dihedral" in inp_file_or_dir:
        handler_kwargs = {
            "OC_JSON_TO_OPS_JSON": {
                "os_iris": "Species_11-111-111",
                "os_atoms_iris": "Atom_11-11-111_H1,Atom_11-11-111_C1,Atom_11-11-111_C2,Atom_11-11-111_H2",
                "oc_atoms_pos": "4,1,2,8",
                "random_id": "OPStestID-111-111-11",
            }
        }
    else:
        handler_kwargs = {
            "OC_JSON_TO_OPS_JSON": {
                "os_iris": "Species_11-111-111",
                "os_atoms_iris": "Atom_11-11-111_C1,Atom_11-11-111_C2",
                "oc_atoms_pos": "1,2",
                "random_id": "OPStestID-111-111-11",
            }
        }

    handler_kwargs[OC_PIPELINE] = {
        "QC_JSON_TO_OC_JSON": {"random_id": "OCtestID-111-111-111"}
    }

    pipeline = _write_abox(
        pipeline_type=OPS_PIPELINE,
        file_or_dir=inp_file_or_dir,
        input_file_type=inp_file_type,
        handler_kwargs=handler_kwargs,
        no_file_logging=True,
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
    inp_file_or_dir,
    inp_file_type,
    regenerate_result,
    clean_tests,
    regenerate_all_results=False,
):
    print("========================================================")
    print("TEST INPUT FILE: ", inp_file_or_dir)
    print("TEST INPUT FILE TYPE: ", inp_file_type)
    print()
    print()

    inp_file_or_dir = os.path.join(OMOPS_REF_DIR, inp_file_or_dir)
    handler_kwargs = {"OMINP_JSON_TO_OM_JSON": {"random_id": "testID-111-111-111"}}

    pipeline = _write_abox(
        pipeline_type=OMOPS_PIPELINE,
        file_or_dir=inp_file_or_dir,
        input_file_type=inp_file_type,
        handler_kwargs=handler_kwargs,
        no_file_logging=True,
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
