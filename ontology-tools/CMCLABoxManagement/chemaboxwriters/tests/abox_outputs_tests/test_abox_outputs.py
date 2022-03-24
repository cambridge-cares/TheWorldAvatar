from chemaboxwriters.app import write_abox
from chemaboxwriters.common.utilsfunc import readFile, fileExists
import pytest
import shutil
import re
import os
import chemaboxwriters.common.assemble_pipeline as asp
from chemaboxwriters.ontocompchem.pipeline import OC_PIPELINE
from chemaboxwriters.ontospecies.pipeline import OS_PIPELINE
from chemaboxwriters.ontomops.pipeline import OMOPS_PIPELINE
from chemaboxwriters.ontopesscan.pipeline import OPS_PIPELINE
from typing import List


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

        for tarLine, refLine in zip(targetFile, refFile):
            assert tarLine == refLine

        assert len(targetFile) == len(refFile)

    for file in files_to_check_existance:
        assert fileExists(file) is True


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
    regenerate_all_results=False,
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
    pipeline.set_handlers_parameters(
        handlers_params_config={
            "QC_JSON_TO_OC_JSON": {
                "random_id": "testID-111-111-111",
                "ontospecies_IRI": "test_species_iri",
            }
        }
    )

    write_abox(
        pipeline=pipeline, file_or_dir=inp_file_or_dir, input_file_type=inp_file_type
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
    mocker.patch(
        "chemaboxwriters.ontospecies.handlers.qc_json_handler.pcp.get_compounds",
        return_value=[DummyPubchemComp(cid=1111, synonyms=["1111-11-1"])],
    )

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
    inp_file_or_dir,
    inp_file_type,
    handler_kwargs,
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

    pipeline = asp.assemble_pipeline(
        pipeline_type=OPS_PIPELINE, config_file=ABOX_CONFIG_FILE
    )
    pipeline.set_handlers_parameters(handlers_params_config=handler_kwargs)

    write_abox(
        pipeline=pipeline, file_or_dir=inp_file_or_dir, input_file_type=inp_file_type
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

    pipeline = asp.assemble_pipeline(
        pipeline_type=OMOPS_PIPELINE, config_file=ABOX_CONFIG_FILE
    )
    pipeline.set_handlers_parameters(
        handlers_params_config={
            "OMINP_JSON_TO_OM_JSON": {"random_id": "testID-111-111-111"}
        }
    )

    write_abox(
        pipeline=pipeline, file_or_dir=inp_file_or_dir, input_file_type=inp_file_type
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
