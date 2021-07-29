from chemaboxwriters.ontocompchem.writeabox import write_abox as write_oc_abox
from chemaboxwriters.ontospecies.writeabox import write_abox as write_os_abox
from chemaboxwriters.ontopesscan.writeabox import write_abox as write_ops_abox
from chemaboxwriters.common.stageenums import aboxStages
from chemaboxwriters.common.commonfunc import get_file_ext, get_inStage
from chemutils.ioutils import readFile, fileExists
import pytest
import shutil
import re
import os

THIS_DIR = os.path.dirname(os.path.abspath(__file__))

#QC_LOGS_DIR = os.path.join(THIS_DIR,'..','..','..','..','thermo','CoMoCompChemParser','tests','gaussian')
OCOMPCHEM_REF_DIR = os.path.join(THIS_DIR,'refData','ontocompchem')
OSPECIES_REF_DIR = os.path.join(THIS_DIR,'refData','ontospecies')
OPSSCAN_REF_DIR = os.path.join(THIS_DIR,'refData','ontopesscan')

def compare_results(pipeline, regenerateResult, regenerateAllResults, fileExts):

    files_to_check_content = []
    files_to_check_existance = []

    for file in pipeline.writtenFiles:
        check_content = False
        for fileExt in fileExts:
            match=re.search(fileExt.replace('.','\.')+'$', file)
            if match is not None:
                check_content = True
                break

        if check_content:
            files_to_check_content.append(file)
        else:
            files_to_check_existance.append(file)


    if regenerateResult or regenerateAllResults:
        for file in files_to_check_content:
            shutil.copy2(file, file+'_ref')

    for file in files_to_check_content:
        targetFile = readFile(file)
        refFile = readFile(file+'_ref')
        assert targetFile == refFile

    for file in files_to_check_existance:
        assert fileExists(file) == True

def cleanup_test_data(pipeline, inp_file_type, fileExtPrefix, fileExts):

    inStage = get_inStage(inp_file_type)
    testInputFileExt = get_file_ext(inStage,fileExtPrefix)
    fileExts = [fileExt+'_ref' for fileExt in fileExts]
    fileExts.extend(testInputFileExt)

    for file in pipeline.writtenFiles:
        remove_file = True
        for fileExt in fileExts:
            match=re.search(fileExt.replace('.','\.')+'$', file)
            if match is not None:
                remove_file = False
                break
        if remove_file:
            os.remove(file)


@pytest.mark.parametrize("inp_file_or_dir, inp_file_type,  \
                          regenerateResult",
[
('OC_oc_csv_test', 'csv', False),
('OC_oc_json_test', 'oc_json', False),
('OC_qc_json_test', 'qc_json', False),
('OC_qc_log_multi_job_test', 'qc_log', False),
('OC_qc_log_multi_log_scan_test', 'qc_log', False),
('OC_qc_log_single_log_scan_test\\ethane_scan_rigid.g09', 'qc_log', False)
]
)
def test_ocompchem_abox_writer(inp_file_or_dir, inp_file_type,
           regenerateResult, regenerateAllResults=False):
    print('========================================================')
    print('TEST INPUT FILE: ', inp_file_or_dir)
    print('TEST INPUT FILE TYPE: ', inp_file_type)
    print()
    print()

    inp_file_or_dir = os.path.join(OCOMPCHEM_REF_DIR,inp_file_or_dir)
    handlerFuncKwargs={
        'QC_JSON_TO_OC_JSON':{'calc_id':'testID-111-111-111'}}
    pipeline = write_oc_abox(inp_file_or_dir, inp_file_type, handlerFuncKwargs=handlerFuncKwargs)

    fileExts = ['.oc.json', '.oc.csv']
    compare_results(pipeline,regenerateResult, regenerateAllResults,
                    fileExts=fileExts)

    cleanup_test_data(pipeline,inp_file_type,fileExtPrefix='oc',fileExts=fileExts)

    print('========================================================')
    print()
    print()


@pytest.mark.parametrize("inp_file_or_dir, inp_file_type,  \
                          regenerateResult",
[
('OS_qc_log_test', 'qc_log', False),
('OS_qc_json_test', 'qc_json', False),
('OS_os_json_test', 'os_json', False),
('OS_os_csv_test', 'csv', False),
]
)
def test_ospecies_abox_writer(inp_file_or_dir, inp_file_type,
           regenerateResult, regenerateAllResults=False):
    print('========================================================')
    print('TEST INPUT FILE: ', inp_file_or_dir)
    print('TEST INPUT FILE TYPE: ', inp_file_type)
    print()
    print()

    inp_file_or_dir = os.path.join(OSPECIES_REF_DIR,inp_file_or_dir)
    handlerFuncKwargs={
        'QC_JSON_TO_OS_JSON':{'calc_id':'testID-111-111-111'}}

    pipeline = write_os_abox(inp_file_or_dir, inp_file_type, handlerFuncKwargs=handlerFuncKwargs)

    fileExts = ['.os.json', '.os.csv']
    compare_results(pipeline,regenerateResult,regenerateAllResults,
                    fileExts=fileExts)

    cleanup_test_data(pipeline,inp_file_type,fileExtPrefix='os',fileExts=fileExts)

    print('========================================================')
    print()
    print()


@pytest.mark.parametrize("inp_file_or_dir, inp_file_type, \
                          regenerateResult",
[
('OPS_oc_json_test', 'oc_json', False),
('OPS_qc_json_test\\co2_cbsapno_g09.log.qc.json', 'qc_json', False),
('OPS_qc_log_test', 'qc_log', False)
]
)
def test_opsscan_abox_writer(inp_file_or_dir, inp_file_type,
           regenerateResult, regenerateAllResults=False):
    print('========================================================')
    print('TEST INPUT FILE: ', inp_file_or_dir)
    print('TEST INPUT FILE TYPE: ', inp_file_type)
    print()
    print()

    inp_file_or_dir = os.path.join(OPSSCAN_REF_DIR,inp_file_or_dir)

    OPS_handlerFuncKwargs= {
        'OC_JSON_TO_OPS_JSON':
                {'os_iris': 'Species_11-111-111',
                 'os_atoms_iris': 'Atom_11-11-111_C1,Atom_11-11-111_C2',
                 'oc_atoms_pos': '1,2',
                 'calc_id': 'OPStestID-111-111-11'
                }
            }

    OC_handlerFuncKwargs={
        'QC_JSON_TO_OC_JSON':{'calc_id':'OCtestID-111-111-111'}}
    pipeline = write_ops_abox(inp_file_or_dir, inp_file_type,
               OPS_handlerFuncKwargs=OPS_handlerFuncKwargs,
               OC_handlerFuncKwargs=OC_handlerFuncKwargs)

    fileExts=['.ops.json', '.ops.csv']
    compare_results(pipeline,regenerateResult, regenerateAllResults,
                    fileExts=fileExts)

    cleanup_test_data(pipeline,inp_file_type,fileExtPrefix='ops',fileExts=fileExts)

    print('========================================================')
    print()
    print()