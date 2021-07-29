import os
from chemaboxwriters.ontocompchem.writeabox import write_abox as write_oc_abox
from chemaboxwriters.ontospecies.writeabox import write_abox as write_os_abox
from chemaboxwriters.ontopesscan.writeabox import write_abox as write_ops_abox
from chemutils.ioutils import readFile
import pytest
import glob
import shutil

THIS_DIR = os.path.dirname(os.path.abspath(__file__))

#QC_LOGS_DIR = os.path.join(THIS_DIR,'..','..','..','..','thermo','CoMoCompChemParser','tests','gaussian')
OCOMPCHEM_REF_DIR = os.path.join(THIS_DIR,'refData','ontocompchem')
OSPECIES_REF_DIR = os.path.join(THIS_DIR,'refData','ontospecies')
OPSSCAN_REF_DIR = os.path.join(THIS_DIR,'refData','ontopesscan')

# setup the pipelines

def compare_results(pipeline,regenerateResult, regenerateAllResults):
    if regenerateResult or regenerateAllResults:
        for file in pipeline.writtenFiles:
            shutil.copy2(file, file+'_ref')

    for file in pipeline.writtenFiles:
        targetFile = readFile(file)
        refFile = readFile(file+'_ref')
        if '.owl' not in file[-4:]:
            assert targetFile==refFile
        else:
            assert len(targetFile.split('\n')) == len(refFile.split('\n'))

@pytest.mark.parametrize("inp_file_or_dir, inp_file_type,  \
                          regenerateResult",
[
('OC_oc_csv_test', 'csv', False),
('OC_oc_json_test', 'oc_json', False),
('OC_qc_json_test', 'qc_json', False), # zpe energy needs fixing!
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

    compare_results(pipeline,regenerateResult, regenerateAllResults)

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

    compare_results(pipeline,regenerateResult, regenerateAllResults)

    print('========================================================')
    print()
    print()


@pytest.mark.parametrize("inp_file_or_dir, inp_file_type, handlerFuncKwargs, \
                          regenerateResult",
[
('OPS_oc_json_test', 'oc_json',  {'OC_JSON_TO_OPS_JSON':
                                    {'os_iris': 'Species_11-111-111', \
                                     'os_atoms_iris': 'Atom_11-11-111_C1,Atom_11-11-111_C2', \
                                     'oc_atoms_pos': '1,2', \
                                     'calc_id': 'OPStestID-111-111-11'
                                     }}, \
                                        False),
#('OPS_oc_json_test', 'oc_json', {}, False)
]
)
def test_opsscan_abox_writer(inp_file_or_dir, inp_file_type,
           handlerFuncKwargs, regenerateResult, regenerateAllResults=False):
    print('========================================================')
    print('TEST INPUT FILE: ', inp_file_or_dir)
    print('TEST INPUT FILE TYPE: ', inp_file_type)
    print()
    print()

    inp_file_or_dir = os.path.join(OPSSCAN_REF_DIR,inp_file_or_dir)

    pipeline = write_ops_abox(inp_file_or_dir, inp_file_type, OPS_handlerFuncKwargs=handlerFuncKwargs)

    compare_results(pipeline,regenerateResult, regenerateAllResults)

    print('========================================================')
    print()
    print()