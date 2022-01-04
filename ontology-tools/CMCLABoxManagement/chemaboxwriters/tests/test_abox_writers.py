from chemaboxwriters.ontocompchem.writeabox import write_abox as write_oc_abox
from chemaboxwriters.ontospecies.writeabox import write_abox as write_os_abox
from chemaboxwriters.ontopesscan.writeabox import write_abox as write_ops_abox
from chemaboxwriters.ontomops.writeabox import write_abox as write_om_abox
from chemaboxwriters.common.commonfunc import get_file_extensions, get_inStage
from chemutils.ioutils.ioutils import readFile, fileExists
import pytest
import shutil
import re
import os
from typing import List, Dict, Any

THIS_DIR = os.path.dirname(os.path.abspath(__file__))

#QC_LOGS_DIR = os.path.join(THIS_DIR,'..','..','..','..','thermo','CoMoCompChemParser','tests','gaussian')
OCOMPCHEM_REF_DIR = os.path.join(THIS_DIR,'refData','ontocompchem')
OSPECIES_REF_DIR = os.path.join(THIS_DIR,'refData','ontospecies')
OPSSCAN_REF_DIR = os.path.join(THIS_DIR,'refData','ontopesscan')
OMOPS_REF_DIR = os.path.join(THIS_DIR,'refData','ontomops')

class DummyPubchemComp:
    def __init__(self,cid,synonyms):
        self.cid=cid
        self.synonyms=synonyms

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
        targetFile = readFile(file).split('\n')
        refFile = readFile(file+'_ref').split('\n')

        assert len(targetFile) == len(refFile)

        for tarLine, refLine in zip(targetFile,refFile):
            assert tarLine == refLine

    for file in files_to_check_existance:
        assert fileExists(file) == True

def cleanup_test_data(
    writtenFiles: List[str]
    )->None:

    for file in writtenFiles:
        os.remove(file)


@pytest.mark.parametrize("inp_file_or_dir, inp_file_type,  \
                          regenerateResult",
[
('OC_oc_csv_test', 'oc_csv', False),
('OC_oc_json_test', 'oc_json', False),
('OC_qc_json_test', 'qc_json', False),
('OC_qc_log_multi_job_test', 'qc_log', False),
('OC_qc_log_multi_log_scan_test', 'qc_log', False),
('OC_qc_log_single_log_scan_test\\ethane_scan_rigid.g09', 'qc_log', False)
]
)
def test_ocompchem_abox_writer(inp_file_or_dir, inp_file_type,
           regenerateResult, clean_tests, mocker, regenerateAllResults=False):
    print('========================================================')
    print('TEST INPUT FILE: ', inp_file_or_dir)
    print('TEST INPUT FILE TYPE: ', inp_file_type)
    print()
    print()

    inp_file_or_dir = os.path.join(OCOMPCHEM_REF_DIR,inp_file_or_dir)
    funcKwargs={
        'QC_JSON_TO_OC_JSON': {
                'random_id':'testID-111-111-111'
        }
    }
    mocker.patch("chemaboxwriters.ontocompchem.jsonwriter.get_species_iri",
                 return_value='test_species_iri')

    pipeline = write_oc_abox(inp_file_or_dir, inp_file_type, handlerFuncKwargs=funcKwargs)

    fileExts = ['.oc.json', '.oc.csv']
    compare_results(pipeline,regenerateResult, regenerateAllResults,
                    fileExts=fileExts)

    if clean_tests:
        cleanup_test_data(pipeline.get_written_files())

    print('========================================================')
    print()
    print()


@pytest.mark.parametrize("inp_file_or_dir, inp_file_type,  \
                          regenerateResult",
[
('OS_qc_log_test', 'qc_log', False),
('OS_qc_json_test', 'qc_json', False),
('OS_os_json_test', 'os_json', False),
('OS_os_csv_test', 'os_csv', False),
]
)
def test_ospecies_abox_writer(inp_file_or_dir, inp_file_type,
           regenerateResult, clean_tests, mocker, regenerateAllResults=False):
    print('========================================================')
    print('TEST INPUT FILE: ', inp_file_or_dir)
    print('TEST INPUT FILE TYPE: ', inp_file_type)
    print()
    print()

    inp_file_or_dir = os.path.join(OSPECIES_REF_DIR,inp_file_or_dir)
    handlerFuncKwargs={
        'QC_JSON_TO_OS_JSON':{'random_id':'testID-111-111-111'}}

    mocker.patch("chemaboxwriters.ontospecies.jsonwriter.pcp.get_compounds",
                 return_value=[DummyPubchemComp(cid=1111, synonyms=['1111-11-1'])])

    pipeline = write_os_abox(inp_file_or_dir, inp_file_type, handlerFuncKwargs=handlerFuncKwargs)

    fileExts = ['.os.json', '.os.csv']
    compare_results(pipeline,regenerateResult,regenerateAllResults,
                    fileExts=fileExts)

    if clean_tests:
        cleanup_test_data(pipeline.get_written_files())

    print('========================================================')
    print()
    print()


@pytest.mark.parametrize("inp_file_or_dir, inp_file_type, \
                          regenerateResult",
[
('OPS_oc_json_test', 'oc_json', False),
('OPS_qc_json_test\\co2_cbsapno_g09.qc.json', 'qc_json', False),
('OPS_qc_log_test', 'qc_log', False),
('OPS_qc_log_angle_test', 'qc_log', True),
('OPS_qc_log_dihedral_test', 'qc_log', True)
]
)
def test_opsscan_abox_writer(inp_file_or_dir, inp_file_type,
           regenerateResult, clean_tests, regenerateAllResults=False):
    print('========================================================')
    print('TEST INPUT FILE: ', inp_file_or_dir)
    print('TEST INPUT FILE TYPE: ', inp_file_type)
    print()
    print()

    inp_file_or_dir = os.path.join(OPSSCAN_REF_DIR,inp_file_or_dir)


    handlerFuncKwargs: Dict[str, Any]

    if 'angle' in inp_file_or_dir:
        handlerFuncKwargs= {
        'OC_JSON_TO_OPS_JSON':
                {'os_iris': 'Species_11-111-111',
                'os_atoms_iris': 'Atom_11-11-111_C1,Atom_11-11-111_O1,Atom_11-11-111_H1',
                'oc_atoms_pos': '2,3,9',
                'random_id': 'OPStestID-111-111-11'
                }
            }
    elif 'dihedral' in inp_file_or_dir:
        handlerFuncKwargs= {
        'OC_JSON_TO_OPS_JSON':
                {'os_iris': 'Species_11-111-111',
                'os_atoms_iris': 'Atom_11-11-111_H1,Atom_11-11-111_C1,Atom_11-11-111_C2,Atom_11-11-111_H2',
                'oc_atoms_pos': '4,1,2,8',
                'random_id': 'OPStestID-111-111-11'
                }
            }
    else:
        handlerFuncKwargs= {
        'OC_JSON_TO_OPS_JSON':
                {'os_iris': 'Species_11-111-111',
                'os_atoms_iris': 'Atom_11-11-111_C1,Atom_11-11-111_C2',
                'oc_atoms_pos': '1,2',
                'random_id': 'OPStestID-111-111-11'
                }
            }

    handlerFuncKwargs['OC_PIPELINE'] = {'QC_JSON_TO_OC_JSON': {'random_id':'OCtestID-111-111-111'}}
    pipeline = write_ops_abox(inp_file_or_dir, inp_file_type,
               handlerFuncKwargs=handlerFuncKwargs)

    fileExts=['.ops.json', '.ops.csv']
    compare_results(pipeline,regenerateResult, regenerateAllResults,
                    fileExts=fileExts)

    if clean_tests:
        cleanup_test_data(pipeline.get_written_files())

    print('========================================================')
    print()
    print()


@pytest.mark.parametrize("inp_file_or_dir, inp_file_type,  \
                          regenerateResult",
[
('OM_om_json_test', 'ominp_json', False),
]
)
def test_omops_abox_writer(inp_file_or_dir, inp_file_type,
           regenerateResult, clean_tests, regenerateAllResults=False):
    print('========================================================')
    print('TEST INPUT FILE: ', inp_file_or_dir)
    print('TEST INPUT FILE TYPE: ', inp_file_type)
    print()
    print()

    inp_file_or_dir = os.path.join(OMOPS_REF_DIR,inp_file_or_dir)
    handlerFuncKwargs={
        'OMINP_JSON_TO_OM_JSON':{'random_id':'testID-111-111-111'}}

    pipeline = write_om_abox(inp_file_or_dir, inp_file_type, handlerFuncKwargs=handlerFuncKwargs)

    fileExts = ['.om.json', '.om.csv']
    compare_results(pipeline,regenerateResult,regenerateAllResults,
                    fileExts=fileExts)

    if clean_tests:
        cleanup_test_data(pipeline.get_written_files())

    print('========================================================')
    print()
    print()