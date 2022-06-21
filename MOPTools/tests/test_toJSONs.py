import pytest
import re
import os
import shutil
from chemutils.ioutils.ioutils import readFile, fileExists
from CBU_to_os_JSON.cbuCSVtoJSON.osJSONwrapper import cbuOperations
from MOP_to_omJSON.mopcsv_operations.omjson_operations import mopCSVOperations

THIS_DIR = os.path.dirname(os.path.abspath(__file__))

def compare_results(outFileDir, regenerateResult, fileExts):

    files_to_check_content = []
    files_to_check_existance = []

    for file in os.listdir(outFileDir):
        print(file)
        check_content = False
        for fileExt in fileExts:
            match=re.search(fileExt.replace('.','\.')+'$', file)
            if match is not None:
                check_content = True
                break
        
        filepath = os.path.join(THIS_DIR, outFileDir,file)

        if check_content:
            files_to_check_content.append(filepath)
        else:
            files_to_check_existance.append(filepath)


    if regenerateResult:
        for file in files_to_check_content:
            shutil.copy2(file, file+'_ref')

    for file in files_to_check_content:
        targetFile = readFile(file)
        refFile = readFile(file+'_ref')
        assert targetFile == refFile

    for file in files_to_check_existance:
        assert fileExists(file) == True

def cleanup_test_data(outFileDir,fileExts):

    fileExts = [fileExt+'_ref' for fileExt in fileExts]


    for file in outFileDir:
        remove_file = True
        for fileExt in fileExts:
            match=re.search(fileExt.replace('.','\.')+'$', file)
            if match is not None:
                remove_file = False
                break
        if remove_file:
            os.remove(file)

@pytest.mark.parametrize("inp_file, inp_file_type,  \
                          regenerateResult",
[('CBUtest', 'csv' , False)]
)
def test_cbutojson(inp_file, inp_file_type, regenerateResult,clean_tests=None):
    print('========================================================')
    print('TEST INPUT FILE: ', inp_file)
    print('TEST INPUT FILE TYPE: ', inp_file_type)
    print()
    print()

    inp_file_to_run = os.path.join(THIS_DIR,inp_file + '.' + inp_file_type)
    outFilestring = cbuOperations(inp_file_to_run,'CBUs_XYZ_20211014')
    pipeline = os.path.join(THIS_DIR, outFilestring)
    print(pipeline)
    fileExts = ['.qc.json']

    compare_results(pipeline,regenerateResult,
                    fileExts=fileExts)
    
    if clean_tests:
        cleanup_test_data(pipeline, fileExts=fileExts)

    print('========================================================')
    print()
    print()


@pytest.mark.parametrize("inp_file_1, inp_file_2, inp_file_type,  \
                          regenerateResult",
[('CBUtest', 'MOPTest', 'csv' , True)]
)
def test_moptojson(inp_file_1, inp_file_2, inp_file_type, regenerateResult,clean_tests=None):
    print('========================================================')
    print('TEST INPUT FILE 1: ', inp_file_1)
    print('TEST INPUT FILE 2: ', inp_file_2)
    print('TEST INPUT FILE TYPE: ', inp_file_type)
    print()
    print()

    inp_file_run_1 = os.path.join(THIS_DIR,inp_file_1 + '.' + inp_file_type)
    inp_file_run_2 = os.path.join(THIS_DIR,inp_file_2 + '.' + inp_file_type)
    outFilestring = mopCSVOperations(inp_file_run_1,inp_file_run_2)
    pipeline = os.path.join(THIS_DIR, outFilestring)
    print(pipeline)
    fileExts = ['.ominp.json']

    compare_results(pipeline,regenerateResult,
                    fileExts=fileExts)
    
    if clean_tests:
        cleanup_test_data(pipeline, fileExts=fileExts)

    print('========================================================')
    print()
    print()