import pytest
import re
import os
import shutil
from chemutils.ioutils.ioutils import readFile, fileExists
from CBU_to_os_JSON.cbuCSVtoJSON.osJSONwrapper import cbuOperations

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

@pytest.mark.parametrize("inp_file_or_dir, inp_file_type,  \
                          regenerateResult",
[('CBUtest', 'csv' , False)]
)
def test_cbutojson(inp_file_or_dir, inp_file_type, regenerateResult):
    print('========================================================')
    print('TEST INPUT FILE: ', inp_file_or_dir)
    print('TEST INPUT FILE TYPE: ', inp_file_type)
    print()
    print()

    inp_file_or_dir = os.path.join(THIS_DIR,inp_file_or_dir + '.' + inp_file_type)
    outFilestring = cbuOperations(inp_file_or_dir,'CBUs_XYZ_20211014')
    pipeline = os.path.join(THIS_DIR, outFilestring)
    print(pipeline)
    fileExts = ['.qc.json']

    compare_results(pipeline,regenerateResult,
                    fileExts=fileExts)
    
    # if clean_tests:
    #     cleanup_test_data(pipeline, fileExts=fileExts)

    print('========================================================')
    print()
    print()


