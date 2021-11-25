import json
import os
import os.path
from shutil import copyfile
from z_paths.file_paths import FILE_PATHS
def add_missing_files(uniques):
    r1_list_jsonpath = FILE_PATHS['list_R1']
    with open(r1_list_jsonpath , 'r') as jsonfile:
        data = json.load(jsonfile)
    for model in uniques:
        for item in data: 
            if model in item.keys():
                gbus = item[model]
                gbu1 = gbus[0]
                gbu2 = gbus[1]
                updated_am_gbu_file_1 = FILE_PATHS['mops_lib2_type']+model+"__"+gbu1+".json"
                updated_am_gbu_file_2 = FILE_PATHS['mops_lib2_type']+model+"__"+gbu2+".json"
                r1_file1 = FILE_PATHS['mops_lib1_type']+model+"__"+gbu1+".json"
                r1_file2 = FILE_PATHS['mops_lib1_type']+model+"__"+gbu2+".json"
                isFile1 = os.path.isfile(updated_am_gbu_file_1)  
                isFile2 = os.path.isfile(updated_am_gbu_file_2) 
                if isFile1 is False:
                    copyfile(r1_file1, updated_am_gbu_file_1)
                if isFile2 is False:
                    copyfile(r1_file2, updated_am_gbu_file_2)
                else:
                    pass               
            else:
                pass

                