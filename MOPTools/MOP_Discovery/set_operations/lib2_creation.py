'''
Created on Dec 1, 2021

@author: Aleksandar Kondinski
'''

import json
import os
import os.path
from manager.file_paths import FILE_PATHS
from shutil import copyfile

def lib2_creation(list_gbus):
    """Using the gbu list access and loop over the R2 list as json file. 
    Access the libraries of GBU files related to assembly models and do exchange of 
    CBUs where allowed."""
    # Access the R2 file
    list_R2_filePath = FILE_PATHS['list_R2']    
    with open(list_R2_filePath, 'r+') as list_R2:
        data = json.load(list_R2)
        # using the list of GBUs as strings loop over all different GBUs in the R2 file 
        for gbu in list_gbus:
            overlap_list  = data[gbu]
            
            # For each GBU loop over the different AM combinations that share common CBU
            # Make them exchange furhter CBUS and using the updater function
            # at the end of the loop - write those files.

            for overlap in overlap_list:
                am_1 = overlap[0]
                am_2 = overlap[1]

                # access the GBU of AM files and append the CBUs to two separate lists
                am_gbu_file_1 = FILE_PATHS['r1_cbus']+am_1+"__"+gbu+".json"
                am_gbu_file_2 = FILE_PATHS['r1_cbus']+am_2+"__"+gbu+".json"
                am_gbu_file_1_items = []
                am_gbu_file_2_items = []
                with open(am_gbu_file_1, 'r+') as contents1:
                    data1 = json.load(contents1)
                    am_gbu_file_1_items.append(data1)
                with open(am_gbu_file_2, 'r+') as contents2:
                    data2 = json.load(contents2)
                    am_gbu_file_2_items.append(data2)    

                # Create an empty list of common CBUs and loop over the two lists of CBUs associated 
                # with the particular AMs. Append CBUs to the common list. 
                common_cbus_iri = []
                common_cbus = []
                for list_1 in am_gbu_file_1_items:
                    for cbu1 in list_1:
                        if cbu1['CBU'] not in common_cbus_iri:
                            common_cbus_iri.append(cbu1['CBU'])
                            common_cbus.append(cbu1)
                        if cbu1['CBU'] in common_cbus_iri:
                            pass
                for list_2 in am_gbu_file_2_items:
                    for cbu2 in list_2:
                        if cbu2['CBU'] not in common_cbus_iri:
                            common_cbus_iri.append(cbu2['CBU'])
                            common_cbus.append(cbu2)
                        if cbu2['CBU'] in common_cbus_iri:
                            pass
                # Allocate files where the CBU information can be storred
                updated_am_gbu_file_1 = FILE_PATHS['r2_cbus']+am_1+"__"+gbu+".json"
                updated_am_gbu_file_2 = FILE_PATHS['r2_cbus']+am_2+"__"+gbu+".json"
                
                # If the file has been already created, it means that only new
                # CBUs that are recorded previously need to be added.
                isFile1 = os.path.isfile(updated_am_gbu_file_1)  
                isFile2 = os.path.isfile(updated_am_gbu_file_2) 
                updater(isFile1, updated_am_gbu_file_1, common_cbus)
                updater(isFile2, updated_am_gbu_file_2, common_cbus)
                
def updater(isFile, updated_am_gbu_file, common_cbus):
    """ This function is part of the loop. For each pair that can exchange CBUs
    we only update their local respective file."""
    
    # if the file does not exist, this will write such a file
    if isFile is False:
        with open(updated_am_gbu_file, 'w+') as write:
            write = json.dumps(common_cbus, indent=4)
            jsonoutput2 = open(updated_am_gbu_file, 'w') 
            jsonoutput2.write(write)
    
    # if the file exists, then access that file
    else:
        with open(updated_am_gbu_file, 'r+') as list:
            list_updated_cbu = json.load(list)
            # Loop separately through the CBUs in the JSON 
            # and through the CBUs in the common CBUs
            # comming from the loop of lib2_creation and append to lists.  
            new_update_cbus_iri = []
            new_update_cbus = [] 

            for cbu_item in list_updated_cbu:           
                if cbu_item['CBU'] not in new_update_cbus_iri:
                    new_update_cbus_iri.append(cbu_item['CBU'])
                    new_update_cbus.append(cbu_item)
                if cbu_item['CBU'] in new_update_cbus_iri:
                    pass

            for cbu_common in common_cbus:
                if cbu_common['CBU'] not in new_update_cbus_iri:
                    new_update_cbus_iri.append(cbu_common['CBU'])
                    new_update_cbus.append(cbu_common)
                if cbu_common['CBU'] in new_update_cbus_iri:
                    pass
            
            # Write the newly updated list under the same name of the file
            with open(updated_am_gbu_file, 'w+') as write:
                write = json.dumps(new_update_cbus,indent=4)
                newjsonoutput = open(updated_am_gbu_file, 'w') 
                newjsonoutput.write(write)

def add_unchanged_files(uniques):
    """The R2 list records only the GBU-AM lists of assembly models that have a CBU in common.
    Therefore some files GBU-AM lists are not affect and thus need to be copied as they are from R1."""
    
    # using the list of assembly models and the R1 list, finds out which files have not been 
    # created by function lib2_creation. Then only these files are coped directly to the 
    # r2 directory. 
    
    r1_list_jsonpath = FILE_PATHS['list_R1']
    with open(r1_list_jsonpath , 'r') as jsonfile:
        data = json.load(jsonfile)
    for model in uniques:
        for item in data: 
            if model in item.keys():
                gbus = item[model]
                gbu1 = gbus[0]
                gbu2 = gbus[1]
                updated_am_gbu_file_1 = FILE_PATHS['r2_cbus']+model+"__"+gbu1+".json"
                updated_am_gbu_file_2 = FILE_PATHS['r2_cbus']+model+"__"+gbu2+".json"
                r1_file1 = FILE_PATHS['r1_cbus']+model+"__"+gbu1+".json"
                r1_file2 = FILE_PATHS['r1_cbus']+model+"__"+gbu2+".json"
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

                