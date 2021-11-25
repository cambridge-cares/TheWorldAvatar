import json
import os.path
from z_paths.file_paths import FILE_PATHS

def lib2_creation(list_pregbus):
    list_R2_filePath = FILE_PATHS['list_R2']    
    with open(list_R2_filePath, 'r+') as list_R2:
        data = json.load(list_R2)
        for gbu in list_pregbus: # you are at the level of gbu 
            overlap_list  = data[gbu]
            for overlap in overlap_list:
                am_1 = overlap[0]
                am_2 = overlap[1]
                am_gbu_file_1 = FILE_PATHS['mops_lib1_type']+am_1+"__"+gbu+".json"
                am_gbu_file_2 = FILE_PATHS['mops_lib1_type']+am_2+"__"+gbu+".json"
                am_gbu_file_1_items = []
                am_gbu_file_2_items = []
                with open(am_gbu_file_1, 'r+') as contents1:
                    data1 = json.load(contents1)
                    am_gbu_file_1_items.append(data1)
                with open(am_gbu_file_2, 'r+') as contents2:
                    data2 = json.load(contents2)
                    am_gbu_file_2_items.append(data2)    
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
                updated_am_gbu_file_1 = FILE_PATHS['mops_lib2_type']+am_1+"__"+gbu+".json"
                updated_am_gbu_file_2 = FILE_PATHS['mops_lib2_type']+am_2+"__"+gbu+".json"
                isFile1 = os.path.isfile(updated_am_gbu_file_1)  
                isFile2 = os.path.isfile(updated_am_gbu_file_2) 
                updater(isFile1, updated_am_gbu_file_1, common_cbus)
                updater(isFile2, updated_am_gbu_file_2, common_cbus)
                
def updater(isFile, updated_am_gbu_file, common_cbus):
    if isFile is False:
        with open(updated_am_gbu_file, 'w+') as write:
            write = json.dumps(common_cbus, indent=4)
            jsonoutput2 = open(updated_am_gbu_file, 'w') 
            jsonoutput2.write(write)
    else:
        with open(updated_am_gbu_file, 'r+') as list1:
            list_updated_cbu1 = json.load(list1)
            new_update_cbus_iri = []
            new_update_cbus = []                        
            for cbu_item in list_updated_cbu1:           
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
            with open(updated_am_gbu_file, 'w+') as write:
                write = json.dumps(new_update_cbus,indent=4)
                newjsonoutput = open(updated_am_gbu_file, 'w') 
                newjsonoutput.write(write)