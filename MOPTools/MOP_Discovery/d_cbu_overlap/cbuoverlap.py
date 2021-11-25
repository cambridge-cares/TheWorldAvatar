import json
import  itertools
from z_paths.file_paths import FILE_PATHS

def cbuoverlap(list_pregbus):
    list_preR2_filePath = FILE_PATHS['list_preR2']
    #ssemblyModelGroupPath = FILE_PATHS['mops_am']+model+'.json'
    #cbuLibtypePath = FILE_PATHS['mops_lib1_type']
    with open(list_preR2_filePath, 'r+') as list_preR2:
        data = json.load(list_preR2)
        list_R2 = {}
        for item in list_pregbus: # you are at the level of gbu 
            gbu_am_lib = data[item]
            #am_cbus = []
            overlaps = []
            for ampairs in  itertools.combinations(gbu_am_lib,2):  # you are at the level of an assembly model  
                originalpairs = []
                reversedlist = [ampairs[1], ampairs[0]]
                reversedpair = tuple(reversedlist)
                if ampairs[0] == ampairs[1]:
                    pass
                if ampairs[0] != ampairs[1]:
                    if reversedpair not in originalpairs:
                        file1 = FILE_PATHS['mops_lib1_type']+ampairs[0]+"__"+item+".json"
                        file2 = FILE_PATHS['mops_lib1_type']+ampairs[1]+"__"+item+".json"
                        set1 = []
                        set2 = []
                        subset = []
                        with open(file1, 'r+') as cbulib1:
                            cbus1 = json.load(cbulib1)
                            for line1 in cbus1:
                                set1.append(line1['CBU'])
                        with open(file2, 'r+') as cbulib2:
                            cbus2 = json.load(cbulib2)
                            for line2 in cbus2:
                                set2.append(line2['CBU'])
                        for instance1 in set1:
                            for instance2 in set2:
                                if instance1 == instance2:
                                    subset.append(instance1)
                                if instance1 != instance2:
                                    pass
                        if len(subset) > 0: 
                            overlaps.append(ampairs)
                    originalpairs.append(ampairs)
                    mydict = {item:overlaps}
                list_R2.update(mydict)
        list_R2_jsonpath = FILE_PATHS['list_R2']
        outR2 = json.dumps(list_R2)
        jsonoutput = open(list_R2_jsonpath, 'w') 
        jsonoutput.write(outR2)
        print(list_R2)
