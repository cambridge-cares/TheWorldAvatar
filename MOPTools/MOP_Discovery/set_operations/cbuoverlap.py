'''
Created on Dec 1, 2021

@author: Aleksandar Kondinski
'''

import json
import itertools
from manager.file_paths import FILE_PATHS

def cbuoverlap(list_gbus):
    """Accesses the local copy of preR2 and finds which gbus of 
    an assembly model have a CBU in common using a combinatorial tool. 
    Pairs that share cbu in common are saved in R2 list."""
    
    # Access to the list of preR2
    list_preR2_filePath = FILE_PATHS['list_preR2']
    with open(list_preR2_filePath, 'r+') as list_preR2:
        data = json.load(list_preR2)
        list_R2 = {}

        # Loops over the list of GBUs, and using a GBU string as a key access
        # the list of related assembly model strings. 
        for gbu_string in list_gbus: # you are at the level of gbu 
            gbu_am_lib = data[gbu_string ]
            am_overlaping = []

            # using combinations create a list of assembly model pairs 
            for ampairs in  itertools.combinations(gbu_am_lib,2):  # you are at the level of an assembly model  
                # select original pairs by checking for reversed order
                originalpairs = []
                reversedlist = [ampairs[1], ampairs[0]]
                reversedpair = tuple(reversedlist)
                
                # if pair of assembly models is allocate their respective gbu-am file
                if ampairs[0] == ampairs[1]:
                    pass
                if ampairs[0] != ampairs[1]:
                    if reversedpair not in originalpairs:
                        file1 = FILE_PATHS['r1_cbus']+ampairs[0]+"__"+gbu_string+".json"
                        file2 = FILE_PATHS['r1_cbus']+ampairs[1]+"__"+gbu_string+".json"
                        set1 = []
                        set2 = []
                        subset = []
                        # open the respective files each containing cbus
                        with open(file1, 'r+') as cbulib1:
                            cbus1 = json.load(cbulib1)
                            for line1 in cbus1:
                                set1.append(line1['CBU'])
                        with open(file2, 'r+') as cbulib2:
                            cbus2 = json.load(cbulib2)
                            for line2 in cbus2:
                                set2.append(line2['CBU'])
                        
                        # append their CBUs to two lists/sets of CBUs and compare if same 
                        for instance1 in set1:
                            for instance2 in set2:
                                # if same append the common cbu to the "subset" list
                                if instance1 == instance2:
                                    subset.append(instance1)
                                if instance1 != instance2:
                                    pass
                        
                        # if the subset list of common CBUs is not empty, append the pair of AMs to a list of overlaps
                        if len(subset) > 0: 
                            am_overlaping.append(ampairs)
                    originalpairs.append(ampairs)
                    
                    # Create a dictionary ordered by gbu string listing AM pairs that have a common CBU
                    mydict = {gbu_string:am_overlaping}
                list_R2.update(mydict)

        # Write the R2 list as a form of a json file
        list_R2_jsonpath = FILE_PATHS['list_R2']
        outR2 = json.dumps(list_R2, indent=4)
        jsonoutput = open(list_R2_jsonpath, 'w') 
        jsonoutput.write(outR2)
