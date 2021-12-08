'''
Created on Dec 1, 2021

@author: Aleksandar Kondinski
'''

import csv
import json
from manager.file_paths import FILE_PATHS

def kgoverview_csv(uniques):
    """This function produces a csv file with an overview of the MOP
     assembly Models present in the KG and their frequency of occurance."""
    csv_file_path = FILE_PATHS['kg_assembly_csv']
    kg_model = {}
    with open(csv_file_path, 'w', newline='') as csvfile:
        fieldnames = ['Assembly Model', 'Occurance in KG']
        writer = csv.DictWriter(csvfile, fieldnames=fieldnames)
        writer.writeheader()
        my_dict = {}
        for item in uniques:
            x = str(item)
            y = int(uniques.get(x))
            kg_model['Assembly Model'] = x
            kg_model['Occurance in KG'] = int(y)
            my_dict.update(kg_model)
            writer.writerow(my_dict)

def r1_json(list_R1):
    """Produces a json file with library of MOP assebmly models
    their respective building units and symmetry."""
    list_R1_jsonpath = FILE_PATHS['list_R1']
    outpreR1 = json.dumps(list_R1)
    jsonoutput = open(list_R1_jsonpath, 'w') 
    jsonoutput.write(outpreR1)

def preR2_json(list_preR2):
    """Produces a json file with library of generic building units
    and their respective occurance in assembly models."""
    list_preR2_jsonpath = FILE_PATHS['list_preR2']
    outpreR2 = json.dumps(list_preR2)
    jsonoutput = open(list_preR2_jsonpath, 'w') 
    jsonoutput.write(outpreR2)
    
def assemblyModel_json(assemblyModel, string):
    """Produces a starting and final json file with library MOPs and their respective properties."""
    assemblyModel_jsonpath = FILE_PATHS['mops_am']
    outAssemblyModel = json.dumps(assemblyModel)
    jsonoutput = open(assemblyModel_jsonpath+string+'.json', 'w') 
    jsonoutput.write(outAssemblyModel)

def assemblyModel_json_ext(assemblyModel, string, frequency):
    """For each assemly model produces a temporary file."""
    assemblyModel_jsonpath = FILE_PATHS['arrange']
    outAssemblyModel = json.dumps(assemblyModel)
    jsonoutput = open(assemblyModel_jsonpath+string+"__"+str(frequency)+'.json', 'w') 
    jsonoutput.write(outAssemblyModel)

def assemblyModel_json_update(string, frequency):
    """Merges the temporary json file to the original and final json."""
    assemblyModelGroup = []
    refined = []
    path_main = FILE_PATHS['mops_am']+string+'.json'
    i = 0
    while i < frequency:
        path_update = FILE_PATHS['arrange']+string+"__"+str(i)+'.json'
        with open(path_update, 'r+') as file:    
            data = json.load(file)
            mopIRI = data['mopIRI']
            if mopIRI not in refined:
                refined.append(mopIRI)
                assemblyModelGroup.append(data)
            else:
                pass
        i += 0.5
    updated = json.dumps(assemblyModelGroup, indent=4)
    jsonoutput = open(path_main, 'w') 
    jsonoutput.write(updated)