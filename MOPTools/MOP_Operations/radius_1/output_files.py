import csv
import json
from paths.file_paths import FILE_PATHS

###### FILES RELATED TO KG ANALYTICS 1 ######

def analytics1_csv(uniques):
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

###### FILES RELATED TO MOP SEARCH RADIUS 1 ######

def assemblyModel_json(assemblyModel, string):
    assemblyModel_jsonpath = FILE_PATHS['base']
    outAssemblyModel = json.dumps(assemblyModel)
    jsonoutput = open(assemblyModel_jsonpath+string+'.json', 'w') 
    jsonoutput.write(outAssemblyModel)

def assemblyModel_json_ext(assemblyModel, string, frequency):
    assemblyModel_jsonpath = FILE_PATHS['arrange']
    outAssemblyModel = json.dumps(assemblyModel)
    jsonoutput = open(assemblyModel_jsonpath+string+"__"+str(frequency)+'.json', 'w') 
    jsonoutput.write(outAssemblyModel)

def assemblyModel_json_update(string, frequency):
    assemblyModelGroup = []
    refined = []
    path_main = FILE_PATHS['base']+string+'.json'
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
         
def refined_json(string, frequency):
    assemblyModel_jsonpath1 = FILE_PATHS['base']+string+'.json'
    update_json_path2 = FILE_PATHS['arrange']+string+"__"+str(frequency)+'.json'
    with open(update_json_path2, 'r+') as file:
        data = json.load(file)
        refined = []
        for item in data:
            refined.append(item)
        outjson = json.dumps(refined, indent=4)
        update_jsonoutput = open(assemblyModel_jsonpath1, 'w') 
        update_jsonoutput.write(outjson)