import json
from paths.file_paths import FILE_PATHS

def cbu_gbu_lib(model):
    """In previous stage MOPs were segragated based on assembly Model type. This function 
    accesses each model type and extracts cbus that fit in one of two MOP building GBUs. 
    The function returns cbu information and it also saves a local copy."""
    assemblyModelGroupPath = FILE_PATHS['base']+model+'.json'
    cbuLibPath = FILE_PATHS['cbu_gbu_lib']
    with open(assemblyModelGroupPath, 'r+') as file:
        data = json.load(file)
        cbu1lib = []
        cbu2lib = []
        for item in data:
            cbu1name = item['CBU1_Modularity'] + "-" + item['CBU1_Planarity']
            cbu2name = item['CBU2_Modularity'] + "-" + item['CBU2_Planarity']
            cbu1number = item['CBU1_Number']
            cbu2number = item['CBU2_Number']
            if item['CBU1'] in cbu1lib:
                pass
            else:
                cbu1lib.append(item['CBU1'])
            if item['CBU2'] in cbu2lib:
                pass
            else: 
                cbu2lib.append(item['CBU2'])
        cbu1libout = json.dumps(cbu1lib, indent=4)
        cbu2libout = json.dumps(cbu2lib, indent=4)
        cbu1modelLibpath = cbuLibPath+model+"__"+cbu1name+'.json'
        cbu2modelLibpath = cbuLibPath+model+"__"+cbu2name+'.json'
        update_jsonoutput = open(cbu1modelLibpath, 'w') 
        update_jsonoutput.write(cbu1libout)
        update_jsonoutput = open(cbu2modelLibpath, 'w') 
        update_jsonoutput.write(cbu2libout)
    return cbu1lib, cbu1number, cbu2lib, cbu2number

