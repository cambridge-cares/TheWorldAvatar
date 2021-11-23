import json
from z_paths.file_paths import FILE_PATHS

def cbu_gbu_lib(model):
    """In previous stage MOPs were segragated based on assembly Model type. This function 
    accesses each model type and extracts cbus that fit in one of two MOP building GBUs. 
    The function returns cbu information and it also saves a local copy."""
    assemblyModelGroupPath = FILE_PATHS['mops_am']+model+'.json'
    cbuLibPath = FILE_PATHS['mops_lib1']
    cbuLibtypePath = FILE_PATHS['mops_lib1_type']
    with open(assemblyModelGroupPath, 'r+') as file:
        data = json.load(file)
        cbu1lib = []
        cbu2lib = []
        cbu1libtype = []
        cbu2libtype = []
        for item in data:
            cbu1name = item['CBU1_Modularity'] + "-" + item['CBU1_Planarity']
            cbu2name = item['CBU2_Modularity'] + "-" + item['CBU2_Planarity']
            cbu1number = item['CBU1_Number']
            cbu2number = item['CBU2_Number']
            cbu1type = item['CBU1_Type']
            cbu2type = item['CBU2_Type']
            mop_symmetry = item['Symmetry']             
            if item['CBU1'] in cbu1lib:
                pass
            else:
                cbu1lib.append(item['CBU2'])
                cbu1libtype.append({"CBU":item['CBU1'], "BindingSite":cbu1type})   
            if item['CBU2'] in cbu2lib:
                pass
            else: 
                cbu2lib.append(item['CBU2'])
                cbu2libtype.append({"CBU":item['CBU2'], "BindingSite":cbu2type})
        cbu1libout = json.dumps(cbu1lib, indent=4)
        cbu2libout = json.dumps(cbu2lib, indent=4)
        cbu1libtypeout = json.dumps(cbu1libtype, indent=4)
        cbu2libtypeout = json.dumps(cbu2libtype, indent=4)
        cbu1modelLibpath = cbuLibPath+model+"__"+cbu1name+'.json'
        cbu2modelLibpath = cbuLibPath+model+"__"+cbu2name+'.json'
        update_jsonoutput = open(cbu1modelLibpath, 'w') 
        update_jsonoutput.write(cbu1libout)
        update_jsonoutput = open(cbu2modelLibpath, 'w') 
        update_jsonoutput.write(cbu2libout)
        cbu1modelLibtypepath = cbuLibtypePath+model+"__"+cbu1name+'.json'
        cbu2modelLibtypepath = cbuLibtypePath+model+"__"+cbu2name+'.json'
        updatetype_jsonoutput = open(cbu1modelLibtypepath, 'w') 
        updatetype_jsonoutput.write(cbu1libtypeout)
        updatetype_jsonoutput = open(cbu2modelLibtypepath, 'w') 
        updatetype_jsonoutput.write(cbu2libtypeout)
    return cbu1lib, cbu1number, cbu2lib, cbu2number, mop_symmetry

