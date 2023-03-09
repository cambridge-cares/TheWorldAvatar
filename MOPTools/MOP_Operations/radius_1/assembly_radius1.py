from kgoperations.queryKG import querykg
from kgoperations.queryTemplates import mop_reference
from radius_1.assembly_lib import cbu_gbu_lib
from kgoperations.queryendpoint import SPARQL_ENDPOINTS
from paths.file_paths import FILE_PATHS
import json

def searchRadius1(uniques):
    """Takes the uniques from the kg analytics, and returns back how many new 
    and how many known references of search radius 1. """
    mops_output_overviewR1 = []
    for model in uniques: # The function accesses saved json files
        model_libs = cbu_gbu_lib(model) # from the json files we extract CBU/GBU information i.e., we create building unit libraries
        model_gbu1 = model_libs[0] 
        model_gbu1num = model_libs[1]
        model_gbu2 = model_libs[2] 
        model_gbu2num = model_libs[3]
        assembler_r1(model, model_gbu1,model_gbu1num, model_gbu2, model_gbu2num) # Assembles and queries to get DOIs
        r1_analytics = overview_radius1(model) # this is just analytical tool. It prints out how many are new and how many known. 
        mops_output_overviewR1.append(r1_analytics)
    return mops_output_overviewR1
   
def assembler_r1(model, model_gbu1,model_gbu1num, model_gbu2, model_gbu2num):
    """The assembler uses model/building units to create MOPs in a combinatorial fashion.
    The assembler creates strings which are queried."""
    mopFormR1Path = FILE_PATHS['mops_r1']
    mopFormulaR1 = []
    for cbu1 in model_gbu1:
        for cbu2 in  model_gbu2:
            # function on complementarity to be added !!!!!!!!!!!!!
            mopFormula = str(cbu1+model_gbu1num+cbu2+model_gbu2num)
            altmopFormula = str(cbu2+model_gbu2num+cbu1+model_gbu1num)
            if mopFormula in mopFormulaR1: # If MOP formula is in the key
                pass
            else:
                provenance = query_mopFormula(mopFormula, altmopFormula) # We run query on both formulas to avoid possible duplication
                mopFormulaR1.append(provenance)
    mopFormulaOut = json.dumps(mopFormulaR1, indent=4)
    mopFormR1Json = open(mopFormR1Path+model+"__R1.json", 'w') 
    mopFormR1Json.write(mopFormulaOut)
    #return mopFormulaR1

def query_mopFormula(mopFormula, altmopFormula):
    """We query formulas. Formulas that are in the KG are labeled with their DOI, the
    MOP Formulas that are new are labeled with NEW."""
    checked = []
    result1  = querykg(SPARQL_ENDPOINTS['ontomops'], mop_reference(mopFormula))
    result2  = querykg(SPARQL_ENDPOINTS['ontomops'], mop_reference(altmopFormula))
    if result1:
        if 'MOPReference' in result1[0].keys():
            provenance = result1[0]['MOPReference']
            checked.append({mopFormula:provenance})
    if not result1:
        if result2:
            if 'MOPReference' in result2[0].keys():
                provenance = result2[0]['MOPReference']
                checked.append({altmopFormula:provenance})                
    if not result2:
        if not result1:
            provenance = "NEW"
            checked.append({mopFormula:provenance})    
    print(checked)
    return checked

def overview_radius1(model):
    """Provides an overview on MOPs obtained from searh radius 1."""
    assemblyModelGroupPath = FILE_PATHS['mops_r1']+model+"__R1.json"
    with open(assemblyModelGroupPath, 'r+') as file:
        data = json.load(file)
        new = 0
        known = 0 
        for item in data:
            print(item)
            for pair in item:
                x = list(pair.values())
                if "NEW" in x:
                    new += 1
                if "NEW" not in x:
                    known += 1
    model_analytics_r1 = {"Model Type":model, "NEW":new, "KNOWN":known}
    return model_analytics_r1


