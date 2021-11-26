from z_queries.queryKG import querykg
from z_queries.queryTemplates import mop_reference
from z_queries.queryendpoint import SPARQL_ENDPOINTS
from z_paths.file_paths import FILE_PATHS
import json

def searchRadius(uniques, model_numbers):
    """Takes the uniques from the kg analytics, and returns back how many new 
    and how many known references of search radius 1. """
    r1_list_jsonpath = FILE_PATHS['list_R1']
    with open(r1_list_jsonpath , 'r') as jsonfile:
        data = json.load(jsonfile)
    for model in uniques:
        for item in data: 
            for model_n in model_numbers:
                if model in item.keys():
                    if model in model_n.keys():
                        gbus = item[model]
                        numbers = model_n[model]
                        gbu1 = gbus[0]
                        gbu2 = gbus[1]
                        gbu1_number = numbers[0]
                        gbu2_number = numbers[1]
                        assembler_r1(model, gbu1, gbu2, gbu1_number, gbu2_number)
                        assembler_r2(model, gbu1, gbu2, gbu1_number, gbu2_number)
                else:
                    pass
                
def assembler_r1(model, gbu1, gbu2, gbu1_number, gbu2_number):
    """The assembler uses model/building units to create MOPs in a combinatorial fashion.
    The assembler creates strings which are queried."""
    r1_file1 = FILE_PATHS['mops_lib1_type']+model+"__"+gbu1+".json"
    r1_file2 = FILE_PATHS['mops_lib1_type']+model+"__"+gbu2+".json"
    mops_r1 = FILE_PATHS['mops_r1']+model+".json"
    checked_mops = []
    mopFormula = []
    mopFormulaalt = []
    complementary = None
    gbu_cbu1 = None
    gbu_cbu2 = None
    with open(r1_file1 , 'r') as file1:
        gbu_cbu1 = json.load(file1)
    with open(r1_file2 , 'r') as file2:
        gbu_cbu2 = json.load(file2)
    for item1 in gbu_cbu1:
        for item2 in gbu_cbu2:
            mop_symmetry = item1['Symmetry']
            mop_building_1 = (item1['CBU'], gbu1_number)
            binding1 = (item1['BindingSite'],item1['OuterCoordination'],item1['Direction'])
            mop_building_2 = (item2['CBU'], gbu2_number)
            binding2 = (item2['BindingSite'],item2['OuterCoordination'],item2['Direction'])
            complementary = complementarity(binding1, binding2)
            if complementary is True:
                created_mop = mop_builder(mop_building_1, mop_building_2)
                mop_formula_1 = created_mop[0]
                mop_formula_2 = created_mop[1]
                if mop_formula_1 not in mopFormula:
                    if mop_formula_2 not in mopFormula:
                        mop_provenance = query_mopFormula(mop_formula_1, mop_formula_2, mop_symmetry)
                        checked_mops.append(mop_provenance)
                        mopFormula.append(mop_formula_1)
                        mopFormulaalt.append(mop_formula_2)
                else:
                    pass        
        else:
            pass
        mopFormulaOut1 = json.dumps(checked_mops, indent=4)
        mopFormRJson1 = open(mops_r1, 'w') 
        mopFormRJson1.write(mopFormulaOut1)
    return checked_mops

def assembler_r2(model, gbu1, gbu2, gbu1_number, gbu2_number):
    """The assembler uses model/building units to create MOPs in a combinatorial fashion.
    The assembler creates strings which are queried."""
    r2_file_1 = FILE_PATHS['mops_lib2_type']+model+"__"+gbu1+".json"
    r2_file_2 = FILE_PATHS['mops_lib2_type']+model+"__"+gbu2+".json"
    mops_r2 = FILE_PATHS['mops_r2']+model+".json"
    checked_mops = []
    mopFormula = []
    mopFormulaalt = []
    complementary = None
    gbu_cbu1 = None
    gbu_cbu2 = None
    with open(r2_file_1 , 'r') as file1:
        gbu_cbu1 = json.load(file1)
    with open(r2_file_2 , 'r') as file2:
        gbu_cbu2 = json.load(file2)
    for item1 in gbu_cbu1:
        for item2 in gbu_cbu2:
            mop_symmetry = item1['Symmetry']
            mop_building_1 = (item1['CBU'],gbu1_number)
            binding1 = (item1['BindingSite'],item1['OuterCoordination'],item1['Direction'])
            mop_building_2 = (item2['CBU'],gbu2_number)
            binding2 = (item2['BindingSite'],item2['OuterCoordination'],item2['Direction'])
            complementary = complementarity(binding1, binding2)
            if complementary is True:
                created_mop = mop_builder(mop_building_1, mop_building_2)
                mop_formula_1 = created_mop[0]
                mop_formula_2 = created_mop[1]
                if mop_formula_1 not in mopFormula:
                    if mop_formula_2 not in mopFormula:
                        mop_provenance = query_mopFormula(mop_formula_1, mop_formula_2, mop_symmetry)
                        checked_mops.append(mop_provenance)
                        mopFormula.append(mop_formula_1)
                        mopFormulaalt.append(mop_formula_2)
                else:
                    pass        
        else:
            pass
        mopFormulaOut2 = json.dumps(checked_mops, indent=4)
        mopFormRJson2 = open(mops_r2, 'w') 
        mopFormRJson2.write(mopFormulaOut2)
    return checked_mops

def complementarity(binding1, binding2):
    bindingSite1 = binding1[0]
    bindingSite2 = binding2[0]
    outerCoordination1 = binding1[1]
    outerCoordination2 = binding2[1]
    directio1 = binding1[2]
    directio2 = binding2[2]
    complementary = None
    if bindingSite1 != bindingSite2:
        if outerCoordination1 == outerCoordination2:
            if directio1 == directio2:
                complementary = True
    else:
        complementary = False
    return complementary

def mop_builder(mop_building_1, mop_building_2):
    cbu1 = mop_building_1[0]
    cbu1_number = mop_building_1[1]
    cbu2 = mop_building_2[0]
    cbu2_number = mop_building_2[1]
    mop_string = str(cbu1+cbu1_number+cbu2+cbu2_number)
    alt_mop_string = str(cbu2+cbu2_number+cbu1+cbu1_number)
    return mop_string, alt_mop_string

def query_mopFormula(mopFormula, altmopFormula, mop_symmetry):
    """We query formulas. Formulas that are in the KG are labeled with their DOI, the
    MOP Formulas that are new are labeled with NEW."""
    checked = []
    result1  = querykg(SPARQL_ENDPOINTS['ontomops'], mop_reference(mopFormula, mop_symmetry))
    result2  = querykg(SPARQL_ENDPOINTS['ontomops'], mop_reference(altmopFormula, mop_symmetry))
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
