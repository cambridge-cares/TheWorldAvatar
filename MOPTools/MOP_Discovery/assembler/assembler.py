'''
Created on Dec 1, 2021

@author: Aleksandar Kondinski
'''

from query_operations.queryKG import querykg
from query_operations.queryTemplates import mop_reference
from query_operations.queryendpoint import SPARQL_ENDPOINTS
from manager.file_paths import FILE_PATHS
import csv
import json

def searchRadius(uniques, model_numbers):
    """Takes the uniques from the kg analytics, and returns back how many new 
    and how many known references of search radius 1. """
    
    # accesses the R1 file as input and creates empty lists for the generation of MOPs
    r1_list_jsonpath = FILE_PATHS['list_R1']
    r1_mops = []
    r2_mops = []
    mops_output_overviewR1 = []
    mops_output_overviewR2 = []
    with open(r1_list_jsonpath , 'r') as jsonfile:
        data = json.load(jsonfile)
    
    # loops over the assembly model strings derived from the KG overview
    for model in uniques:
        # loops over the assembly model pairs in the R1 list json file
        for item in data: 
            # loops over the AM in order to find the associated GBU numbers
            for model_n in model_numbers:
                # if the assemly model string is in the other two lists, the GBU and GBU number information and combined
                if model in item.keys():
                    if model in model_n.keys():
                        gbus = item[model] # uses the assembly model string to access GBU/CBU data
                        numbers = model_n[model] # uses the assembly model string to access GBU number and symmetry
                        gbu1 = gbus[0]
                        gbu2 = gbus[1]
                        gbu1_number = numbers[0]
                        gbu2_number = numbers[1]
                        mop_symmetry = gbus[2]

                        # Defines the input and output file paths for the assembler
                        paths_r1 = (FILE_PATHS['r1_cbus'], FILE_PATHS['mops_r1'])
                        paths_r2 = (FILE_PATHS['r2_cbus'], FILE_PATHS['mops_r2'])
                        
                        # Pass data to the assembler, and return back list of MOPs and MOP data with
                        # checked provenance. 
                        mops_r1 = assembler(paths_r1, model, gbu1, gbu2, gbu1_number, gbu2_number, mop_symmetry)
                        mops_r2 = assembler(paths_r2, model, gbu1, gbu2, gbu1_number, gbu2_number, mop_symmetry)
                        
                        # For each assembly model create a dictionary with information of the AM and the list of MOPs/MOP data
                        am_radius1 = {'Assembly Model':model, 'Set': mops_r1}
                        am_radius2 = {'Assembly Model':model, 'Set': mops_r2}
                        
                        # Append the data to a radius list
                        r1_mops.append(am_radius1)
                        r2_mops.append(am_radius2)
                        
                        # Provide an overview of how assemblies are in the KG and how many are not
                        r1_analytics = overview_radius(paths_r1, model)
                        r2_analytics = overview_radius(paths_r2, model)
                        mops_output_overviewR1.append(r1_analytics)
                        mops_output_overviewR2.append(r2_analytics)
                else:
                    pass
    # Store all MOPs on a csv file 
    mops_csv(1, r1_mops)
    mops_csv(2, r2_mops)
    # Provide overview of the R1 and R2 phases.   
    searchRoverview(mops_output_overviewR1, mops_output_overviewR2)
    return mops_output_overviewR1, mops_output_overviewR2
 
            
def assembler(paths, model, gbu1, gbu2, gbu1_number, gbu2_number, mop_symmetry):
    """The assembler uses model/building units to create MOPs in a combinatorial fashion.
    The assembler creates strings which are queried."""
    # Constructs the paths to the library files containing CBUs
    file_lib1 = paths[0]+model+"__"+gbu1+".json"
    file_lib2 = paths[0]+model+"__"+gbu2+".json"
    mops = paths[1]+model+".json"
    # List of variables 
    checked_mops = [] # MOP for which provenance has been collected
    mopFormula = [] 
    mopFormulaalt = []
    complementary = None
    gbu_cbu1 = None
    gbu_cbu2 = None
    # Open the library files and loop over pairs of CBUs
    with open(file_lib1, 'r') as file1:
        gbu_cbu1 = json.load(file1)
    with open(file_lib2, 'r') as file2:
        gbu_cbu2 = json.load(file2)
    for item1 in gbu_cbu1:
        for item2 in gbu_cbu2:
            mop_building_1 = (item1['CBU'], gbu1_number)
            cbu1_mw = float(item1['MolecularWeight'])
            cbu1_charge = float(item1['Charge'])
            binding1 = (item1['BindingSite'],item1['OuterCoordination'],item1['Direction'])
            mop_building_2 = (item2['CBU'], gbu2_number)
            cbu2_mw = float(item2['MolecularWeight'])
            cbu2_charge = float(item2['Charge'])
            binding2 = (item2['BindingSite'],item2['OuterCoordination'],item2['Direction'])
            
            # Find out if the pair is complementary and thus a MOP can be constructed
            complementary = complementarity(binding1, binding2)
            if complementary is True:
                # if the CBUs are complementary create strings of MOP formula
                created_mop = mop_string_writer(mop_building_1, mop_building_2)
                mop_formula_1 = created_mop[0]
                mop_formula_2 = created_mop[1]
                # If the MOP formula has not been created in a previous step calculate its mass
                # charge and find out if it is known in the KG. Wrap the information 
                if mop_formula_1 not in mopFormula:
                    if mop_formula_2 not in mopFormula:
                        mop_weight = cbu1_mw*int(gbu1_number)+cbu2_mw*int(gbu2_number)
                        mop_charge = cbu1_charge*int(gbu1_number)+cbu2_charge*int(gbu2_number)
                        mop_provenance = mop_data_wrapper(mop_formula_1, mop_formula_2, mop_symmetry, mop_weight,mop_charge)
                        checked_mops.append(mop_provenance)
                        mopFormula.append(mop_formula_1)
                        mopFormulaalt.append(mop_formula_2)
                else:
                    pass        
        else:
            pass
        # Writes a local json file listing all constructed MOPs. 
        mopFormulaOut = json.dumps(checked_mops, indent=4)
        mopFormRJson = open(mops, 'w') 
        mopFormRJson.write(mopFormulaOut)
    return checked_mops

def complementarity(binding1, binding2):
    """Checks if two CBUs can be conencted in a complementary fashion."""
    # Difference in binding sites
    bindingSite1 = binding1[0]
    bindingSite2 = binding2[0]
    # The outer coordination number should be same to avoid atom and bond defects
    outerCoordination1 = binding1[1]
    outerCoordination2 = binding2[1]
    # The orientation of the binding sites should be alligned
    direction1 = binding1[2]
    direction2 = binding2[2]
    # if these three hold than the two CBUs can be considered complementary
    complementary = None
    if bindingSite1 != bindingSite2:
        if outerCoordination1 == outerCoordination2:
            if direction1 == direction2:
                complementary = True
    else:
        complementary = False
    return complementary

def mop_string_writer(mop_building_1, mop_building_2):
    """Creates a string of a MOP and its alternative formula 
    by exchanging the positions of the CBUs."""
    cbu1 = mop_building_1[0]
    cbu1_number = mop_building_1[1]
    cbu2 = mop_building_2[0]
    cbu2_number = mop_building_2[1]
    mop_string = str(cbu1+cbu1_number+cbu2+cbu2_number)
    alt_mop_string = str(cbu2+cbu2_number+cbu1+cbu1_number)
    return mop_string, alt_mop_string

def mop_data_wrapper(mopFormula, altmopFormula, mop_symmetry, mop_weight,mop_charge):
    """First queries each MOP formula string, then wraps the provenanve, molecular weight and charge."""
    checked = []
    result1  = querykg(SPARQL_ENDPOINTS['ontomops'], mop_reference(mopFormula, mop_symmetry))
    result2  = querykg(SPARQL_ENDPOINTS['ontomops'], mop_reference(altmopFormula, mop_symmetry))
    if result1:
        if 'MOPReference' in result1[0].keys():
            provenance = result1[0]['MOPReference']
            checked.append({"MOPFormula":mopFormula,
            "ReferenceDOI":provenance,
            "MOPWeight":mop_weight,
            "MOPCharge":mop_charge}) 
    if not result1:
        if result2:
            if 'MOPReference' in result2[0].keys():
                provenance = result2[0]['MOPReference']
                checked.append({"MOPFormula":mopFormula,
                "ReferenceDOI":provenance,
                "MOPWeight":mop_weight,
                "MOPCharge":mop_charge})             
    if not result2:
        if not result1:
            provenance = "Not in OntoMOPs KG"
            checked.append({"MOPFormula":mopFormula,
            "ReferenceDOI":provenance,
            "MOPWeight":mop_weight,
            "MOPCharge":mop_charge})    
    print(checked)
    return checked

def overview_radius(paths, model):
    """Provides an overview on MOPs obtained from searh radius 1."""
    assemblyModelGroupPath = paths[1]+model+".json"
    with open(assemblyModelGroupPath, 'r+') as file:
        data = json.load(file)
        new = 0
        known = 0 
        for item in data:
            for pair in item:
                x = list(pair.values())
                if "Not in OntoMOPs KG" in x:
                    new += 1
                if "Not in OntoMOPs KG" not in x:
                    known += 1
    model_analytics = {"Assembly Model":model, "Not in KG":new, "In KG":known}
    return model_analytics

def searchRoverview(mops_output_overviewR1, mops_output_overviewR2):
    """Exports CSV that provides an overview of the number of MOPs in the KG 
    and those not in the KG."""

    csv_file_path = FILE_PATHS['r1andr2_csv']
    r1_r2_model = {}
    with open(csv_file_path, 'w', newline='') as csvfile:
        fieldnames = ['Assembly Model', 'R1 Not In KG','R1 In KG','R2 Not In KG','R2 In KG']
        writer = csv.DictWriter(csvfile, fieldnames=fieldnames)
        writer.writeheader()
        my_dict = {}
        for item1 in mops_output_overviewR1:
            model = str(item1["Assembly Model"])
            new_r1 = int(item1["Not in KG"])
            known_r1 = int(item1["In KG"])
            for item2 in mops_output_overviewR2:
                if model == str(item2["Assembly Model"]):
                    new_r2 = int(item2["Not in KG"])
                    known_r2 = int(item2["In KG"])
                    r1_r2_model['Assembly Model'] = model
                    r1_r2_model['R1 Not In KG'] = new_r1
                    r1_r2_model['R1 In KG'] = known_r1
                    r1_r2_model['R2 Not In KG'] = new_r2
                    r1_r2_model['R2 In KG'] = known_r2
                    my_dict.update(r1_r2_model)
                    writer.writerow(my_dict)
                else:
                    pass

def mops_csv(r, r_mops):
    """Exports csv files for the whole set MOPs under R1 and R2."""
    if r == 1:
        csv_file_path = FILE_PATHS['r1_mops_csv']
    if r == 2:
        csv_file_path = FILE_PATHS['r2_mops_csv']
    mops = {}
    with open(csv_file_path, 'w', newline='') as csvfile:
        fieldnames = ['Assembly Model', 'MOP Formula', 'MOP Charge', 'MOP MW', 'ReferenceDOI']
        writer = csv.DictWriter(csvfile, fieldnames=fieldnames)
        writer.writeheader()
        my_dict = {}
        for mops_radius in r_mops:
            set_mops = mops_radius['Set']
            for list_item in set_mops:
                for item in list_item:
                    print(item)
                    mops['Assembly Model'] = mops_radius['Assembly Model']
                    mops['MOP Formula'] = item['MOPFormula']
                    mops['MOP Charge'] = item["MOPCharge"] 
                    mops['MOP MW'] = item["MOPWeight"]
                    mops['ReferenceDOI'] = item["ReferenceDOI"]
                    my_dict.update(mops)
                    writer.writerow(my_dict)
            else:
                pass