'''
Created on Dec 1, 2021

@author: Aleksandar Kondinski
'''

import json
from manager.file_paths import FILE_PATHS
from query_operations.queryendpoint import SPARQL_ENDPOINTS
from query_operations.queryKG import querykg
from query_operations.queryTemplates import species_properties
    
def lib1_Creation(uniques):
    """Separates CBUs based on GBU of an AM. For each CBU queries species information
    needed in the assembly operations. Saves all information locally in files."""
    
    # Loopig through the list of unique assembly models and for each assembly model
    # accessing the json file
    for model in uniques: 
        assemblyModelGroupPath = FILE_PATHS['mops_am']+model+'.json' # accesses the new input fule
        cbus_r1_file_path = FILE_PATHS['r1_cbus']
        with open(assemblyModelGroupPath, 'r+') as file:
            data = json.load(file)
            cbu1lib = []
            cbu2lib = []
            cbu1_list = []
            cbu2_list = []
            # Looping through the MOPs associated with a particular Assembly Model
            for item in data:
                cbu1name = item['CBU1_Modularity'] + "-" + item['CBU1_Planarity']
                cbu2name = item['CBU2_Modularity'] + "-" + item['CBU2_Planarity']
                
                # For the CBUs of each MOP query their species information. 
                cbu1_species = query_speciesIRI(item['CBU1_SpeciesIRI'])
                cbu1_mw = cbu1_species[0]
                cbu1_charge = cbu1_species[1]
                cbu2_species = query_speciesIRI(item['CBU2_SpeciesIRI'])  
                cbu2_mw = cbu2_species[0]
                cbu2_charge = cbu2_species[1]
                
                # Append CBU to a list and store its infromation as a dictionary         
                if item['CBU1'] in cbu1lib:
                    pass
                if item['CBU1'] not in cbu1lib:
                    cbu1lib.append(item['CBU1'])
                    cbu1_list.append({"CBU":item['CBU1'],
                        "Modularity":item['CBU1_Modularity'],
                        "Planarity":item['CBU1_Planarity'],
                        "Symmetry":item['Symmetry'],
                        "BindingSite":item['CBU1_Type'],
                        "SpeciesIRI":item['CBU1_SpeciesIRI'],
                        "OuterCoordination":item['CBU1_OuterCoordination'],
                        "Direction":item['CBU1_Direction'],
                        "FunctionalGroup":item['CBU1_FunctionalGroup'],
                        "Charge":cbu1_charge,
                        "MolecularWeight":cbu1_mw})   
                if item['CBU2'] in cbu2lib:
                    pass
                if item['CBU2'] not in cbu2lib: 
                    cbu2lib.append(item['CBU2'])
                    cbu2_list.append({"CBU":item['CBU2'],
                        "Modularity":item['CBU2_Modularity'],
                        "Planarity":item['CBU2_Planarity'],
                        "Symmetry":item['Symmetry'],
                        "BindingSite":item['CBU2_Type'],
                        "SpeciesIRI":item['CBU2_SpeciesIRI'],
                        "OuterCoordination":item['CBU2_OuterCoordination'],
                        "Direction":item['CBU2_Direction'],
                        "FunctionalGroup":item['CBU2_FunctionalGroup'],
                        "Charge":cbu2_charge,
                        "MolecularWeight":cbu2_mw})

            # Write the CBUs as local files that can be accessed later by the Assemlber   
            cbu1_listout = json.dumps(cbu1_list, indent=4)
            cbu2_listout = json.dumps(cbu2_list, indent=4)
            cbu1_list_file_path = cbus_r1_file_path+model+"__"+cbu1name+'.json'
            cbu2_list_file_path = cbus_r1_file_path+model+"__"+cbu2name+'.json'
            updatetype_jsonoutput = open(cbu1_list_file_path, 'w') 
            updatetype_jsonoutput.write(cbu1_listout)
            updatetype_jsonoutput = open(cbu2_list_file_path, 'w') 
            updatetype_jsonoutput.write(cbu2_listout)

def query_speciesIRI(cbuiri):
    """For each CBU queries its OntoSpecies Information."""
    result  = querykg(SPARQL_ENDPOINTS['ontospecies'], species_properties(cbuiri)) #The query gets a full list of IRIs
    cbu_properties = [] # Each IRI is saved in the list of refined IRIs
    if result:
        if 'MolecularWeightValue' in result[0].keys():
            cbu_mol_weight = result[0]['MolecularWeightValue']
            cbu_charge = result[0]['MolecularChargeValue']
            cbu_properties.append(cbu_mol_weight)
            cbu_properties.append(cbu_charge)
        if not result:
            pass
        return cbu_properties