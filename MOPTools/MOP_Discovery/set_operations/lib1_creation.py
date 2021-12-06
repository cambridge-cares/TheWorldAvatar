import json
from manager.file_paths import FILE_PATHS
from query_operations.queryendpoint import SPARQL_ENDPOINTS
from query_operations.queryKG import querykg
from query_operations.queryTemplates import species_properties

def lib1_Creation(uniques):
   """For a library of assembly models
   creates two libraries of CBU files separated based on GBU type."""
   for model in uniques: # The function accesses saved json files
        cbu_gbu_lib(model)
        print(model+"_____________"+"conveted to json")
    
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
            mop_symmetry = item['Symmetry']
            cbu1iri = item['CBU1_SpeciesIRI']
            cbu1_species = query_speciesIRI(cbu1iri)
            cbu1_mw = cbu1_species[0]
            cbu1_charge = cbu1_species[1]
            cbu2iri = item['CBU2_SpeciesIRI']
            cbu2_species = query_speciesIRI(cbu2iri)  
            cbu2_mw = cbu2_species[0]
            cbu2_charge = cbu2_species[1]         
            if item['CBU1'] in cbu1lib:
                pass
            if item['CBU1'] not in cbu1lib:
                cbu1lib.append(item['CBU1'])
                cbu1libtype.append({"CBU":item['CBU1'],
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
                cbu2libtype.append({"CBU":item['CBU2'],
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

def query_speciesIRI(cbuiri):
    """This function runs preset query that returns back all of the MOP IRIs found in the OntoMOP KG"""
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