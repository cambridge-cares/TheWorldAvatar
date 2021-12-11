'''
Created on Dec 1, 2021

@author: Aleksandar Kondinski
'''

from query_operations.queryKG import querykg
from query_operations.queryendpoint import SPARQL_ENDPOINTS
from query_operations.queryTemplates import getMOPIRIs
from query_operations.queryTemplates import mop_GBUs
from kg_overview.analytics_output import assemblyModel_json
from kg_overview.analytics_output import assemblyModel_json_temp
from kg_overview.analytics_output import assemblyModel_json_update
from kg_overview.analytics_output import r1_json
from kg_overview.analytics_output import preR2_json

def mopsoverview():
    """Collects all MOP IRIs found in the OntoMOP KG"""
    result  = querykg(SPARQL_ENDPOINTS['ontomops'], getMOPIRIs())
    refinedlist = [] # Each IRI is saved in the list of refined IRIs
    for item in result:
        refined = item['mopIRI']
        refinedlist.append(refined)
    return refinedlist 

def assemblyModelGroups(listofMOPs):
    """ Main KG operation function:
    Input: For each MOR IRI collects further information. 
    Operation: Passes information furhter to derive derives the Assembly Model am_string based on the collected information. 
    Output 1: json files assembly models containing instances of MOPs and their information. 
    Output 2: List of all assembly models for furhter set operations (R1). 
    Output 3: List of GBUs with associated Assembly Models (pre-R2) for further set operations."""
    
    # List of variables 
    uniques = {} # Informs on assembly model and number of associated MOPs
    mop_symmetry = None # Stored separately for each assembly Model
    list_gbus = [] # List of GBUs found in the KG. 
    list_gbus_am = [] # List of GBU:AM dictionaries finaly merged into preR2 file
    list_R1 = [] # [{AM_am_string:{GBU_information, GB2_information}}]
    list_R1_number = []
    
    # When quering SPARQL we obtain two lines of information deriving from each GBU/CBU 
    # We do not know which CBU will be returned first, therefore this is being sorted. 
    for mopIRI in listofMOPs:
        MOPandGBUs  = querykg(SPARQL_ENDPOINTS['ontomops'], mop_GBUs(mopIRI))
        i = 0 
        assemblyModel = {} 
        for MOPandGBU in MOPandGBUs:
            assemblyModel['MOPFormula'] = MOPandGBU['MOPFormula']
            assemblyModel['mopIRI'] = MOPandGBU['mopIRI']
            assemblyModel['Symmetry'] = MOPandGBU['Symmetry'] # Ideally we should have the symmetry aslo as part of the MolFormRef
            mop_symmetry = assemblyModel['Symmetry']
            i += 1
            gbu = {}
            if i == 1:
                cbuA = dict.copy(MOPandGBU)
            elif i == 2:
                cbuB = dict.copy(MOPandGBU)
        gbu = order(cbuA,cbuB) # Returns a block of sorted GBU/CBU information 
        assemblyModel.update(gbu) # The GBU/CBU information is merged with the general information on the MOP
        am_string = createAssemblyam_string(assemblyModel) # Based on sorted order of GBUs, returns an assembly models am_string          
        print(assemblyModel['MOPFormula'], "_________________",  am_string)        
        
        # Append all unique GBUs in a lists
        gbu1 = str(gbu['CBU1_Modularity']+"-"+gbu['CBU1_Planarity'])
        gbu2 = str(gbu['CBU2_Modularity']+"-"+gbu['CBU2_Planarity'])
        gbu1_number = gbu['CBU1_Number']
        gbu2_number = gbu['CBU2_Number']
        if gbu1 not in list_gbus:
            list_gbus.append(gbu1)
        if gbu2 not in list_gbus:
            list_gbus.append(gbu2)
        if gbu1 in list_gbus:
            pass        
        if gbu2 in list_gbus:
            pass            
        
        # Append all unique GBU:AM pairs to a list
        gbu1dict = {gbu1:am_string}
        gbu2dict = {gbu2:am_string}
        if gbu1dict not in list_gbus_am:
            list_gbus_am.append(gbu1dict)
        if gbu2dict not in list_gbus_am:
            list_gbus_am.append(gbu2dict)
        if gbu1dict  in list_gbus_am:
            pass        
        if gbu2dict in list_gbus_am:
            pass            
        
        # Append all unique AM to a list and count occurance
        # IF AM is unique, appends it and its GBUs+Symmetry to R1 for set operations
        # IF AM is unique, appenand it and its GBUs numbers to R1 numbers for assembly operations
        if am_string not in uniques.keys():
            uniques[str(am_string)] = 0
            frequency = uniques[str(am_string)]
            assemblyModel_json(assemblyModel, am_string)
            assemblyModel_json_temp(assemblyModel, am_string, frequency)
            list_R1.append({am_string:[str(gbu['CBU1_Modularity']+"-"+gbu['CBU1_Planarity']),str(gbu['CBU2_Modularity']+"-"+gbu['CBU2_Planarity']), mop_symmetry]})
            list_R1_number.append({am_string:[gbu1_number, gbu2_number]})
        if am_string in uniques.keys():
            uniques[str(am_string)] += 1/2
            frequency = uniques[str(am_string)] 
            assemblyModel_json_temp(assemblyModel, am_string, frequency)
            assemblyModel_json_update(am_string, frequency)
    
    # Saves the R1 and preR2 lists
    r1_json(list_R1)
    list_preR2 = create_preR2(list_gbus, list_gbus_am)
    preR2_json(list_preR2) 

    # Provides general overview for the user
    print("UNIQUES:\n", uniques)
    return uniques, list_gbus, list_R1_number

def create_preR2(list_gbus, list_gbus_am):
    """Creates a dictionary of GBUs nested with lists of related AMs."""
    list_preR2 = {}
    for gbu_am_string in list_gbus:
        list_ams = []
        for gbu_dict in list_gbus_am:
            if gbu_am_string in gbu_dict.keys():
                value = gbu_dict[gbu_am_string]
                list_ams.append(value)
            if gbu_am_string not in gbu_dict.keys():
                pass
        mydict = {gbu_am_string:list_ams}
        list_preR2.update(mydict)   
    return list_preR2    
        
def order(cbuA, cbuB):
    """Based on comparison of two GBUs, orders them and returns single gbus dictionary"""
    mod_cbuA = int(cbuA['Modularity']) # The ordering is based on modularity. Alternatively it can be done on the nature of the cbu. i.e. organic vs inorganic. 
    mod_cbuB = int(cbuB['Modularity'])
    gbu = {}
    if mod_cbuA > mod_cbuB :
        gbu['CBU1'] = cbuA['CBUFormula']
        gbu['CBU1_Number'] = cbuA['NumberValue']
        gbu['CBU1_Modularity'] = cbuA['Modularity']
        gbu['CBU1_Planarity'] = cbuA['Planarity']
        gbu['CBU1_Type'] = cbuA['CBUType']
        gbu['CBU1_SpeciesIRI'] = cbuA['speciesIRI']
        gbu['CBU1_OuterCoordination'] = cbuA['OuterCoordination']
        gbu['CBU1_FunctionalGroup'] = cbuA['CBUFunctionalGroup']
        gbu['CBU1_Direction'] = cbuA['Direction']    
        gbu['CBU2'] = cbuB['CBUFormula']
        gbu['CBU2_Number'] = cbuB['NumberValue']
        gbu['CBU2_Modularity'] = cbuB['Modularity']
        gbu['CBU2_Planarity'] = cbuB['Planarity']
        gbu['CBU2_Type'] = cbuB['CBUType']
        gbu['CBU2_SpeciesIRI'] = cbuB['speciesIRI']
        gbu['CBU2_OuterCoordination'] = cbuB['OuterCoordination']
        gbu['CBU2_FunctionalGroup'] = cbuB['CBUFunctionalGroup']
        gbu['CBU2_Direction'] = cbuB['Direction']
    else:
        gbu['CBU1'] = cbuB['CBUFormula']
        gbu['CBU1_Number'] = cbuB['NumberValue']
        gbu['CBU1_Modularity'] = cbuB['Modularity']
        gbu['CBU1_Planarity'] = cbuB['Planarity']
        gbu['CBU1_Type'] = cbuB['CBUType']
        gbu['CBU1_SpeciesIRI'] = cbuB['speciesIRI']
        gbu['CBU1_OuterCoordination'] = cbuB['OuterCoordination']
        gbu['CBU1_FunctionalGroup'] = cbuB['CBUFunctionalGroup']
        gbu['CBU1_Direction'] = cbuB['Direction']    
        gbu['CBU2'] = cbuA['CBUFormula']
        gbu['CBU2_Number'] = cbuA['NumberValue']
        gbu['CBU2_Modularity'] = cbuA['Modularity']
        gbu['CBU2_Planarity'] = cbuA['Planarity']
        gbu['CBU2_Type'] = cbuA['CBUType']
        gbu['CBU2_SpeciesIRI'] = cbuA['speciesIRI']
        gbu['CBU2_OuterCoordination'] = cbuA['OuterCoordination']
        gbu['CBU2_FunctionalGroup'] = cbuA['CBUFunctionalGroup']
        gbu['CBU2_Direction'] = cbuA['Direction']
    return gbu

def createAssemblyam_string(assemblyModel):
    """Using properites of the MOP, creates a string of an Assembly Model."""
    ind_1 = assemblyModel['CBU1_Number']
    mod_1 = assemblyModel['CBU1_Modularity']
    pln_1 = assemblyModel['CBU1_Planarity']
    ind_2 = assemblyModel['CBU2_Number']
    mod_2 = assemblyModel['CBU2_Modularity']
    pln_2 = assemblyModel['CBU2_Planarity']
    symMOP = assemblyModel['Symmetry']
    assemblyStr = "("+ mod_1 + "-" + pln_1 + ")x" + ind_1 + "(" + mod_2 + "-" + pln_2 + ")x" + ind_2 + "___(" + symMOP + ")" 
    return assemblyStr