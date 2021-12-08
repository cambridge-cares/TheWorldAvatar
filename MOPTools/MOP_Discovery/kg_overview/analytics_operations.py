'''
Created on Dec 1, 2021

@author: Aleksandar Kondinski
'''

from query_operations.queryKG import querykg
from query_operations.queryendpoint import SPARQL_ENDPOINTS
from query_operations.queryTemplates import getMOPIRIs
from query_operations.queryTemplates import mop_GBUs
from kg_overview.analytics_output import assemblyModel_json
from kg_overview.analytics_output import assemblyModel_json_ext
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
    """For each MOP IRI we collect GBU, CBU and other information.
    The information is used to derive assembly models and R1 and pre-R2 lists."""
    uniques = {}
    mop_symmetry = None
    list_pregbus = []
    list_gbus = []
    list_R1 = [] # list of type [{AM_string:{GBU_information, GB2_information}}]
    list_R1_number = []
    for mopIRI in listofMOPs:
        MOPandGBUs  = querykg(SPARQL_ENDPOINTS['ontomops'], mop_GBUs(mopIRI))
        i = 0
        assemblyModel = {} # At this stage we create two parallel things 
        for MOPandGBU in MOPandGBUs: # for each queried MOP IRI we get two GBU lines as an output.
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
        gbu = order(cbuA,cbuB) 
        assemblyModel.update(gbu)
        string = createAssemblyString(assemblyModel)                 
        print(assemblyModel['MOPFormula'], "_________________",  string)        
        gbu1 = str(gbu['CBU1_Modularity']+"-"+gbu['CBU1_Planarity'])
        gbu2 = str(gbu['CBU2_Modularity']+"-"+gbu['CBU2_Planarity'])
        gbu1_number = gbu['CBU1_Number']
        gbu2_number = gbu['CBU2_Number']
        gbu1dict = {gbu1:string}
        gbu2dict = {gbu2:string}
        if gbu1 not in list_pregbus:
            list_pregbus.append(gbu1)
        if gbu2 not in list_pregbus:
            list_pregbus.append(gbu2)
        if gbu1  in list_pregbus:
            pass        
        if gbu2 in list_pregbus:
            pass            
        if gbu1dict not in list_gbus:
            list_gbus.append(gbu1dict)
        if gbu2dict not in list_gbus:
            list_gbus.append(gbu2dict)
        if gbu1dict  in list_gbus:
            pass        
        if gbu2dict in list_gbus:
            pass            
        if string not in uniques.keys():
            uniques[str(string)] = 0
            frequency = uniques[str(string)]
            assemblyModel_json(assemblyModel, string)
            assemblyModel_json_ext(assemblyModel, string, frequency)
            list_R1.append({string:[str(gbu['CBU1_Modularity']+"-"+gbu['CBU1_Planarity']),str(gbu['CBU2_Modularity']+"-"+gbu['CBU2_Planarity']), mop_symmetry]})
            list_R1_number.append({string:[gbu1_number, gbu2_number]})
        if string in uniques.keys():
            uniques[str(string)] += 1/2
            frequency = uniques[str(string)] 
            assemblyModel_json_ext(assemblyModel, string, frequency)
            assemblyModel_json_update(string, frequency)
    r1_json(list_R1)
    list_preR2 = mergeR2(list_pregbus, list_gbus)
    preR2_json(list_preR2) 
    print("UNIQUES:\n", uniques)
    return uniques, list_pregbus, list_R1_number

def mergeR2(list_pregbus, list_gbus):
    """This function takes a list of the unique gbus in the kg.
    For each gbu in the kg connects all possible assembly models."""
    list_preR2 = {}
    for gbu_string in list_pregbus:
        list_ams = []
        for gbu_dict in list_gbus:
            if gbu_string in gbu_dict.keys():
                value = gbu_dict[gbu_string]
                list_ams.append(value)
            if gbu_string not in gbu_dict.keys():
                pass
        mydict = {gbu_string:list_ams}
        list_preR2.update(mydict)   
    return list_preR2    
        
def order(cbuA, cbuB):
    """This function takes two generic building units (GBUs), orders them and merges them into a single gbu dictionary"""
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

def createAssemblyString(assemblyModel):
    """The properties of an assembly model dictionary are transformed into single line string. The string is used for grouping."""
    ind_1 = assemblyModel['CBU1_Number']
    mod_1 = assemblyModel['CBU1_Modularity']
    pln_1 = assemblyModel['CBU1_Planarity']
    ind_2 = assemblyModel['CBU2_Number']
    mod_2 = assemblyModel['CBU2_Modularity']
    pln_2 = assemblyModel['CBU2_Planarity']
    symMOP = assemblyModel['Symmetry']
    assemblyStr = "("+ mod_1 + "-" + pln_1 + ")x" + ind_1 + "(" + mod_2 + "-" + pln_2 + ")x" + ind_2 + "___(" + symMOP + ")" 
    return assemblyStr