from kgoperations.queryKG import querykg
from kgoperations.queryendpoint import SPARQL_ENDPOINTS
from kgoperations.queryTemplates import getMOPIRIs
from kgoperations.queryTemplates import mop_GBUs
from radius_1.assembly_functions import order
from radius_1.assembly_functions import createAssemblyString

def kgoverview():
    """This Function runs preset query that returns back all of the MOP IRIs found in the OntoMOP KG"""
    result  = querykg(SPARQL_ENDPOINTS['ontomops'], getMOPIRIs()) #The query gets a full list of IRIs
    refinedlist = [] # Each IRI is saved in the list of refined IRIs
    for item in result:
        refined = item['mopIRI']
        refinedlist.append(refined)
    return refinedlist 

def assembliesOverview(listofMOPs):
    """Overview on the different assembly models present in the KG""" 
    uniques = {}
    for mopIRI in listofMOPs:
        MOPandGBUs  = querykg(SPARQL_ENDPOINTS['ontomops'], mop_GBUs(mopIRI))
        i = 0
        assemblyModel = {} # At this stage we create two parallel things 
        for MOPandGBU in MOPandGBUs: # for each queried MOP IRI we get two GBU lines as an output.
            assemblyModel['MOPFormula'] = MOPandGBU['MOPFormula']
            assemblyModel['mopIRI'] = MOPandGBU['mopIRI']
            assemblyModel['Symmetry'] = MOPandGBU['Symmetry'] # Ideally we should have the symmetry aslo as part of the MolFormRef
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
        if string not in uniques.keys():
            uniques[str(string)] = 0
        if string in uniques.keys():
            uniques[str(string)] += 1/2
    return uniques