from assembler.assembly_string_parser import nestingModelDict 
from kgoperations.queryKG import querykg
from kgoperations.queryendpoint import SPARQL_ENDPOINTS
from kgoperations.queryTemplates import testquery
from kgoperations.queryTemplates import mop_GBUs

def runquery():
    result  = querykg(SPARQL_ENDPOINTS['ontomops'], testquery())
    refinedlist = []
    for item in result:
        refined = item['mopIRI']
        refinedlist.append(refined)
    return refinedlist

def order(cbuA, cbuB):
    mod_cbuA = int(cbuA['Modularity'])
    mod_cbuB = int(cbuB['Modularity'])
    gbu = {}
    if mod_cbuA > mod_cbuB :
        gbu['CBU1'] = cbuA['CBUFormula']
        gbu['CBU1_Number'] = cbuA['NumberValue']
        gbu['CBU1_Modularity'] = cbuA['Modularity']
        gbu['CBU1_Planarity'] = cbuA['Planarity']
        gbu['CBU2'] = cbuB['CBUFormula']
        gbu['CBU2_Number'] = cbuB['NumberValue']
        gbu['CBU2_Modularity'] = cbuB['Modularity']
        gbu['CBU2_Planarity'] = cbuB['Planarity']
    else:
        gbu['CBU1'] = cbuB['CBUFormula']
        gbu['CBU1_Number'] = cbuB['NumberValue']
        gbu['CBU1_Modularity'] = cbuB['Modularity']
        gbu['CBU1_Planarity'] = cbuB['Planarity']
        gbu['CBU2'] = cbuA['CBUFormula']
        gbu['CBU2_Number'] = cbuA['NumberValue']
        gbu['CBU2_Modularity'] = cbuA['Modularity']
        gbu['CBU2_Planarity'] = cbuA['Planarity']
    return gbu

def createAssemblyString(assemblyModel):
    ind_1 = assemblyModel['CBU1_Number']
    mod_1 = assemblyModel['CBU1_Modularity']
    pln_1 = assemblyModel['CBU1_Planarity']
    ind_2 = assemblyModel['CBU2_Number']
    mod_2 = assemblyModel['CBU2_Modularity']
    pln_2 = assemblyModel['CBU2_Planarity']
    symMOP = assemblyModel['Symmetry']
    assemblyStr = "("+ mod_1 + "-" + pln_1 + ")x" + ind_1 + "(" + mod_2 + "-" + pln_2 + ")x" + ind_2 + "___(" + symMOP + ")" 
    return assemblyStr
    
def mopIRIquery():
    results = []
    listofMOPs = runquery()
    uniques = {}
    unique_assembly_models = 0
    for mopIRI in listofMOPs:
        result  = querykg(SPARQL_ENDPOINTS['ontomops'], mop_GBUs(mopIRI))
        y = 0
        assemblyModel = {}
        for x in result:
            assemblyModel['MOPFormula'] = x['MOPFormula']
            assemblyModel['mopIRI'] = x['mopIRI']
            assemblyModel['Symmetry'] = x['Symmetry']
            y += 1
            gbu = {}
            if y == 1:
                cbuA = dict.copy(x)
            elif y == 2:
                cbuB = dict.copy(x)
        gbu = order(cbuA,cbuB) 
        assemblyModel.update(gbu)
        string = createAssemblyString(assemblyModel)
        print(assemblyModel['MOPFormula'], "_________________",  string)
        if string not in uniques.keys():
            uniques[str(string)] = 0
        if string in uniques.keys():
            uniques[str(string)] += 1/2            
    print(uniques)
    results.append(result)
    return assemblyModel

def assemblyModelList():
    return

def getMOPFormula():
    result  = querykg(SPARQL_ENDPOINTS['ontomops'], testquery())
    return result

def assembly_operations(assemeblyModelString):
    x = nestingModelDict(assemeblyModelString)
    return x
