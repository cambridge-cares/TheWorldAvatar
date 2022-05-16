from kgoperations.queryTemplates import mop_GBUs
from kgoperations.queryKG import querykg
from kgoperations.queryendpoint import SPARQL_ENDPOINTS
from radius_1.output_files import assemblyModel_json
from radius_1.output_files import assemblyModel_json_ext
from radius_1.output_files import assemblyModel_json_update
from radius_1.assembly_functions import order
from radius_1.assembly_functions import createAssemblyString

##### OPERATIONS RELATED TO MOP SEARCH RADIUS 1 #####

def assemblyModelGroups(listofMOPs):
    """Takes a list of MOP IRIs, queries the building untis, orders and gets formulas and counts assembly model."""
    uniques = {}
    mopFormulaList = []
    mopProvenance = []
    for mopIRI in listofMOPs:     #### This loops over the MOPs list
        MOPandGBUs  = querykg(SPARQL_ENDPOINTS['ontomops'], mop_GBUs(mopIRI))
        i = 0
        savedMOPReferences = {} # Saved info so we do not need to requery again
        assemblyModel = {} # At this stage we create two parallel things 
        for MOPandGBU in MOPandGBUs: # for each queried MOP IRI we get two GBU lines as an output.
            assemblyModel['MOPFormula'] = MOPandGBU['MOPFormula']
            assemblyModel['mopIRI'] = MOPandGBU['mopIRI']
            assemblyModel['Symmetry'] = MOPandGBU['Symmetry'] # Ideally we should have the symmetry aslo as part of the MolFormRef
            savedMOPReferences['MOPReference'] = MOPandGBU['MOPReference'] # Saved so no need to requery
            savedMOPReferences['MOPFormula'] = MOPandGBU['MOPFormula'] # Saved so no need to requery
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
        if savedMOPReferences['MOPReference'] not in mopFormulaList: 
            mopFormulaList.append(savedMOPReferences['MOPReference'])
            mopProvenance.append(savedMOPReferences)
        if string not in uniques.keys():
            uniques[str(string)] = 0
            frequency = uniques[str(string)]
            assemblyModel_json(assemblyModel, string)
            assemblyModel_json_ext(assemblyModel, string, frequency)
        if string in uniques.keys():
            uniques[str(string)] += 1/2
            frequency = uniques[str(string)] 
            assemblyModel_json_ext(assemblyModel, string, frequency)
            assemblyModel_json_update(string, frequency)
    print(uniques)
    return uniques