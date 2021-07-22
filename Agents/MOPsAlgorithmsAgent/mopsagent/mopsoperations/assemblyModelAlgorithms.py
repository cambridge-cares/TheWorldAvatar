from mopsagent.utils.params import REMOTE_KG
from mopsagent.kgoperations.queryKG import queryKG, queryKG3
from mopsagent.kgoperations.queryKG import queryKG2
from mopsagent.mopsoperations.assemblyModelParser import nestingModelDict
from mopsagent.mopsoperations.MOPs_query_template import queryBuildingUnitsTemplate
from mopsagent.mopsoperations.MOPs_query_template import queryAssemblyModel
from mopsagent.mopsoperations.MOPs_query_template import queryMOPFormula
import json

def refinePairs(buildingUnit):
    '''This Function screens and removes duplicates of building units with the same formula. Ideally the removal of duplicates should be repsved by accessing the unique species of the building units.'''
    uniques = []
    refined = []
    for unit in buildingUnit:
        if unit['BuildingUnit_Formula'] not in uniques:
            uniques.append(unit['BuildingUnit_Formula'])
            refined.append(dict(unit))
    return refined

def refineMOPPairs(novelPairs):
    '''This Function screens and removes duplicates
    of building units with the same formula.
    Ideally the removal of duplicates should be
    repsved by accessing the unique species of the building units.'''
    
    pairuniques = []
    for pair in novelPairs:
        if pair not in pairuniques:
            pairuniques.append(pair)
    return pairuniques


def getcomplementaryUnitsPairs(refined):
    """This function ensures that two units do not share the
    same functionality and planarity. At the next stage,
    further complementarity based on complementarity of
    building units and directionality needs to be encoded."""

    complementarypairs = []
    for i, unit1 in enumerate(refined):
        for l, unit2 in enumerate(refined[i+1:]):
            if unit1['Binding_Functionality'] != unit2['Binding_Functionality'] \
                and unit1['Planarity_Type'] != unit2['Planarity_Type']:
                complementarypairs.append([unit1, unit2])
    return complementarypairs

def fromModelToReportedMOP(assemblyModelString):
    """This function represents the overall MOP Algoritm. It takes a generic assembly model string based on which it obtains i) MOP instances of that assembly model; ii) chemical building units based on the generic building units in the MOP."""
    mopPairs = []
    queryModel = queryAssemblyModel(assemblyModelString)
    response2 = queryKG2(queryStr2=queryModel)
    assemblyModel = json.loads(response2)
    for item in assemblyModel:
        mofFormula = item["MOP"]
        queryMOP = queryMOPFormula(mofFormula)
        response3 = queryKG3(queryStr3=queryMOP)
        mopBuildingUnits = json.loads(response3)
        mopPair = {}
        mopNotPair = {}

        mopPair["MOP"] = None
        mopPair["B1"] = None
        mopPair["B2"] = None
        for i, unitFormula1 in enumerate(mopBuildingUnits):
            for l, unitFormula2 in enumerate(mopBuildingUnits[i+1:]):
                if unitFormula1["MOP"] == unitFormula2["MOP"]:
                    mopPair.update({"MOP" :  unitFormula1["MOP"], "B1": unitFormula1["BuildingUnitFormula"], "B2": unitFormula2["BuildingUnitFormula"]})
                    #print(mopPair)
                    mopPair1 = dict(mopPair)
                    mopPairs.append(mopPair1)
                else:
                    continue
    return mopPairs


def fromAssemblyModToGenBU(assemblyModelString):
    assemblyModelList = ['C3x4-E2x6','C3x4-F3x4','E2x6-F3x4','C4x6-E2x12','C4x6-F3x8','E2x12-F3x8','C3x8-E2x12','C3x8-F4x6','E2x12-F4x6','C3x20-E2x30','C3x20-F5x12','E2x30-F5x12', 'C5x12-E2x30','C5x12-F3x20','E2x30-F3x20']
    #prevent incorrect assembly model strings by:
    #   - Check if multiplicity x modularity matches
    #   - Check if C, E and F are inserted correctly
    #   - Ensure that users obtain results only when they insert two generic units (e.g. C3x4-E2x6)
    if assemblyModelString in assemblyModelList:
        pass
    if str(assemblyModelString) not in assemblyModelList:
        print("Please enter a correct Assembly Model")
        exit()

    reportedMOP = fromModelToReportedMOP(assemblyModelString)
    #print(type(reportedMOP))

    units = nestingModelDict(assemblyModelString)
    buildingUnits = []
    item = {}

    for unit in units:
        queryTemplate = queryBuildingUnitsTemplate(modularity=unit["Modularity"],planarity=unit["Planarity"])
        response = queryKG(queryStr=queryTemplate)
        multiplicity = unit["UnitNumber"]
        unitList = json.loads(response)
        for item in unitList:
            item.update({"Multiplicity": unit["UnitNumber"]})
        buildingUnits.extend(unitList)
    refined = refinePairs(buildingUnits)
    print(" ")
    print("This is a number of units comming from the query", len(buildingUnits))
    print("This is the refined number of building units", len(refined))
    print(" ")
    print("### This is a list of Unique Combinations ###")
    complementarypairs = getcomplementaryUnitsPairs(refined)
    reportedPairs = []
    novelPairs = []

    for x, y in complementarypairs:
        for z in reportedMOP:
            if x['BuildingUnit_Formula'] == z['B1'] and y['BuildingUnit_Formula'] == z['B2']:
                reportedPairs.append([x,y])
            if x['BuildingUnit_Formula'] == z['B2'] and y['BuildingUnit_Formula'] == z['B1']:
                reportedPairs.append([x,y])
            elif x['BuildingUnit_Formula'] != z['B1'] or y['BuildingUnit_Formula'] != z['B2']:
                novelPairs.append([x,y])


    print("Nr. COMPLEMENTARY BUILDING UNIT PAIRS: ", len(complementarypairs))
    pairnoveluniques = refineMOPPairs(novelPairs)
    pairreporteduniques = refineMOPPairs(reportedPairs)
    print("Nr. REPORTED/KNOW MOPs: ",len(pairreporteduniques))
    print("Nr. NOVEL/UNKNOW MOPs: ", len(pairnoveluniques))

    for a, b in pairreporteduniques:
        print(f"REPORTED: [{b['BuildingUnit_Formula']}x{b['Multiplicity']}{a['BuildingUnit_Formula']}x{a['Multiplicity']}]")
    for c, d in pairnoveluniques:
            print(f"NOVEL: [{d['BuildingUnit_Formula']}x{d['Multiplicity']}{c['BuildingUnit_Formula']}x{c['Multiplicity']}]")
    return buildingUnits