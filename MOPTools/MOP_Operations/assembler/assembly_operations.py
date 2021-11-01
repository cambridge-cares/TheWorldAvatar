from assembler.assembly_string_parser import nestingModelDict 
from kgoperations.queryKG import querykg
from kgoperations.queryendpoint import SPARQL_ENDPOINTS
from kgoperations.queryTemplates import testquery
from kgoperations.queryTemplates import mop_GBUs
from kgoperations.queryTemplates import mop_reference
import json
import time

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
    mopFormR1Path = "E:\\to_clean_up\\assembly_out_test\\mops_r1\\"
    listofMOPs = runquery()
    uniques = {}
    mopFormulaList = []
    mopProvenance = []
    for mopIRI in listofMOPs:
        result  = querykg(SPARQL_ENDPOINTS['ontomops'], mop_GBUs(mopIRI))
        y = 0
        mopFormulaRef = {}
        assemblyModel = {}
        for x in result:
            assemblyModel['MOPFormula'] = x['MOPFormula']
            assemblyModel['mopIRI'] = x['mopIRI']
            assemblyModel['Symmetry'] = x['Symmetry']
            mopFormulaRef['MOPReference'] = x['MOPReference']
            mopFormulaRef['MOPFormula'] = x['MOPFormula']
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
        if mopFormulaRef['MOPReference'] not in mopFormulaList:
            mopFormulaList.append(mopFormulaRef['MOPReference'])
            mopProvenance.append(mopFormulaRef)
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
    analyticsR1 = []
    for model in uniques:
        model_libs = cbu_gbu_lib(model)
        model_gbu1 = model_libs[0] 
        model_gbu1num = model_libs[1]
        model_gbu2 = model_libs[2] 
        model_gbu2num = model_libs[3]
        mop_radius1(model, model_gbu1,model_gbu1num, model_gbu2, model_gbu2num)
        r1_analytics = newvsknown(model)
        print(r1_analytics)
        analyticsR1.append(r1_analytics)
    print(uniques)
    print(analyticsR1)

def newvsknown(model):
    assemblyModelGroupPath = "E:\\to_clean_up\\assembly_out_test\\mops_r1\\"+model+"__R1.json"
    with open(assemblyModelGroupPath, 'r+') as file:
        data = json.load(file)
        new = 0
        known = 0 
        for item in data:
            print(item)
            for pair in item:
                x = list(pair.values())
                if "NEW" in x:
                    new += 1
                if "NEW" not in x:
                    known += 1
    model_analytics_r1 = {"Model Type":model, "NEW":new, "KNOWN":known}
    return model_analytics_r1

def mop_radius1(model, model_gbu1,model_gbu1num, model_gbu2, model_gbu2num):
    mopFormR1Path = "C:\\Users\\ak2332\\Documents\\SandboxPrograms\\assembly_out_test\\mops_r1\\"
    mopFormulaR1 = []
    for cbu1 in model_gbu1:
        for cbu2 in  model_gbu2:
            mopFormula = str(cbu1+model_gbu1num+cbu2+model_gbu2num)
            altmopFormula = str(cbu2+model_gbu2num+cbu1+model_gbu1num)
            if mopFormula in mopFormulaR1:
                pass
            else:
                mopFormulaR1.append(mopFormula)
    mopFormulaOut = json.dumps(mopFormulaR1, indent=4)
    mopFormR1Json = open(mopFormR1Path+model+"__R1.json", 'w') 
    mopFormR1Json.write(mopFormulaOut)
    #return mopFormulaR1

def cbu_gbu_lib(model):
    assemblyModelGroupPath = "C:\\Users\\ak2332\\Documents\\SandboxPrograms\\assembly_out_test\\"+model+'.json'
    cbuLibPath = "C:\\Users\\ak2332\\Documents\\SandboxPrograms\\assembly_out_test\\cbu_gbu_lib\\"
    with open(assemblyModelGroupPath, 'r+') as file:
        data = json.load(file)
        cbu1lib = []
        cbu2lib = []
        for item in data:
            cbu1name = item['CBU1_Modularity'] + "-" + item['CBU1_Planarity']
            cbu2name = item['CBU2_Modularity'] + "-" + item['CBU2_Planarity']
            cbu1number = item['CBU1_Number']
            cbu2number = item['CBU2_Number']
            if item['CBU1'] in cbu1lib:
                pass
            else:
                cbu1lib.append(item['CBU1'])
            if item['CBU2'] in cbu2lib:
                pass
            else: 
                cbu2lib.append(item['CBU2'])
            #print(item['MOPFormula'])
        cbu1libout = json.dumps(cbu1lib, indent=4)
        cbu2libout = json.dumps(cbu2lib, indent=4)
        cbu1modelLibpath = cbuLibPath+model+"__"+cbu1name+'.json'
        cbu2modelLibpath = cbuLibPath+model+"__"+cbu2name+'.json'
        update_jsonoutput = open(cbu1modelLibpath, 'w') 
        update_jsonoutput.write(cbu1libout)
        update_jsonoutput = open(cbu2modelLibpath, 'w') 
        update_jsonoutput.write(cbu2libout)
    return cbu1lib, cbu1number, cbu2lib, cbu2number

def assemblyModel_json(assemblyModel, string):
    assemblyModel_jsonpath = "C:\\Users\\ak2332\\Documents\\SandboxPrograms\\assembly_out_test\\"
    outAssemblyModel = json.dumps(assemblyModel)
    jsonoutput = open(assemblyModel_jsonpath+string+'.json', 'w') 
    jsonoutput.write(outAssemblyModel)

def assemblyModel_json_ext(assemblyModel, string, frequency):
    assemblyModel_jsonpath = "C:\\Users\\ak2332\\Documents\\SandboxPrograms\\assembly_out_test\\arrange\\"
    outAssemblyModel = json.dumps(assemblyModel)
    jsonoutput = open(assemblyModel_jsonpath+string+"__"+str(frequency)+'.json', 'w') 
    jsonoutput.write(outAssemblyModel)

def assemblyModel_json_update(string, frequency):
    assemblyModelGroup = []
    refined = []
    path_main = "C:\\Users\\ak2332\\Documents\\SandboxPrograms\\assembly_out_test\\"+string+'.json'
    i = 0
    while i < frequency:
        path_update = "C:\\Users\\ak2332\\Documents\\SandboxPrograms\\assembly_out_test\\arrange\\"+string+"__"+str(i)+'.json'
        with open(path_update, 'r+') as file:    
            data = json.load(file)
            mopIRI = data['mopIRI']
            if mopIRI not in refined:
                refined.append(mopIRI)
                assemblyModelGroup.append(data)
            else:
                pass
        i += 0.5
    updated = json.dumps(assemblyModelGroup, indent=4)
    jsonoutput = open(path_main, 'w') 
    jsonoutput.write(updated)
         

def generatedMOPquery(mopFormula):
    time.sleep(0.1)
    result1  = querykg(SPARQL_ENDPOINTS['ontomops'], generatedMOPquery(mopFormula))
    #result2  = querykg(SPARQL_ENDPOINTS['ontomops'], generatedMOPquery(altmopFormula))
    if result1:
        if 'speciesIRI' in result1[0].keys():
            provenance =  result1[0]['speciesIRI']
        else:
            provenance = "not in KG"
    return provenance

def refined_json(string, frequency):
    assemblyModel_jsonpath1 = "C:\\Users\\ak2332\\Documents\\SandboxPrograms\\assembly_out_test\\"+string+'.json'
    update_json_path2 = "C:\\Users\\ak2332\\Documents\\SandboxPrograms\\assembly_out_test\\arrange\\"+string+"__"+str(frequency)+'.json'
    with open(update_json_path2, 'r+') as file:
        data = json.load(file)
        refined = []
        for item in data:
            refined.append(item)
        outjson = json.dumps(refined, indent=4)
        update_jsonoutput = open(assemblyModel_jsonpath1, 'w') 
        update_jsonoutput.write(outjson)

def getMOPFormula():
    result  = querykg(SPARQL_ENDPOINTS['ontomops'], testquery())
    return result

def assembly_operations(assemeblyModelString):
    x = nestingModelDict(assemeblyModelString)
    return x
