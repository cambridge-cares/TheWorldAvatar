import os, sys, json
from rdflib.graph import ConjunctiveGraph
from rdflib.store import NO_STORE
BASE = os.path.dirname(os.path.dirname(os.path.abspath(__file__)))
sys.path.insert(0, BASE)
from UK_Digital_Twin_Package.queryInterface import performQuery, performUpdate, performFederatedQuery
from UK_Digital_Twin_Package import UKPowerGridModel
from collections import Counter
from shapely.wkt import loads
from shapely.geometry import mapping
import geojson
import ast


def queryGeneratorModelInput_new(numOfBus, numOfBranch, endPoint) -> list:
    
    queryVar = []
    for var in UKPowerGridModel.UKEGenModel.INPUT_VARIABLE:
        queryVar.append("?" + str(var))    
    selectClause = " ".join(queryVar)
    
    label = "UK_Electrical_Grid_" + str(numOfBus) + "_Bus_" + str(numOfBranch) + "_Branch"
    queryStr = """
    PREFIX rdfs: <http://www.w3.org/2000/01/rdf-schema#>
    PREFIX j1:<http://www.theworldavatar.com/ontology/ontopowsys/PowSysRealization.owl#>
	PREFIX j2:<http://www.theworldavatar.com/ontology/ontocape/upper_level/system.owl#>
	PREFIX j3:<http://www.theworldavatar.com/ontology/ontopowsys/model/PowerSystemModel.owl#>
	PREFIX j4:<http://www.theworldavatar.com/ontology/meta_model/topology/topology.owl#>
	PREFIX j5:<http://www.theworldavatar.com/ontology/ontocape/model/mathematical_model.owl#>
	PREFIX j6:<http://www.theworldavatar.com/ontology/ontocape/chemical_process_system/CPS_behavior/behavior.owl#>
	PREFIX j7:<http://www.theworldavatar.com/ontology/ontocape/supporting_concepts/space_and_time/space_and_time_extended.owl#>
	PREFIX j8:<http://www.theworldavatar.com/ontology/ontocape/material/phase_system/phase_system.owl#>
	SELECT %s ?EGen
    
    #?BusNumbervalue ?activepowervalue ?reactivepowervalue ?Qmaxvalue ?Qminvalue ?Vgvalue ?mBasevalue ?Statusvalue ?Pmaxvalue ?Pminvalue ?Pc1value ?Pc2value ?Qc1minvalue ?Qc1maxvalue ?Qc2minvalue ?Qc2maxvalue ?Rampagcvalue ?Ramp10value ?Ramp30value ?Rampqvalue ?apfvalue

    WHERE {
        ?Model_EGen a j5:Submodel .
        ?Model_EGen rdfs:label ?label .
        FILTER regex(?label, "%s") .
        
        ?Model_EGen j2:isComposedOfSubsystem ?EGen .
        #?EGen  a  j3:GeneratorModel .        

        ?EGen   j5:hasModelVariable ?num . 
        ?num  a  j3:BusNumber  .
        ?num  j2:hasValue ?vnum .
        ?vnum   j2:numericalValue  %s .
        
        ?EGen   j5:hasModelVariable ?Pg . 
        ?Pg  a  j3:Pg  .
        ?Pg  j2:hasValue ?vpg .
        ?vpg   j2:numericalValue  %s .
        

        ?EGen   j5:hasModelVariable ?Qg .
        ?Qg  a  j3:Qg  . 
        ?Qg  j2:hasValue ?vqg .
        ?vqg   j2:numericalValue  %s .

        ?EGen   j5:hasModelVariable ?qmax .
        ?qmax  a  j3:QMax  . 
        ?qmax  j2:hasValue ?vqmax .
        ?vqmax   j2:numericalValue  %s .

        ?EGen   j5:hasModelVariable ?qmin .
        ?qmin  a  j3:QMin  . 
        ?qmin  j2:hasValue ?vqmin .
        ?vqmin   j2:numericalValue  %s .

        ?EGen   j5:hasModelVariable ?Vg . 
        ?Vg  a  j3:Vg  . 
        ?Vg  j2:hasValue ?vVg .
        ?vVg   j2:numericalValue  %s .

        ?EGen   j5:hasModelVariable ?mbase .
        ?mbase  a  j3:mBase  .
        ?mbase  j2:hasValue ?vmbase .
        ?vmbase   j2:numericalValue  %s .

        ?EGen   j5:hasModelVariable ?stat . 
        ?stat  a  j3:Status . 
        ?stat  j2:hasValue ?vstat .
        ?vstat   j2:numericalValue  %s .

        ?EGen   j5:hasModelVariable ?pmax . 
        ?pmax  a  j3:PMax  .
        ?pmax  j2:hasValue ?vpmax .
        ?vpmax   j2:numericalValue  %s .

        ?EGen   j5:hasModelVariable ?pmin . 
        ?pmin  a  j3:PMin  . 
        ?pmin  j2:hasValue ?vpmin .
        ?vpmin   j2:numericalValue  %s .

        ?EGen   j5:hasModelVariable ?pc1 . 
        ?pc1  a  j3:Pc1  . 
        ?pc1  j2:hasValue ?vpc1 .
        ?vpc1   j2:numericalValue  %s .

        ?EGen   j5:hasModelVariable ?pc2 . 
        ?pc2  a  j3:Pc2  .
        ?pc2  j2:hasValue ?vpc2 .
        ?vpc2   j2:numericalValue  %s .

        ?EGen   j5:hasModelVariable ?qc1min . 
        ?qc1min  a  j3:QC1Min  .
        ?qc1min  j2:hasValue ?vqc1min . 
        ?vqc1min   j2:numericalValue  %s .

        ?EGen   j5:hasModelVariable ?Qc1max .
        ?Qc1max  a  j3:QC1Max  .
        ?Qc1max  j2:hasValue ?vQc1max . 
        ?vQc1max   j2:numericalValue  %s .

        ?EGen   j5:hasModelVariable ?qc2min .
        ?qc2min  a  j3:QC2Min  .
        ?qc2min  j2:hasValue ?vqc2min .
        ?vqc2min   j2:numericalValue  %s .

        ?EGen   j5:hasModelVariable ?Qc2max .
        ?Qc2max  a  j3:QC2Max  .
        ?Qc2max  j2:hasValue ?vQc2max . 
        ?vQc2max   j2:numericalValue  %s .

        ?EGen   j5:hasModelVariable ?rampagc . 
        ?rampagc  a  j3:Rampagc  .
        ?rampagc  j2:hasValue ?vrampagc . 
        ?vrampagc   j2:numericalValue  %s .

        ?EGen   j5:hasModelVariable ?ramp10 . 
        ?ramp10  a  j3:Ramp10  .
        ?ramp10  j2:hasValue ?vramp10 .
        ?vramp10   j2:numericalValue  %s .

        ?EGen   j5:hasModelVariable ?ramp30 . 
        ?ramp30  a  j3:Ramp30  .
        ?ramp30  j2:hasValue ?vramp30 . 
        ?vramp30   j2:numericalValue  %s .

        ?EGen   j5:hasModelVariable ?rampq . 
        ?rampq  a  j3:Rampq  . 
        ?rampq  j2:hasValue ?vrampq .
        ?vrampq   j2:numericalValue  %s .

        ?EGen   j5:hasModelVariable ?apf .
        ?apf  a  j3:APF .
        ?apf  j2:hasValue ?vapf .
        ?vapf   j2:numericalValue  %s .
        }
        ORDER BY ASC(%s)
    """ % (selectClause, label, queryVar[0], queryVar[1], queryVar[2], queryVar[3], queryVar[4], queryVar[5], queryVar[6], queryVar[7], queryVar[8], \
        queryVar[9], queryVar[10], queryVar[11], queryVar[12], queryVar[13], queryVar[14], queryVar[15], queryVar[16], queryVar[17], queryVar[18], \
            queryVar[19], queryVar[20], queryVar[0]) 
    
    # print(queryStr)
    print('remoteQuery queryGeneratorModelInput')
    res = json.loads(performQuery(endPoint, queryStr))
    print('queryGeneratorModelInput is done')
    
    if len(res) == 0:
      raise Exception("The queryGeneratorModelInput is failed.")
    
    return res

def queryGeneratorModelInput(numOfBus, numOfBranch, endPoint, splitCharacter):
    label = "UK_Electrical_Grid_" + str(numOfBus) + "_Bus_" + str(numOfBranch) + "_Branch"
    queryStr = """
    PREFIX rdfs: <http://www.w3.org/2000/01/rdf-schema#>
    PREFIX j1:<http://www.theworldavatar.com/ontology/ontopowsys/PowSysRealization.owl#>
	PREFIX j2:<http://www.theworldavatar.com/ontology/ontocape/upper_level/system.owl#>
	PREFIX j3:<http://www.theworldavatar.com/ontology/ontopowsys/model/PowerSystemModel.owl#>
	PREFIX j4:<http://www.theworldavatar.com/ontology/meta_model/topology/topology.owl#>
	PREFIX j5:<http://www.theworldavatar.com/ontology/ontocape/model/mathematical_model.owl#>
	PREFIX j6:<http://www.theworldavatar.com/ontology/ontocape/chemical_process_system/CPS_behavior/behavior.owl#>
	PREFIX j7:<http://www.theworldavatar.com/ontology/ontocape/supporting_concepts/space_and_time/space_and_time_extended.owl#>
	PREFIX j8:<http://www.theworldavatar.com/ontology/ontocape/material/phase_system/phase_system.owl#>
	SELECT ?BusNumbervalue ?activepowervalue ?reactivepowervalue ?Qmaxvalue ?Qminvalue ?Vgvalue ?mBasevalue ?Statusvalue ?Pmaxvalue ?Pminvalue ?Pc1value ?Pc2value ?Qc1minvalue ?Qc1maxvalue ?Qc2minvalue ?Qc2maxvalue ?Rampagcvalue ?Ramp10value ?Ramp30value ?Rampqvalue ?apfvalue

    WHERE {
        ?Model_EGen a j5:Submodel .
        ?Model_EGen rdfs:label ?label .
        FILTER regex(?label, "%s") .
        
        ?Model_EGen j2:isComposedOfSubsystem ?EGen .
        #?EGen  a  j3:GeneratorModel .        

        ?EGen   j5:hasModelVariable ?num . 
        ?num  a  j3:BusNumber  .
        ?num  j2:hasValue ?vnum .
        ?vnum   j2:numericalValue ?BusNumbervalue .
        
        ?EGen   j5:hasModelVariable ?Pg . 
        ?Pg  a  j3:Pg  .
        ?Pg  j2:hasValue ?vpg .
        ?vpg   j2:numericalValue ?activepowervalue .
        # FILTER (?activepowervalue >= 10)

        ?EGen   j5:hasModelVariable ?Qg .
        ?Qg  a  j3:Qg  . 
        ?Qg  j2:hasValue ?vqg .
        ?vqg   j2:numericalValue ?reactivepowervalue .

        ?EGen   j5:hasModelVariable ?qmax .
        ?qmax  a  j3:QMax  . 
        ?qmax  j2:hasValue ?vqmax .
        ?vqmax   j2:numericalValue ?Qmaxvalue .

        ?EGen   j5:hasModelVariable ?qmin .
        ?qmin  a  j3:QMin  . 
        ?qmin  j2:hasValue ?vqmin .
        ?vqmin   j2:numericalValue ?Qminvalue .

        ?EGen   j5:hasModelVariable ?Vg . 
        ?Vg  a  j3:Vg  . 
        ?Vg  j2:hasValue ?vVg .
        ?vVg   j2:numericalValue ?Vgvalue .

        ?EGen   j5:hasModelVariable ?mbase .
        ?mbase  a  j3:mBase  .
        ?mbase  j2:hasValue ?vmbase .
        ?vmbase   j2:numericalValue ?mBasevalue .

        ?EGen   j5:hasModelVariable ?stat . 
        ?stat  a  j3:Status . 
        ?stat  j2:hasValue ?vstat .
        ?vstat   j2:numericalValue ?Statusvalue .

        ?EGen   j5:hasModelVariable ?pmax . 
        ?pmax  a  j3:PMax  .
        ?pmax  j2:hasValue ?vpmax .
        ?vpmax   j2:numericalValue ?Pmaxvalue .

        ?EGen   j5:hasModelVariable ?pmin . 
        ?pmin  a  j3:PMin  . 
        ?pmin  j2:hasValue ?vpmin .
        ?vpmin   j2:numericalValue ?Pminvalue .

        ?EGen   j5:hasModelVariable ?pc1 . 
        ?pc1  a  j3:Pc1  . 
        ?pc1  j2:hasValue ?vpc1 .
        ?vpc1   j2:numericalValue ?Pc1value .

        ?EGen   j5:hasModelVariable ?pc2 . 
        ?pc2  a  j3:Pc2  .
        ?pc2  j2:hasValue ?vpc2 .
        ?vpc2   j2:numericalValue ?Pc2value .

        ?EGen   j5:hasModelVariable ?qc1min . 
        ?qc1min  a  j3:QC1Min  .
        ?qc1min  j2:hasValue ?vqc1min . 
        ?vqc1min   j2:numericalValue ?Qc1minvalue .

        ?EGen   j5:hasModelVariable ?Qc1max .
        ?Qc1max  a  j3:QC1Max  .
        ?Qc1max  j2:hasValue ?vQc1max . 
        ?vQc1max   j2:numericalValue ?Qc1maxvalue .

        ?EGen   j5:hasModelVariable ?qc2min .
        ?qc2min  a  j3:QC2Min  .
        ?qc2min  j2:hasValue ?vqc2min .
        ?vqc2min   j2:numericalValue ?Qc2minvalue .

        ?EGen   j5:hasModelVariable ?Qc2max .
        ?Qc2max  a  j3:QC2Max  .
        ?Qc2max  j2:hasValue ?vQc2max . 
        ?vQc2max   j2:numericalValue ?Qc2maxvalue .

        ?EGen   j5:hasModelVariable ?rampagc . 
        ?rampagc  a  j3:Rampagc  .
        ?rampagc  j2:hasValue ?vrampagc . 
        ?vrampagc   j2:numericalValue ?Rampagcvalue .

        ?EGen   j5:hasModelVariable ?ramp10 . 
        ?ramp10  a  j3:Ramp10  .
        ?ramp10  j2:hasValue ?vramp10 .
        ?vramp10   j2:numericalValue ?Ramp10value .

        ?EGen   j5:hasModelVariable ?ramp30 . 
        ?ramp30  a  j3:Ramp30  .
        ?ramp30  j2:hasValue ?vramp30 . 
        ?vramp30   j2:numericalValue ?Ramp30value .

        ?EGen   j5:hasModelVariable ?rampq . 
        ?rampq  a  j3:Rampq  . 
        ?rampq  j2:hasValue ?vrampq .
        ?vrampq   j2:numericalValue ?Rampqvalue .

        ?EGen   j5:hasModelVariable ?apf .
        ?apf  a  j3:APF .
        ?apf  j2:hasValue ?vapf .
        ?vapf   j2:numericalValue ?apfvalue .
        }
        ORDER BY ASC(?BusNumbervalue)
    """ %label 
    
    # print(queryStr)
    print('remoteQuery queryGeneratorModelInput')
    res = json.loads(performQuery(endPoint, queryStr))
    print('queryGeneratorModelInput is done')
    for r in res:
      for key in r.keys():
          if '\"^^' in  r[key]:
            r[key] = (r[key].split('\"^^')[0]).replace('\"','')   

    ret_array = [[ r['BusNumbervalue'], r['activepowervalue'], r['reactivepowervalue'], r['Qmaxvalue'], r['Qminvalue'], r['Vgvalue'], r['mBasevalue'], r['Statusvalue'], \
                  r['Pmaxvalue'], r['Pminvalue'], r['Pc1value'], r['Pc2value'], r['Qc1minvalue'], r['Qc1maxvalue'], r['Qc2minvalue'], r['Qc2maxvalue'], r['Rampagcvalue'], \
                      r['Ramp10value'], r['Ramp30value'], r['Rampqvalue'], r['apfvalue']] for r in res ]
       
    ret_array = [ str(r['BusNumbervalue']) + splitCharacter + str(r['activepowervalue']) + splitCharacter + str(r['reactivepowervalue']) + splitCharacter + str(r['Qmaxvalue']) + \
                 splitCharacter + str(r['Qminvalue']) + splitCharacter + str(r['Vgvalue']) + splitCharacter + str(r['mBasevalue']) + splitCharacter + str(r['Statusvalue'])+ \
                  splitCharacter + str(r['Pmaxvalue']) + splitCharacter + str(r['Pminvalue']) + splitCharacter + str(r['Pc1value']) + splitCharacter + str(r['Pc2value']) + \
                      splitCharacter + str(r['Qc1minvalue']) + splitCharacter + str(r['Qc1maxvalue']) + splitCharacter + str(r['Qc2minvalue']) + splitCharacter + str(r['Qc2maxvalue']) + \
                          splitCharacter + str(r['Rampagcvalue']) + splitCharacter + str(r['Ramp10value']) + splitCharacter + str(r['Ramp30value']) + splitCharacter + \
                              str(r['Rampqvalue']) + splitCharacter + str(r['apfvalue']) for r in res ]    
    
    print(len(ret_array))
    textfile = open("gen.txt", "w")
    for r in ret_array:
        textfile.write(r + "\n")
    textfile.close()
    return ret_array


# TODO: need to change the function argument 
def queryBusModelInput_new(numOfBus, numOfBranch, endPoint) -> list:
    queryVar = []
    for var in UKPowerGridModel.UKEbusModel.INPUT_VARIABLE_KEYS:
        queryVar.append("?" + str(var))    
    selectClause = " ".join(queryVar)
    
    label = "UK_Electrical_Grid_" + str(numOfBus) + "_Bus_" + str(numOfBranch) + "_Branch"
    
    queryStr = """
    PREFIX rdfs: <http://www.w3.org/2000/01/rdf-schema#>
    PREFIX j1:<http://www.theworldavatar.com/ontology/ontopowsys/PowSysRealization.owl#>
	PREFIX j2:<http://www.theworldavatar.com/ontology/ontocape/upper_level/system.owl#>
	PREFIX j3:<http://www.theworldavatar.com/ontology/ontopowsys/model/PowerSystemModel.owl#>
	PREFIX j4:<http://www.theworldavatar.com/ontology/meta_model/topology/topology.owl#>
	PREFIX j5:<http://www.theworldavatar.com/ontology/ontocape/model/mathematical_model.owl#>
	PREFIX j6:<http://www.theworldavatar.com/ontology/ontocape/chemical_process_system/CPS_behavior/behavior.owl#>
	PREFIX j7:<http://www.theworldavatar.com/ontology/ontocape/supporting_concepts/space_and_time/space_and_time_extended.owl#>
	PREFIX j8:<http://www.theworldavatar.com/ontology/ontocape/material/phase_system/phase_system.owl#>
	SELECT DISTINCT %s ?EBus
    
    
    #?BusNumbervalue ?typevalue ?activepowervalue ?reactivepowervalue ?Gsvalue ?Bsvalue ?areavalue ?VoltMagvalue ?VoltAnglevalue ?BaseKVvalue ?Zonevalue ?VMaxvalue ?VMinvalue

    WHERE {
        
    ?Model_EBus rdfs:label ?label .
    FILTER regex(?label, "%s") .
    
    ?Model_EBus j2:isComposedOfSubsystem ?EBus .
    ?EBus  a  j3:BusModel .     
   
    ?EBus   j5:hasModelVariable ?num .
    ?num  a  j3:BusNumber  . 
    ?num  j2:hasValue ?vnum .
    ?vnum   j2:numericalValue %s .

    ?EBus   j5:hasModelVariable ?type . 
    ?type  a  j3:BusType  .
    ?type  j2:hasValue ?vtype .
    ?vtype   j2:numericalValue %s .

    ?EBus   j5:hasModelVariable ?Pd . 
    ?Pd  a  j3:PdBus  . 
    ?Pd  j2:hasValue ?vpd .
    ?vpd   j2:numericalValue %s .

    ?EBus   j5:hasModelVariable ?Gd . 
    ?Gd  a  j3:GdBus  . 
    ?Gd  j2:hasValue ?vgd .
    ?vgd   j2:numericalValue %s .

    ?EBus   j5:hasModelVariable ?Gsvar . 
    ?Gsvar  a  j3:Gs  . 
    ?Gsvar  j2:hasValue ?vGsvar .
    ?vGsvar   j2:numericalValue %s .

    ?EBus   j5:hasModelVariable ?Bsvar . 
    ?Bsvar  a  j3:Bs  . 
    ?Bsvar  j2:hasValue ?vBsvar .
    ?vBsvar   j2:numericalValue %s .

    ?EBus   j5:hasModelVariable ?areavar . 
    ?areavar  a  j3:Area  .
    ?areavar  j2:hasValue ?vareavar . 
    ?vareavar   j2:numericalValue %s .

    ?EBus   j5:hasModelVariable ?VM . 
    ?VM  a  j3:Vm  . 
    ?VM  j2:hasValue ?vVM .
    ?vVM   j2:numericalValue %s .

    ?EBus   j5:hasModelVariable ?VA . 
    ?VA  a  j3:Va  . 
    ?VA  j2:hasValue ?vVA .
    ?vVA   j2:numericalValue %s .

    ?EBus   j5:hasModelVariable ?BKV . 
    ?BKV  a  j3:baseKV  . 
    ?BKV  j2:hasValue ?vBKV .
    ?vBKV   j2:numericalValue %s .

    ?EBus   j5:hasModelVariable ?zvar . 
    ?zvar  a  j3:Zone  . 
    ?zvar  j2:hasValue ?vzvar .
    ?vzvar   j2:numericalValue %s .

    ?EBus   j5:hasModelVariable ?vmaxvar .
    ?vmaxvar  a  j3:VmMax  .
    ?vmaxvar  j2:hasValue ?vvmaxvar .
    ?vvmaxvar   j2:numericalValue %s .

    ?EBus   j5:hasModelVariable ?vminvar . 
    ?vminvar  a  j3:VmMin  .
    ?vminvar  j2:hasValue ?vvminvar . 
    ?vvminvar   j2:numericalValue %s .
        }
    ORDER BY ASC(%s)
    """ % (selectClause, label, queryVar[0], queryVar[1], queryVar[2], queryVar[3], queryVar[4], queryVar[5], queryVar[6], queryVar[7], queryVar[8], \
        queryVar[9], queryVar[10], queryVar[11], queryVar[12], queryVar[0]) 
    
    # print(queryStr)
    print('remoteQuery queryBusModelInput')
    res: list = json.loads(performQuery(endPoint, queryStr))
    print('queryBusModelInput is done')
    
    if len(res) == 0:
      raise Exception("The queryBusModelInput is failed.")
      
    for r in res:
      for key in r.keys():
          if '\"^^' in  r[key]:
            r[key] = (r[key].split('\"^^')[0]).replace('\"','')   

    return res


def queryBusModelInput(numOfBus, numOfBranch, endPoint, splitCharacter):
    label = "UK_Electrical_Grid_" + str(numOfBus) + "_Bus_" + str(numOfBranch) + "_Branch"
    queryStr = """
    PREFIX rdfs: <http://www.w3.org/2000/01/rdf-schema#>
    PREFIX j1:<http://www.theworldavatar.com/ontology/ontopowsys/PowSysRealization.owl#>
	PREFIX j2:<http://www.theworldavatar.com/ontology/ontocape/upper_level/system.owl#>
	PREFIX j3:<http://www.theworldavatar.com/ontology/ontopowsys/model/PowerSystemModel.owl#>
	PREFIX j4:<http://www.theworldavatar.com/ontology/meta_model/topology/topology.owl#>
	PREFIX j5:<http://www.theworldavatar.com/ontology/ontocape/model/mathematical_model.owl#>
	PREFIX j6:<http://www.theworldavatar.com/ontology/ontocape/chemical_process_system/CPS_behavior/behavior.owl#>
	PREFIX j7:<http://www.theworldavatar.com/ontology/ontocape/supporting_concepts/space_and_time/space_and_time_extended.owl#>
	PREFIX j8:<http://www.theworldavatar.com/ontology/ontocape/material/phase_system/phase_system.owl#>
	SELECT ?BusNumbervalue ?typevalue ?activepowervalue ?reactivepowervalue ?Gsvalue ?Bsvalue ?areavalue ?VoltMagvalue ?VoltAnglevalue ?BaseKVvalue ?Zonevalue ?VMaxvalue ?VMinvalue

    WHERE {
        
    #?Model_EBus a j5:Submodel .
    ?Model_EBus rdfs:label ?label .
    FILTER regex(?label, "%s") .
    
    ?Model_EBus j2:isComposedOfSubsystem ?EBus .
    ?EBus  a  j3:BusModel .     
   
    ?EBus   j5:hasModelVariable ?num .
    ?num  a  j3:BusNumber  . 
    ?num  j2:hasValue ?vnum .
    ?vnum   j2:numericalValue ?BusNumbervalue .

    ?EBus   j5:hasModelVariable ?type . 
    ?type  a  j3:BusType  .
    ?type  j2:hasValue ?vtype .
    ?vtype   j2:numericalValue ?typevalue .

    ?EBus   j5:hasModelVariable ?Pd . 
    ?Pd  a  j3:PdBus  . 
    ?Pd  j2:hasValue ?vpd .
    ?vpd   j2:numericalValue ?activepowervalue .

    ?EBus   j5:hasModelVariable ?Gd . 
    ?Gd  a  j3:GdBus  . 
    ?Gd  j2:hasValue ?vgd .
    ?vgd   j2:numericalValue ?reactivepowervalue .

    ?EBus   j5:hasModelVariable ?Gsvar . 
    ?Gsvar  a  j3:Gs  . 
    ?Gsvar  j2:hasValue ?vGsvar .
    ?vGsvar   j2:numericalValue ?Gsvalue .

    ?EBus   j5:hasModelVariable ?Bsvar . 
    ?Bsvar  a  j3:Bs  . 
    ?Bsvar  j2:hasValue ?vBsvar .
    ?vBsvar   j2:numericalValue ?Bsvalue .

    ?EBus   j5:hasModelVariable ?areavar . 
    ?areavar  a  j3:Area  .
    ?areavar  j2:hasValue ?vareavar . 
    ?vareavar   j2:numericalValue ?areavalue .

    ?EBus   j5:hasModelVariable ?VM . 
    ?VM  a  j3:Vm  . 
    ?VM  j2:hasValue ?vVM .
    ?vVM   j2:numericalValue ?VoltMagvalue .

    ?EBus   j5:hasModelVariable ?VA . 
    ?VA  a  j3:Va  . 
    ?VA  j2:hasValue ?vVA .
    ?vVA   j2:numericalValue ?VoltAnglevalue .

    ?EBus   j5:hasModelVariable ?BKV . 
    ?BKV  a  j3:baseKV  . 
    ?BKV  j2:hasValue ?vBKV .
    ?vBKV   j2:numericalValue ?BaseKVvalue .

    ?EBus   j5:hasModelVariable ?zvar . 
    ?zvar  a  j3:Zone  . 
    ?zvar  j2:hasValue ?vzvar .
    ?vzvar   j2:numericalValue ?Zonevalue .

    ?EBus   j5:hasModelVariable ?vmaxvar .
    ?vmaxvar  a  j3:VmMax  .
    ?vmaxvar  j2:hasValue ?vvmaxvar .
    ?vvmaxvar   j2:numericalValue ?VMaxvalue .

    ?EBus   j5:hasModelVariable ?vminvar . 
    ?vminvar  a  j3:VmMin  .
    ?vminvar  j2:hasValue ?vvminvar . 
    ?vvminvar   j2:numericalValue ?VMinvalue .
        }
    ORDER BY ASC(?BusNumbervalue)
    """ %label 
    
    #print(queryStr)
    print('remoteQuery queryBusModelInput')
    res = json.loads(performQuery(endPoint, queryStr))
    print('queryBusModelInput is done')
    
    print(res, type(res))
    
    print(len(res))
    for r in res:
      for key in r.keys():
          if '\"^^' in  r[key]:
            r[key] = (r[key].split('\"^^')[0]).replace('\"','')   

    ret_array = [ str(r['BusNumbervalue']) + splitCharacter + str(r['typevalue']) + splitCharacter + str(r['activepowervalue']) + splitCharacter + str(r['reactivepowervalue']) + \
                 splitCharacter + str(r['Gsvalue']) + splitCharacter + str(r['Bsvalue']) + splitCharacter + str(r['areavalue']) + splitCharacter + str(r['VoltMagvalue'])+ \
                  splitCharacter + str(r['VoltAnglevalue']) + splitCharacter + str(r['BaseKVvalue']) + splitCharacter + str(r['Zonevalue']) + splitCharacter + str(r['VMaxvalue']) + \
                      splitCharacter + str(r['VMinvalue']) for r in res ]    
    
    textfile = open("bus.txt", "w")
    
    for r in ret_array:
        textfile.write(r + "\n")
    textfile.close()
    return ret_array

def queryBranchModelInput_new(numOfBus, numOfBranch, endPoint) -> list:
    
    queryVar = []
    for var in UKPowerGridModel.UKElineModel.INPUT_VARIABLE_KEYS:
        queryVar.append("?" + str(var))    
    selectClause = " ".join(queryVar)
    
    label = "UK_Electrical_Grid_" + str(numOfBus) + "_Bus_" + str(numOfBranch) + "_Branch"
    queryStr = """
    PREFIX rdfs: <http://www.w3.org/2000/01/rdf-schema#>
    PREFIX j1:<http://www.theworldavatar.com/ontology/ontopowsys/PowSysRealization.owl#>
	PREFIX j2:<http://www.theworldavatar.com/ontology/ontocape/upper_level/system.owl#>
	PREFIX j3:<http://www.theworldavatar.com/ontology/ontopowsys/model/PowerSystemModel.owl#>
	PREFIX j4:<http://www.theworldavatar.com/ontology/meta_model/topology/topology.owl#>
	PREFIX j5:<http://www.theworldavatar.com/ontology/ontocape/model/mathematical_model.owl#>
	PREFIX j6:<http://www.theworldavatar.com/ontology/ontocape/chemical_process_system/CPS_behavior/behavior.owl#>
	PREFIX j7:<http://www.theworldavatar.com/ontology/ontocape/supporting_concepts/space_and_time/space_and_time_extended.owl#>
	PREFIX j8:<http://www.theworldavatar.com/ontology/ontocape/material/phase_system/phase_system.owl#>
	SELECT DISTINCT %s ?EBranch
    
    # ?BusNumber1value ?BusNumber2value ?resistancevalue ?reactancevalue ?susceptancevalue ?rateAvalue ?rateBvalue ?rateCvalue ?ratiovalue ?anglevalue ?statusvalue ?angleminvalue ?anglemaxvalue

    WHERE {
     
    #?Model_EBus a j5:Submodel .
    ?Model rdfs:label ?label .
    FILTER regex(?label, "%s") .
    
    ?Model j2:isComposedOfSubsystem ?EBranch .
    ?EBranch  a  j3:ElectricalBranchModel .     
     

    ?EBranch   j5:hasModelVariable ?num1 . 
    ?num1  a  j3:BusFrom  . 
    ?num1  j2:hasValue ?vnum1 .
    ?vnum1   j2:numericalValue %s .

    ?EBranch   j5:hasModelVariable ?num2 . 
    ?num2  a  j3:BusTo  . 
    ?num2  j2:hasValue ?vnum2 .
    ?vnum2   j2:numericalValue %s .

    ?EBranch   j5:hasModelVariable ?res . 
    ?res  a  j3:R  . 
    ?res  j2:hasValue ?vres .
    ?vres   j2:numericalValue %s .

    ?EBranch   j5:hasModelVariable ?rea . 
    ?rea  a  j3:X  . 
    ?rea  j2:hasValue ?vrea .
    ?vrea   j2:numericalValue %s .

    ?EBranch   j5:hasModelVariable ?sus . 
    ?sus  a  j3:B  . 
    ?sus  j2:hasValue ?vsus .
    ?vsus   j2:numericalValue %s .

    ?EBranch   j5:hasModelVariable ?ratea . 
    ?ratea  a  j3:RateA  . 
    ?ratea  j2:hasValue ?vratea .
    ?vratea   j2:numericalValue %s .

    ?EBranch   j5:hasModelVariable ?rateb . 
    ?rateb  a  j3:RateB  . 
    ?rateb  j2:hasValue ?vrateb .
    ?vrateb   j2:numericalValue %s .

    ?EBranch   j5:hasModelVariable ?ratec . 
    ?ratec  a  j3:RateC  . 
    ?ratec  j2:hasValue ?vratec .
    ?vratec   j2:numericalValue %s .

    ?EBranch   j5:hasModelVariable ?ratio . 
    ?ratio  a  j3:RatioCoefficient  .
    ?ratio  j2:hasValue ?vratio . 
    ?vratio   j2:numericalValue %s .

    ?EBranch   j5:hasModelVariable ?ang . 
    ?ang  a  j3:Angle  . 
    ?ang  j2:hasValue ?vang .
    ?vang   j2:numericalValue %s .

    ?EBranch   j5:hasModelVariable ?stat . 
    ?stat  a  j3:BranchStatus . 
    ?stat  j2:hasValue ?vstat .
    ?vstat   j2:numericalValue %s .

    ?EBranch   j5:hasModelVariable ?angmin . 
    ?angmin  a  j3:AngleMin  .
    ?angmin  j2:hasValue ?vangmin . 
    ?vangmin   j2:numericalValue %s .

    ?EBranch   j5:hasModelVariable ?angmax . 
    ?angmax  a  j3:AngleMax  .
    ?angmax  j2:hasValue ?vangmax . 
    ?vangmax   j2:numericalValue %s .
    }
    ORDER BY ASC(%s)
    """ %(selectClause, label, queryVar[0], queryVar[1], queryVar[2], queryVar[3], queryVar[4], queryVar[5], queryVar[6], queryVar[7], queryVar[8], \
        queryVar[9], queryVar[10], queryVar[11], queryVar[12], queryVar[0])  
    
    # print(queryStr)
    
    print('remoteQuery queryBranchModelInput')
    res = json.loads(performQuery(endPoint, queryStr))
    print('queryBranchModelInput is done')
    
    if len(res) == 0:
      raise Exception("The queryBranchModelInput is failed.")
      
    for r in res:
      for key in r.keys():
          if '\"^^' in  r[key]:
            r[key] = (r[key].split('\"^^')[0]).replace('\"','')   
    
    return res

def queryBranchModelInput(numOfBus, numOfBranch, endPoint, splitCharacter):
    label = "UK_Electrical_Grid_" + str(numOfBus) + "_Bus_" + str(numOfBranch) + "_Branch"
    queryStr = """
    PREFIX rdfs: <http://www.w3.org/2000/01/rdf-schema#>
    PREFIX j1:<http://www.theworldavatar.com/ontology/ontopowsys/PowSysRealization.owl#>
	PREFIX j2:<http://www.theworldavatar.com/ontology/ontocape/upper_level/system.owl#>
	PREFIX j3:<http://www.theworldavatar.com/ontology/ontopowsys/model/PowerSystemModel.owl#>
	PREFIX j4:<http://www.theworldavatar.com/ontology/meta_model/topology/topology.owl#>
	PREFIX j5:<http://www.theworldavatar.com/ontology/ontocape/model/mathematical_model.owl#>
	PREFIX j6:<http://www.theworldavatar.com/ontology/ontocape/chemical_process_system/CPS_behavior/behavior.owl#>
	PREFIX j7:<http://www.theworldavatar.com/ontology/ontocape/supporting_concepts/space_and_time/space_and_time_extended.owl#>
	PREFIX j8:<http://www.theworldavatar.com/ontology/ontocape/material/phase_system/phase_system.owl#>
	SELECT ?BusNumber1value ?BusNumber2value ?resistancevalue ?reactancevalue ?susceptancevalue ?rateAvalue ?rateBvalue ?rateCvalue ?ratiovalue ?anglevalue ?statusvalue ?angleminvalue ?anglemaxvalue

    WHERE {
     
    #?Model_EBus a j5:Submodel .
    ?Model rdfs:label ?label .
    FILTER regex(?label, "%s") .
    
    ?Model j2:isComposedOfSubsystem ?EBranch .
    ?EBranch  a  j3:ElectricalBranchModel .     
     

    ?EBranch   j5:hasModelVariable ?num1 . 
    ?num1  a  j3:BusFrom  . 
    ?num1  j2:hasValue ?vnum1 .
    ?vnum1   j2:numericalValue ?BusNumber1value .

    ?EBranch   j5:hasModelVariable ?num2 . 
    ?num2  a  j3:BusTo  . 
    ?num2  j2:hasValue ?vnum2 .
    ?vnum2   j2:numericalValue ?BusNumber2value .

    ?EBranch   j5:hasModelVariable ?res . 
    ?res  a  j3:R  . 
    ?res  j2:hasValue ?vres .
    ?vres   j2:numericalValue ?resistancevalue .

    ?EBranch   j5:hasModelVariable ?rea . 
    ?rea  a  j3:X  . 
    ?rea  j2:hasValue ?vrea .
    ?vrea   j2:numericalValue ?reactancevalue .

    ?EBranch   j5:hasModelVariable ?sus . 
    ?sus  a  j3:B  . 
    ?sus  j2:hasValue ?vsus .
    ?vsus   j2:numericalValue ?susceptancevalue .

    ?EBranch   j5:hasModelVariable ?ratea . 
    ?ratea  a  j3:RateA  . 
    ?ratea  j2:hasValue ?vratea .
    ?vratea   j2:numericalValue ?rateAvalue .

    ?EBranch   j5:hasModelVariable ?rateb . 
    ?rateb  a  j3:RateB  . 
    ?rateb  j2:hasValue ?vrateb .
    ?vrateb   j2:numericalValue ?rateBvalue .

    ?EBranch   j5:hasModelVariable ?ratec . 
    ?ratec  a  j3:RateC  . 
    ?ratec  j2:hasValue ?vratec .
    ?vratec   j2:numericalValue ?rateCvalue .

    ?EBranch   j5:hasModelVariable ?ratio . 
    ?ratio  a  j3:RatioCoefficient  .
    ?ratio  j2:hasValue ?vratio . 
    ?vratio   j2:numericalValue ?ratiovalue .

    ?EBranch   j5:hasModelVariable ?ang . 
    ?ang  a  j3:Angle  . 
    ?ang  j2:hasValue ?vang .
    ?vang   j2:numericalValue ?anglevalue .

    ?EBranch   j5:hasModelVariable ?stat . 
    ?stat  a  j3:BranchStatus . 
    ?stat  j2:hasValue ?vstat .
    ?vstat   j2:numericalValue ?statusvalue .

    ?EBranch   j5:hasModelVariable ?angmin . 
    ?angmin  a  j3:AngleMin  .
    ?angmin  j2:hasValue ?vangmin . 
    ?vangmin   j2:numericalValue ?angleminvalue .

    ?EBranch   j5:hasModelVariable ?angmax . 
    ?angmax  a  j3:AngleMax  .
    ?angmax  j2:hasValue ?vangmax . 
    ?vangmax   j2:numericalValue ?anglemaxvalue .
    }
    ORDER BY ASC(?BusNumber1value)
    """ %label 
    
    print('remoteQuery queryBranchModelInput')
    res = json.loads(performQuery(endPoint, queryStr))
    print('queryBranchModelInput is done')
    for r in res:
      for key in r.keys():
          if '\"^^' in  r[key]:
            r[key] = (r[key].split('\"^^')[0]).replace('\"','')   

    ret_array = [ str(r['BusNumber1value']) + splitCharacter + str(r['BusNumber2value']) + splitCharacter + str(r['resistancevalue']) + splitCharacter + str(r['reactancevalue']) + \
                 splitCharacter + str(r['susceptancevalue']) + splitCharacter + str(r['rateAvalue']) + splitCharacter + str(r['rateBvalue']) + splitCharacter + str(r['rateCvalue'])+ \
                  splitCharacter + str(r['ratiovalue']) + splitCharacter + str(r['anglevalue']) + splitCharacter + str(r['statusvalue']) + splitCharacter + str(r['angleminvalue']) + \
                      splitCharacter + str(r['anglemaxvalue']) for r in res ]    
    
    textfile = open("branch.txt", "w")
    
    for r in ret_array:
        textfile.write(r + "\n")
    textfile.close()
    return ret_array

if __name__ == '__main__':
    ## res = queryGeneratorModelInput(10, 14, 'ukdigitaltwin_test1', ',')
    ## res = queryGeneratorModelInput(29, 99, 'ukdigitaltwin_test1', ',')
    ## res = queryBusModelInput(10, 14, 'ukdigitaltwin_test1', ',')
    ## res = queryBusModelInput(29, 99, 'ukdigitaltwin_test1', ',')
    ## res = queryBranchModelInput(10, 14, 'ukdigitaltwin_test1', ',')
    ## res = queryBranchModelInput(29, 99, 'ukdigitaltwin_test1', ',')

    
    # res = queryBusModelInput_new(10, 14, 'ukdigitaltwin_test1')
    res = queryBranchModelInput_new(10, 14, 'ukdigitaltwin_test1')
    # res = queryGeneratorModelInput_new(10, 14, 'ukdigitaltwin_test1')
    
    print(len(res))
    