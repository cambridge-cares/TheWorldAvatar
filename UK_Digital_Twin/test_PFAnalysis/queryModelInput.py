import os, sys, json
BASE = os.path.dirname(os.path.dirname(os.path.abspath(__file__)))
sys.path.insert(0, BASE)
from UK_Digital_Twin_Package.queryInterface import performQuery
from UK_Digital_Twin_Package import UKPowerGridModel

def queryGeneratorModelInput_new(numOfBus, numOfBranch, endPoint) -> list:
    
    queryVar = []
    for var in UKPowerGridModel.UKEGenModel.INPUT_VARIABLE:
        queryVar.append("?" + str(var))    
    selectClause = " ".join(queryVar)
    
    label = "UK_Electrical_Grid_" + str(numOfBus) + "_Bus_" + str(numOfBranch) + "_Branch"
    queryStr = f"""
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

    WHERE {{
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
        }}
        ORDER BY ASC(%s)
    """ % (selectClause, label, queryVar[0], queryVar[1], queryVar[2], queryVar[3], queryVar[4], queryVar[5], queryVar[6], queryVar[7], queryVar[8], \
        queryVar[9], queryVar[10], queryVar[11], queryVar[12], queryVar[13], queryVar[14], queryVar[15], queryVar[16], queryVar[17], queryVar[18], \
            queryVar[19], queryVar[20], queryVar[0]) 

    print('remoteQuery queryGeneratorModelInput')
    res = json.loads(performQuery(endPoint, queryStr))
    print('queryGeneratorModelInput is done')
    
    if len(res) == 0:
      raise Exception("The queryGeneratorModelInput is failed.")  
    return res