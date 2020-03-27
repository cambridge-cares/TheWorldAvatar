var scenario = "base";
// var prefix = "http://localhost:8080";
var prefix = "http://www.jparksimulator.com"; //wouldn't work without the www apparently>
iriofnetwork = 'http://www.jparksimulator.com/kb/sgp/jurongisland/jurongislandpowernetwork/JurongIslandPowerNetwork.owl#JurongIsland_PowerNetwork';
var infoWindow; 
var marker;
var actualCarbon = 0.00;
var emissionValueForSingapore = 50908130;
var designCarbon= 0.00;
let actualCarbonYr = 0.0;
let designCarbonYr =0.0;
let wildPercentage =0.0;
let wildPercentage2 =0.0;
let dict = {};
var numTypeGen = '';
var kmlLayer;
var markers = [];
var nameSet = [];
var batterylist = null;
var branchInfo = "PREFIX j1:<http://www.theworldavatar.com/ontology/ontopowsys/PowSysRealization.owl#> "
    + "PREFIX j2:<http://www.theworldavatar.com/ontology/ontocape/upper_level/system.owl#> "
    + "PREFIX j3:<http://www.theworldavatar.com/ontology/ontopowsys/model/PowerSystemModel.owl#> "
    + "PREFIX j4:<http://www.theworldavatar.com/ontology/meta_model/topology/topology.owl#> "
    + "PREFIX j5:<http://www.theworldavatar.com/ontology/ontocape/model/mathematical_model.owl#> "
    + "PREFIX j6:<http://www.theworldavatar.com/ontology/ontocape/chemical_process_system/CPS_behavior/behavior.owl#> "
    + "PREFIX j7:<http://www.theworldavatar.com/ontology/ontocape/supporting_concepts/space_and_time/space_and_time_extended.owl#> "
    + "PREFIX j8:<http://www.theworldavatar.com/ontology/ontocape/material/phase_system/phase_system.owl#> "
    + "SELECT ?entity ?V_R ?V_X ?V_B ?V_RateA ?V_RateB ?V_RateC ?V_RatioCoeff ?V_Angle ?V_Angle_unit ?V_Status ?V_AngleMin ?V_AngleMin_unit ?V_AngleMax ?V_AngleMax_unit ?V_ToBusNumber "

    + "WHERE {?entity  a  j1:UndergroundCable  ." 
    + "?entity   j2:isModeledBy ?model ."
    + "?model   j5:hasModelVariable ?res ." 

    + "?res  a  j3:R  ." 
    + "?res  j2:hasValue ?vres ."
    + "?vres   j2:numericalValue ?V_R ." // resistance
    + "?vres   j2:hasUnitOfMeasure ?V_R_unit ." // resistance

    + "?model   j5:hasModelVariable ?rea ." 
    + "?rea  a  j3:X  ." 
    + "?rea  j2:hasValue ?vrea ."
    + "?vrea   j2:numericalValue ?V_X ." // reactance

    + "?model   j5:hasModelVariable ?sus ." 
    + "?sus  a  j3:B  ." 
    + "?sus  j2:hasValue ?vsus ."
    + "?vsus   j2:numericalValue ?V_B ." // susceptance

    + "?model   j5:hasModelVariable ?ratea ." 
    + "?ratea  a  j3:RateA  ." 
    + "?ratea  j2:hasValue ?vratea ."
    + "?vratea   j2:numericalValue ?V_RateA ." // rateA

    + "?model   j5:hasModelVariable ?rateb ." 
    + "?rateb  a  j3:RateB  ." 
    + "?rateb  j2:hasValue ?vrateb ."
    + "?vrateb   j2:numericalValue ?V_RateB ." // rateB

    + "?model   j5:hasModelVariable ?ratec ." 
    + "?ratec  a  j3:RateC  ." 
    + "?ratec  j2:hasValue ?vratec ."
    + "?vratec   j2:numericalValue ?V_RateC ." // rateC

    + "?model   j5:hasModelVariable ?ratio ." 
    + "?ratio  a  j3:RatioCoefficient  ."
    + "?ratio  j2:hasValue ?vratio ." 
    + "?vratio   j2:numericalValue ?V_RatioCoeff ." // ratio

    + "?model   j5:hasModelVariable ?ang ." 
    + "?ang  a  j3:Angle  ." 
    + "?ang  j2:hasValue ?vang ."
    + "?vang   j2:numericalValue ?V_Angle ." // angle
    + "?vang   j2:hasUnitOfMeasure ?V_Angle_unit ." // angle

    + "?model   j5:hasModelVariable ?stat ." 
    + "?stat  a  j3:BranchStatus ." 
    + "?stat  j2:hasValue ?vstat ."
    + "?vstat   j2:numericalValue ?V_Status ." // status

    + "?model   j5:hasModelVariable ?tobusnum ." 
    + "?tobusnum  a  j3:BranchStatus ." 
    + "?tobusnum  j2:hasValue ?vtobusnum ."
    + "?vtobusnum   j2:numericalValue ?V_ToBusNumber ." // status

    + "?model   j5:hasModelVariable ?angmin ." 
    + "?angmin  a  j3:AngleMin  ."
    + "?angmin  j2:hasValue ?vangmin ." 
    + "?vangmin   j2:numericalValue ?V_AngleMin ." // anglemin
    + "?vangmin   j2:hasUnitOfMeasure ?V_AngleMin_unit ." // anglemin

    + "?model   j5:hasModelVariable ?angmax ." 
    + "?angmax  a  j3:AngleMax  ."
    + "?angmax  j2:hasValue ?vangmax ." 
    + "?vangmax   j2:numericalValue ?V_AngleMax ." // anglemax
    + "?vangmax   j2:hasUnitOfMeasure ?V_AngleMax_unit ." // anglemin

    + "}";
var busInfo = "PREFIX j1:<http://www.theworldavatar.com/ontology/ontopowsys/PowSysRealization.owl#> "
    + "PREFIX j2:<http://www.theworldavatar.com/ontology/ontocape/upper_level/system.owl#> "
    + "PREFIX j3:<http://www.theworldavatar.com/ontology/ontopowsys/model/PowerSystemModel.owl#> "
    + "PREFIX j4:<http://www.theworldavatar.com/ontology/meta_model/topology/topology.owl#> "
    + "PREFIX j5:<http://www.theworldavatar.com/ontology/ontocape/model/mathematical_model.owl#> "
    + "PREFIX j7:<http://www.theworldavatar.com/ontology/ontocape/supporting_concepts/space_and_time/space_and_time_extended.owl#> "
    + "SELECT ?entity ?V_Pd ?V_Pd_unit ?V_Pd_Gen ?V_Pd_Gen_unit ?V_Gd ?V_Gd_unit ?V_Gd_Gen ?V_Gd_Gen_unit " 
    + "?V_Gs ?V_Bs ?V_Vm ?V_Va ?V_Va_unit ?V_BaseKV ?V_BaseKV_unit ?V_VmMax ?V_VmMin ?V_x ?V_x_unit ?V_y ?V_y_unit "

    + "WHERE {?entity  a  j1:BusNode  ." 
    + "?entity   j2:isModeledBy ?model ."

    + "?model   j5:hasModelVariable ?Pd ." 
    + "?Pd  a  j3:PdBus  ." 
    + "?Pd  j2:hasValue ?vpd ."
    + "?vpd   j2:numericalValue ?V_Pd ." // pd
    + "?vpd   j2:hasUnitOfMeasure ?V_Pd_unit ." // pd unit

    + "?model   j5:hasModelVariable ?PdGen ." 
    + "?PdGen  a  j3:PdGen  ." 
    + "?PdGen  j2:hasValue ?vpdgen ."
    + "?vpdgen   j2:numericalValue ?V_Pd_Gen ." // pdgen
    + "?vpdgen   j2:hasUnitOfMeasure ?V_Pd_Gen_unit ." // pdgen
    
    + "?model   j5:hasModelVariable ?Gd ." 
    + "?Gd  a  j3:GdBus  ." 
    + "?Gd  j2:hasValue ?vgd ."
    + "?vgd   j2:numericalValue ?V_Gd ." // Gd
    + "?vgd   j2:hasUnitOfMeasure ?V_Gd_unit ." // Gd
    
    + "?model   j5:hasModelVariable ?Gd_Gen ." 
    + "?Gd_Gen  a  j3:GdGen  ." 
    + "?Gd_Gen  j2:hasValue ?vgdgen ."
    + "?vgdgen   j2:numericalValue ?V_Gd_Gen ." // Gdgen
    + "?vgdgen   j2:hasUnitOfMeasure ?V_Gd_Gen_unit ." // Gdgen


    + "?model   j5:hasModelVariable ?Gsvar ." 
    + "?Gsvar  a  j3:Gs  ." 
    + "?Gsvar  j2:hasValue ?vGsvar ."
    + "?vGsvar   j2:numericalValue ?V_Gs ." // Gs (has no unit)

    + "?model   j5:hasModelVariable ?Bsvar ." 
    + "?Bsvar  a  j3:Bs  ." 
    + "?Bsvar  j2:hasValue ?vBsvar ."
    + "?vBsvar   j2:numericalValue ?V_Bs ." // Bs (has no unit)

    + "?model   j5:hasModelVariable ?VM ." 
    + "?VM  a  j3:Vm  ." 
    + "?VM  j2:hasValue ?vVM ."
    + "?vVM   j2:numericalValue ?V_Vm ." // Vm

    + "?model   j5:hasModelVariable ?VA ." 
    + "?VA  a  j3:Va  ." 
    + "?VA  j2:hasValue ?vVA ."
    + "?vVA   j2:numericalValue ?V_Va ." // Va
    + "?vVA   j2:hasUnitOfMeasure ?V_Va_unit ." // Va

    + "?model   j5:hasModelVariable ?BKV ." 
    + "?BKV  a  j3:baseKV  ." 
    + "?BKV  j2:hasValue ?vBKV ."
    + "?vBKV   j2:numericalValue ?V_BaseKV ." // Base KV
    + "?vBKV   j2:hasUnitOfMeasure ?V_BaseKV_unit ." // Base KV
    
    + "?model   j5:hasModelVariable ?vmaxvar ." 
    + "?vmaxvar  a  j3:VmMax  ."
    + "?vmaxvar  j2:hasValue ?vvmaxvar ." 
    + "?vvmaxvar   j2:numericalValue ?V_VmMax ." // Vmax

    + "?model   j5:hasModelVariable ?vminvar ." 
    + "?vminvar  a  j3:VmMin  ."
    + "?vminvar  j2:hasValue ?vvminvar ." 
    + "?vvminvar   j2:numericalValue ?V_VmMin ." // Vmin
    
    + "?coorsys  j7:hasProjectedCoordinate_y  ?y  ." 
    + "?y  j2:hasValue ?vy ." 
    + "?vy  j2:numericalValue ?V_y ."//longitude
    + "?vy  j2:hasUnitOfMeasure ?V_y_unit ."//longitude

    + "?coorsys  j7:hasProjectedCoordinate_x  ?x  ."
    + "?x  j2:hasValue ?vx ." 
    + "?vx  j2:numericalValue ?V_x ."//latitude
    + "?vx  j2:hasUnitOfMeasure ?V_x_unit ."//latitude
    

    + "}";
var genInfo = "PREFIX j1:<http://www.theworldavatar.com/ontology/ontopowsys/PowSysRealization.owl#> "
    + "PREFIX j2:<http://www.theworldavatar.com/ontology/ontocape/upper_level/system.owl#> "
    + "PREFIX j3:<http://www.theworldavatar.com/ontology/ontopowsys/model/PowerSystemModel.owl#> "
    + "PREFIX j5:<http://www.theworldavatar.com/ontology/ontocape/model/mathematical_model.owl#> "
    + "PREFIX j7:<http://www.theworldavatar.com/ontology/ontocape/supporting_concepts/space_and_time/space_and_time_extended.owl#> "
    + "PREFIX j9:<http://www.theworldavatar.com/ontology/ontoeip/system_aspects/system_performance.owl#> "
    + "PREFIX technical_system:<http://www.theworldavatar.com/ontology/ontocape/upper_level/technical_system.owl#> "
    + "SELECT ?entity ?V_BusNumber ?V_PGen ?V_PGen_unit ?V_QGen ?V_QGen_unit ?V_Qmax ?V_Qmax_unit ?V_Qmin ?V_Qmin_unit ?V_Vg ?V_mBase ?V_mBase_unit "
    + "?V_Pmax ?V_Pmax_unit ?V_Pmin ?V_Pmin_unit ?V_Pc1 ?V_Pc2 ?V_Qc1Min ?V_Qc1Max "
    + "?V_Qc2Min ?V_Qc2Max ?V_Ramp_agc ?V_Ramp_10 ?V_Ramp_30 ?V_Ramp_q ?V_APF "
    + "?V_StartupCost ?V_ShutdownCost ?V_genCostn ?V_genCostn1 ?V_genCostn2 ?V_genCostc0 "

    + "WHERE {?entity  a  j1:PowerGenerator  ."
    + "?entity   j2:isModeledBy ?model ."

    + "?model   j5:hasModelVariable ?num ." 
    + "?num  a  j3:BusNumber  ." 
    + "?num  j2:hasValue ?vnum ."
    + "?vnum   j2:numericalValue ?V_BusNumber ." // number

    + "?model   j5:hasModelVariable ?Pg ." 
    + "?Pg  a  j3:Pg  ." 
    + "?Pg  j2:hasValue ?vpg ."
    + "?vpg   j2:numericalValue ?V_PGen ." // pg
    + "?vpg   j2:hasUnitOfMeasure ?V_PGen_unit ." // pg

    + "?model   j5:hasModelVariable ?Qg ." 
    + "?Qg  a  j3:Qg  ." 
    + "?Qg  j2:hasValue ?vqg ."
    + "?vqg   j2:numericalValue ?V_QGen ." // qg
    + "?vqg   j2:hasUnitOfMeasure ?V_QGen_unit ." // qg

    + "?model   j5:hasModelVariable ?qmax ." 
    + "?qmax  a  j3:QMax  ." 
    + "?qmax  j2:hasValue ?vqmax ."
    + "?vqmax   j2:numericalValue ?V_Qmax ." // qmax
    + "?vqmax   j2:hasUnitOfMeasure ?V_Qmax_unit ." // qmax

    + "?model   j5:hasModelVariable ?qmin ." 
    + "?qmin  a  j3:QMin  ." 
    + "?qmin  j2:hasValue ?vqmin ."
    + "?vqmin   j2:numericalValue ?V_Qmin ." // qmin
    + "?vqmin   j2:hasUnitOfMeasure ?V_Qmin_unit ." // qmin

    + "?model   j5:hasModelVariable ?Vg ." 
    + "?Vg  a  j3:Vg  ." 
    + "?Vg  j2:hasValue ?vVg ."
    + "?vVg   j2:numericalValue ?V_Vg ." // vg

	+ "OPTIONAL{?model   j5:hasModelVariable ?mbase }" 
	+ "OPTIONAL{?mbase  a  j3:mBase  }" 
	+ "OPTIONAL{?mbase  j2:hasValue ?vmbase }"
	+ "OPTIONAL{?vmbase   j2:numericalValue ?V_mBase }" // mbase
	+ "OPTIONAL{?vmbase   j2:hasUnitOfMeasure ?V_mBase_unit }" // mbase

    + "?model   j5:hasModelVariable ?pmax ." 
    + "?pmax  a  j3:PMax  ." 
    + "?pmax  j2:hasValue ?vpmax ."
    + "?vpmax   j2:numericalValue ?V_Pmax ." // pmax
    + "?vpmax   j2:hasUnitOfMeasure  ?V_Pmax_unit ." // pmax

    + "?model   j5:hasModelVariable ?pmin ." 
    + "?pmin  a  j3:PMin  ." 
    + "?pmin  j2:hasValue ?vpmin ."
    + "?vpmin   j2:numericalValue ?V_Pmin ." // pmin
    + "?vpmin   j2:hasUnitOfMeasure?V_Pmin_unit ." // pmin

    + "?model   j5:hasModelVariable ?pc1 ." 
    + "?pc1  a  j3:Pc1  ." 
    + "?pc1  j2:hasValue ?vpc1 ."
    + "?vpc1   j2:numericalValue ?V_Pc1 ." // pc1

    + "?model   j5:hasModelVariable ?pc2 ." 
    + "?pc2  a  j3:Pc2  ." 
    + "?pc2  j2:hasValue ?vpc2 ."
    + "?vpc2   j2:numericalValue ?V_Pc2 ." // pc2

    + "?model   j5:hasModelVariable ?qc1min ." 
    + "?qc1min  a  j3:QC1Min  ."
    + "?qc1min  j2:hasValue ?vqc1min ." 
    + "?vqc1min   j2:numericalValue ?V_Qc1Min ." // qc1min

    + "?model   j5:hasModelVariable ?Qc1max ." 
    + "?Qc1max  a  j3:QC1Max  ."
    + "?Qc1max  j2:hasValue ?vQc1max ." 
    + "?vQc1max   j2:numericalValue ?V_Qc1Max ." // qc1max

    + "?model   j5:hasModelVariable ?qc2min ." 
    + "?qc2min  a  j3:QC2Min  ."
    + "?qc2min  j2:hasValue ?vqc2min ."
    + "?vqc2min   j2:numericalValue ?V_Qc2Min ." // qc2min

    + "?model   j5:hasModelVariable ?Qc2max ."
    + "?Qc2max  a  j3:QC2Max  ."
    + "?Qc2max  j2:hasValue ?vQc2max ." 
    + "?vQc2max   j2:numericalValue ?V_Qc2Max ." // qc2max

    + "?model   j5:hasModelVariable ?rampagc ." 
    + "?rampagc  a  j3:Rampagc  ."
    + "?rampagc  j2:hasValue ?vrampagc ." 
    + "?vrampagc   j2:numericalValue ?V_Ramp_agc ." // rampagc

    + "?model   j5:hasModelVariable ?ramp10 ." 
    + "?ramp10  a  j3:Ramp10  ."
    + "?ramp10  j2:hasValue ?vramp10 ."
    + "?vramp10   j2:numericalValue ?V_Ramp_10 ." // ramp10

    + "?model   j5:hasModelVariable ?ramp30 ." 
    + "?ramp30  a  j3:Ramp30  ."
    + "?ramp30  j2:hasValue ?vramp30 ." 
    + "?vramp30   j2:numericalValue ?V_Ramp_30 ." // ramp30

    + "?model   j5:hasModelVariable ?rampq ." 
    + "?rampq  a  j3:Rampq  ." 
    + "?rampq  j2:hasValue ?vrampq ."
    + "?vrampq   j2:numericalValue ?V_Ramp_q ." // rampq

    + "?model   j5:hasModelVariable ?apf ."
    + "?apf  a  j3:APF  ." 
    + "?apf  j2:hasValue ?vapf ."
    + "?vapf   j2:numericalValue ?V_APF ." // apf
    
    + "?model   j5:hasModelVariable ?startup ." 
    + "?startup  a  j3:StartCost  ."
    + "?startup  j2:hasValue ?vstartup ." 
    + "?vstartup   j2:numericalValue ?V_StartupCost ." //startup cost

    + "?model   j5:hasModelVariable ?shutdown ." 
    + "?shutdown  a  j3:StopCost  ."
    + "?shutdown  j2:hasValue ?vshutdown ." 
    + "?vshutdown   j2:numericalValue ?V_ShutdownCost ."  //shutdown cost
    
    + "?model   j5:hasModelVariable ?gencostn ." 
    + "?gencostn  a  j3:genCostn  ."
    + "?gencostn  j2:hasValue ?vgencostn ." 
    + "?vgencostn   j2:numericalValue ?V_genCostn ." //genCostn

    + "?model   j5:hasModelVariable ?gencostn1 ." 
    + "?gencostn1  a  j3:genCostcn-1  ."
    + "?gencostn1  j2:hasValue ?vgencostn1 ." 
    + "?vgencostn1   j2:numericalValue ?V_genCostn1 ." //genCostn-1

    + "?model   j5:hasModelVariable ?gencostn2 ." 
    + "?gencostn2  a  j3:genCostcn-2  ."
    + "?gencostn2  j2:hasValue ?vgencostn2 ." 
    + "?vgencostn2   j2:numericalValue ?V_genCostn2 ."//genCostn-2

    + "?model   j5:hasModelVariable ?gencostc ." 
    + "?gencostc  a  j3:genCostc0  ."
    + "?gencostc  j2:hasValue ?vgencostc ." 
    + "?vgencostc   j2:numericalValue ?V_genCostc0 ." //genCostc0

    + "}";

var genInfo2 = "PREFIX j1:<http://www.theworldavatar.com/ontology/ontopowsys/PowSysRealization.owl#> "
        + "PREFIX j2:<http://www.theworldavatar.com/ontology/ontocape/upper_level/system.owl#> "
        + "PREFIX j3:<http://www.theworldavatar.com/ontology/ontopowsys/model/PowerSystemModel.owl#> "
        + "PREFIX j5:<http://www.theworldavatar.com/ontology/ontocape/model/mathematical_model.owl#> "
        + "PREFIX j7:<http://www.theworldavatar.com/ontology/ontocape/supporting_concepts/space_and_time/space_and_time_extended.owl#> "
        + "PREFIX j9:<http://www.theworldavatar.com/ontology/ontoeip/system_aspects/system_performance.owl#> "
        + "PREFIX technical_system:<http://www.theworldavatar.com/ontology/ontocape/upper_level/technical_system.owl#> "
        + "SELECT ?entity ?V_QGen ?V_QGen_unit ?V_x ?V_x_unit ?V_y ?V_y_unit ?V_Actual_CO2_Emission ?V_Actual_CO2_Emission_unit ?V_Design_CO2_Emission ?V_Design_CO2_Emission_unit "

        + "WHERE {?entity  a  j1:PowerGenerator  ."
        + "?entity   technical_system:realizes ?generation ."
        + "?generation j9:hasEmission ?emission ." 

        + "?model   j5:hasModelVariable ?Qg ." 
        + "?Pg  a  j3:Qg  ." 
        + "?Pg  j2:hasValue ?vpg ."
        + "?vpg   j2:numericalValue ?V_QGen ." // Qg
        + "?vpg   j2:hasUnitOfMeasure ?V_QGen_unit ." // Qg

        + "?emission a j9:Actual_CO2_Emission ."
        + "?emission   j2:hasValue ?valueemission ."
        + "?valueemission   j2:numericalValue ?V_Actual_CO2_Emission ." //
        + "?valueemission   j2:hasUnitOfMeasure ?V_Actual_CO2_Emission_unit ." //


        + "?generation j9:hasEmission ?v_emission ." 
        + "?v_emission a j9:CO2_emission ."
        + "OPTIONAL {?v_emission   j2:hasValue ?valueemission_d }"
        + "OPTIONAL {?valueemission_d   j2:numericalValue ?V_Design_CO2_Emission }" //
        + "OPTIONAL {?valueemission_d   j2:hasUnitOfMeasure ?V_Design_CO2_Emission_unit }" //

        + "?coorsys  j7:hasProjectedCoordinate_y  ?y  ." 
        + "?y  j2:hasValue ?vy ." 
        + "?vy  j2:numericalValue ?V_y ."
        + "?vy  j2:hasUnitOfMeasure ?V_y_unit ."
        //
        + "?coorsys  j7:hasProjectedCoordinate_x  ?x  ."
        + "?x  j2:hasValue ?vx ." 
        + "?vx  j2:numericalValue ?V_x ."//longitude
        + "?vx  j2:hasUnitOfMeasure ?V_x_unit ."//longitude


        + "}";


/*** accesses parallel scenarios through these helper functions
 * @param scenarioname the name of the scenario, be it base or specific folder 
 * @param iri: iri of resource to be queried. 
 * @param sparql: the sparql update to be fired
 * @returns modified url for query
 */
function createUrlForSparqlUpdate(scenarioname, iri, sparql) {

    var url2 = prefix + '/jps/scenario/' + scenarioname + '/update?query=';
    urljson = {"scenarioresource":iri,"sparqlupdate":sparql};
    url2 += encodeURIComponent(JSON.stringify(urljson)); 
    //url2 += JSON.stringify(urljson); 
    return url2;    
}
/*** accesses parallel scenarios through these helper functions
 * @param scenarioname the name of the scenario, be it base or specific folder 
 * @param iri: iri of resource to be queried. 
 * @param sparql: the sparql query to be fired
 * @returns modified url for update
 */
function createUrlForSparqlQuery(scenarioname, iri, sparql) {

    var url2 = prefix + '/jps/scenario/' + scenarioname + '/query?query=';
    urljson = {"scenarioresource":iri,"sparqlquery":sparql};
    url2 += encodeURIComponent(JSON.stringify(urljson)); 
    //url2 += JSON.stringify(urljson); 
    return url2;    
}
/*** accesses parallel scenarios through these helper functions
 * @param scenarioname the name of the scenario, be it base or specific folder 
 * @param agenturl: GET request to Java Backend Servlet
 * @param sparql: JSON packets or what not that the Java backend could request. 
 * @returns modified url for update
 */
function createUrlForAgent(scenarioname, agenturl, agentparams) {

    var url;
    if ((scenarioname == null) || scenarioname == "base") {
        url = agenturl;
    } else {
        agentparams['scenarioagentoperation'] = agenturl;
        var scenariourl = prefix + '/jps/scenario/' + scenarioname + '/call';
        url = scenariourl;
    }

    return url + "?query=" + encodeURIComponent(JSON.stringify(agentparams));
}
/***
 *  displays the CO2 values of the generators by reading from AggregationEmissionAgent
 *  writes to local variables actual and design carbon 
 * @param data: electricalnetwork topnode iri
 */
function displayCO2(data){
    //read the value of CO2 and display upon calling
   // var agenturl =  prefix + '/JPS_POWSYS/ENVisualization/readGenerator' ;
	var agenturl =  prefix + '/JPS_POWSYS/AggregationEmissionAgent/aggregateemission' ;
    var kmlurl = createUrlForAgent(scenario, agenturl, data);
    console.log(kmlurl);
    var request = $.ajax({
        url: kmlurl,
        type: 'GET',
        async: true,
        contentType: 'application/json; charset=utf-8', 
        success: function(info, textStatus){
            console.log(info);
            console.log(textStatus);
            console.log("successful execution");
        }, 
        fail: function (xhr, textStatus, errorThrown){
            console.log(xhr);
            console.log(textStatus);
            console.log(errorThrown);
            setTimeout(function() {
                console.log('timeout');
            }, 20000);
            document.getElementById("loader").style.display = "none";
            displayCO2(data);
        }
    });     
    
    request.done(function(info) {
        console.log(info);
        var obj0 = JSON.parse(info);
        actualCarbon = parseFloat(obj0.actual);
        actualCarbonYr = actualCarbon*8760/1000000;
        wildPercentage = (actualCarbonYr/emissionValueForSingapore)*100*1000000;
        designCarbon = parseFloat(obj0.design);    
        designCarbonYr = designCarbon*8760/1000000;
        wildPercentage2 = (designCarbonYr/emissionValueForSingapore)*100*1000000;  
        document.getElementById("loader").style.display = "none";
    });
}
/** creates the tiny pop up window if there are more than one element on a kml element. I.E. generator. 
 * 
 * @param {KML element} kmlEvent 
 */
function setKMLMenu(kmlEvent){
    var data = kmlEvent.featureData;
    var nameString = data.name.substr(1);
    var names = nameString.split('[');
    var buttonsList = '<p>Please select the Entity you would like to modify</p>';
    for(var index in names)
    {
        var name = names[index];

        buttonsList = buttonsList + '<div><label>' + name.split('#')[1] + '</label>' +
            '<button onclick="selectEBus(event)" style= "cursor: pointer;" id="' + name + '"> > </span></div>'
    }

    buttonsList = '<div id="buttonContainer">'+ buttonsList +'</div><hr/><div id="inputsContainer"></div>';
    // set the content of the popup window.
    kmlEvent.featureData.infoWindowHtml = '<div>' + buttonsList + '</div>';

}
/** launches openWindow()
 * 
 * @param {*} event KML element, IRI of element
 */
function selectEBus(event) {
    selectedId =  event.srcElement.id;
    openWindow(selectedId);
}
/** selects the appropriate query. Generator is different from other functions
 * as generator requires two queries without making the header too large. 
 * 
 * @param {String} selectedId iri of the element
 */
function openWindow(selectedId){
    nameSet = [];
    if (selectedId.includes("Bus")){
        openWindowLineAndBus(selectedId, busInfo);
    }else if (selectedId.includes("ine")){
        openWindowLineAndBus(selectedId, branchInfo);
    }else if (selectedId.includes('VRB')){
		openWindowLineAndBus(selectedId, batteryInfo);
	}
    else{
        openWindowGen(selectedId);
    }
    
}

/** loads and refreshes the kml layer if it gets changed between base and test scenario
 * 
 * @param {String} iriofnetwork 
 * @param {String} kmlURL 
 */
function refreshLayer(iriofnetwork, kmlURL){
    if (kmlLayer){
        kmlLayer.setMap(null);
        }
    drawGenerator(iriofnetwork, kmlURL);
    console.log('Check that it should have refreshed. ')
}
/** calls ENVisualization to create kml file in ROOT/ONTOEN folder, and for google to read KML from there. 
 * This works ONLY because Claudius ROOT files are open to browser
 * @param data: electricalnetwork topnode iri
 */
function drawGenerator(data, anotherURL){
    var kmljson = {};
    var agenturl = prefix + '/JPS_POWSYS/ENVisualization/createKMLFile'; 
    var kmlurl = createUrlForAgent(scenario, agenturl, data);
    console.log(kmlurl);
    var request = $.ajax({
        url: kmlurl,
        type: 'GET',
        data: kmljson,
        contentType: 'application/json; charset=utf-8',
        success: function(){  
        },
        error: function(ts) {
            alert(ts.responseText);
        }   
    });

    request.done( function(data) {
    console.log ("success create request");
    if (anotherURL != null){
        kmlLayer = new google.maps.KmlLayer({
            // url: 'http://www.theworldavatar.com/OntoEN/testfinal.kml',//In other cases, will eventually be read and overwritten here. NO PROBLEM!
            url: anotherURL+ "?r="+(new Date()).getTime(), //this is completely necessary for cache-busting. 
            suppressInfoWindows: false,
            map: map
        });
    }else{
        kmlLayer = new google.maps.KmlLayer({
            url: 'http://theworldavatar.com/OntoEN/testfinal'+scenario+'.kml'+ "?r="+(new Date()).getTime(),
            suppressInfoWindows: false,
            map: map
    });
    }

        kmlLayer.addListener('click', function(kmlEvent) {
            setKMLMenu(kmlEvent)
        });             
        
    });

    request.fail(function(jqXHR, textStatus) {
    });
}

/** mystery of the missing kml layer. 
 * 
 */
var checkExistKML = setInterval(function() {
    if (kmlLayer != null && kmlLayer.status != "OK") {
        refreshLayer(json);
        clearInterval(checkExistKML);
    }
    }, 10000); // check every 10s
/*** calls ENVisualization to call POWSYS from markers
 * @param data: electricalnetwork topnode iri
 */
function drawMarkers(data){
    var agenturl=  prefix + '/JPS_POWSYS/ENVisualization/createMarkers'; 
    var kmlurl = createUrlForAgent(scenario, agenturl, data);
    console.log(kmlurl);
    console.log('number of markers: '+markers.length );
    var request = $.ajax({
        url: kmlurl,
        type: 'GET',
        async: true,
        contentType: 'application/json; charset=utf-8'
    });     
    
    request.done(function(data) {
        var obj0 = JSON.parse(data).result;
        var size=obj0.length; 
        
    //We currently know of a few cases:
    var x;
    // scan some duplicates
    dict = {"nuclear": 0, "gas": 0, "oil": 0, "solar":0};
    var markerdict = new Map();
    var latLng = new Map();
    for (x=0; x< size; x++){
        var obj = JSON.parse(obj0[x]);  
        var fueltype = obj.fueltype;
        var name = obj.name;
        if (fueltype== "NaturalGasGeneration"){
            var icon = {
                url: 'images/naturalgas.png',
                scaledSize : new google.maps.Size(40, 40),
            };
            dict["gas"] += 1;
        }else if (fueltype== "OilGeneration"){
            var icon = {
                url: 'images/oil.png',
                scaledSize : new google.maps.Size(40, 40),
            };
            dict["oil"] += 1;
        }else if (fueltype== "SolarGeneration"){
            var icon = {
                url: 'images/solar.png',
                scaledSize : new google.maps.Size(40, 40),
            };
            dict["solar"] += 1;
        }else{
            var icon = {
                url: 'images/radiation.png', 
                scaledSize : new google.maps.Size(40, 40),
            };
            dict["nuclear"] += 1;
        }
        // we need to do this because duplicates occur on the same location and we don't do clustering. 
        if (markerdict.has(obj.coors.lat)){
            var v = markerdict.get(obj.coors.lat);
            v.push(name);
            markerdict.set(obj.coors.lat, v);
        }else{
            markerdict.set(obj.coors.lat, [name]);
            
            latLng.set(obj.coors.lat, [obj.coors.lng, icon]);
            }
    }

    for (var [key, value] of latLng.entries()) {
        createMarker(key, value, markerdict);
          
    }
    if (batterylist){
        console.log(batterylist);
        searchBatteryCoord(batterylist);	
    }
	
    });
    }
/** creates a single marker and places it on the google map
 * 
 * @param {*} key the latitude of the marker
 * @param {Array} value [0] longitude of the marker [1] type of icon
 * @param {Map} markerdict dictionary of generators at that location
 */
function createMarker(key, value, markerdict){
    var marker = new google.maps.Marker({
        position: new google.maps.LatLng(key, value[0]),
        map: map,
        icon: value[1]
      });
    marker.addListener('click', function(){
        _content = setMarkerMenu(markerdict.get(key));
        infowindow.setContent(_content);
        infowindow.open(map, this);
    });
    markers.push(marker);
}
/** clears all markers on the page
 * 
 */
function clearMarkers() {
    if(!markers){
        return;
    }
    for(marker of markers){
        marker.setMap(null);
        marker=null;
    }
}
/** creates content of marker subMenu
 * 
 * @param {Array} jsonArray 
 * @return {string} content of HTML
 */
function setMarkerMenu(jsonArray)
	{
		var buttonsList = '<p>Please select the Entity you would like to modify</p>';
		console.log(jsonArray);
		for(var index in jsonArray)
		{
			var name = jsonArray[index];
			console.log(name);
			 buttonsList = buttonsList + '<div><label>'  + name.split('#')[1] + '</label>'+
				'<button onclick="selectEBus(event)" style= "cursor: pointer;" id="' + name + '"> > </span></div>'
		}

		buttonsList = '<div id="buttonContainer">'+ buttonsList +'</div><hr/><div id="inputsContainer"></div>';
		// set the content of the popup window.
		infoWindowHtml = '<div>' + buttonsList + '</div>';

		console.log(infoWindowHtml);
		return infoWindowHtml;

    }
    /** creates individual battery objects. Need to do this because of closure principle, otherwise all the markers will end up showing the same content. 
     * 
     * @param {JSON} srcdata 
     * @fires createBattery
     */
    function searchBatteryCoord(srcdata) {
        var size=srcdata.length; 
        var x;
        for (x=0; x< size; x++){
            createBattery(srcdata[x][0], srcdata[x][2], srcdata[x][1]);
           }
        console.log('Markers '+ markers);
        document.getElementById("loader").style.display = "none";
        document.getElementById("run-btn").disabled = false;
    }
    /** creates battery marker. 
     * 
     * @param {String} iri 
     * @param {Number} lat 
     * @param {Number} lng 
     */
    function createBattery(iri, lat, lng){
        var marker = new google.maps.Marker({
            position: new google.maps.LatLng(String(lat), String(lng)),
            map: map,
            title: String(iri),
            icon: "images/battery.png"
        });
        marker.addListener('click', function(){
            var jsonArray = marker.title.split('/');
            console.log(jsonArray);
            var spec = marker.title + '#' + jsonArray[jsonArray.length-1];
            _content = setMarkerMenu([spec]);
            infowindow.setContent(_content);
            infowindow.open(map, this);
        });
        markers.push(marker);
    }
/** opens popup window for generator (In the > button)
* 
* @param {String} id  iri of generator 
*/ 
function openWindowGen(id){
    //since geninfo too large for request header, I'll split it up
        selectedId =  id; //this needs to be saved on a local version, and not towards here. 
        var inputsHTML = '';
        var kmlurl = createUrlForSparqlQuery(scenario, selectedId.split('#')[0], genInfo);
        var kmlurl2 = createUrlForSparqlQuery(scenario, selectedId.split('#')[0], genInfo2);
        $.when(
            $.ajax({
            url: kmlurl,
            type: 'GET',
            contentType: 'application/json; charset=utf-8',
            success: function(data){ 
                var obj0 = JSON.parse(data);
                obj1 = obj0['results']['bindings'][0];
            },
            error: function(ts) {
                alert(ts.responseText);
            }   
            }),
        
            $.ajax({
            url: kmlurl2,
            type: 'GET',
            contentType: 'application/json; charset=utf-8',
            success: function(data){   
                var obj0 = JSON.parse(data);
                obj2 = obj0['results']['bindings'][0];
                console.log(obj2);
            },
            error: function(ts) {
                alert(ts.responseText);
            }   
        })).then( function(){
            var obj0 = Object.assign(obj1, obj2);
            console.log(obj0,obj1, obj2)
            var result = Object.keys(obj0).map(function(key) {return [key, obj0[key]];});
            console.log(selectedId);
            var owlName = selectedId.split('#')[1].split('.')[0];
            for(var item in result)
            {
                var pair = result[item];
                if (pair[0] == "entity"){}
                else if(!pair[1]['value'].includes('.owl')) //this is for values only. 
                {
                    var inputLine = '<tr><td><label>' + pair[0]+"_" +owlName +'</label></td><td><input class="input_class" data-dataType="' + pair[1]['datatype'] 
                    + '" value="' + pair[1]['value'] + '" style="float: right;"></td><td><input class="input_class" value="p.u." style="float: right;" disabled="disabled"></td></tr>';
                    inputsHTML = inputsHTML + inputLine;
                    pair[1]['name'] = pair[0]+"_" +owlName;
                    nameSet.push(pair);
                }else {
                    //for units, just place below the box. 
                    //remove the last 
                    inputsHTML = inputsHTML.slice(0, -101)
                    //add in the units 
                    var inputLine = '</td><td><input class="input_class" data-dataType="' + pair[1]['datatype'] + '" value="' + pair[1]['value'].split('#')[1] + '" style="float: right;" disabled="disabled"> </td></tr>';
                    inputsHTML = inputsHTML + inputLine;
                }
            }
    
            console.log(inputsHTML);
            var div = document.getElementById('inputsContainer');
            div.innerHTML = '<table data-type="kml" data-url='+ selectedId +' id="inputsTable">' + inputsHTML + '</table><br/><button onclick="SubmitTable(this)">OPF</button>'+
            '<img id="myProgressBar" style="width:100px;height:100px;display:none" src="https://media.giphy.com/media/3oEjI6SIIHBdRxXI40/giphy.gif"/><br/>'
    
    
        }, function (jqXHR, textStatus, errorThrown){
            alert(textStatus);
            console.log(errorThrown);
        }
        );
    }
       
/** construct update query strings for all modification
 * @param uri iri of resource
 * @param modifications array containing name (uri + # + name of resource), numericalvalue/uri string (attrObj.p), value (for insert statement)
 * @returns {[String]} array of changes to be made to OWL file
 */

function constructUpdate(uri, modifications) {
    let spoStrs = [];
    const values = Object.values(modifications);
    for (const item of values){
        spoStrs = spoStrs.concat(constructSingleUpdate(uri, item));
    }
    console.log(spoStrs);
    return [uri, spoStrs];
};

/** construct a single update query
 * @param uri iri of object
 * @param attrObj dictionary of name, datatype, value and numerical/string
 * @returns {[string, string]} [stringofDelete,stringofInsert]
 */
function constructSingleUpdate(uri, attrObj) {
    var spoDel = "DELETE WHERE {<" + uri + "#" + attrObj.name + "> <" + attrObj.p + "> " + "?o.}";

    var spoIns = "INSERT DATA {<" + uri + "#" + attrObj.name + "> <" + attrObj.p + "> " +attrObj.value +".}";
    return [spoDel, spoIns];
};
/** Old method still in use for submitting updates. 
 * 
 * @param {*} e 
 */
/** runs when OPF button is clicked
 * @param {Element} e 
 */
function SubmitTable(e) {

    opt = e.innerHTML;
    var table = document.getElementById('inputsTable');
    var rows = table.firstElementChild.childNodes;
    var url = table.getAttribute('data-url');
    var type = table.getAttribute('data-type');
    console.log('type',type);

    var JSONArray  = {};

    var proceed = true;
    console.log(rows.length, 'Rows length');
    for(var i = 0; i < rows.length; i++)
    {
        var row = rows[i];
        var name = row.getElementsByTagName('label')[0].innerText;
        var value = row.getElementsByTagName('input')[0].value;
        
        
		value =parseFloat(value)
        if(name.includes('EBus-001')){ // This is a slack bus, the magnitude is always 1 and the angle is always 0
            //console.log("label forbidden= "+label);            
            if (name.includes('VoltageAngle')|| name.includes('Va_EBus')){
                if (value !== 0){
                    alert('The value of the voltage angle and Va for a slack bus should always be 0 degree')
                    proceed = false;
                }
            }
        }
        else{ // This is a load bus 
        //console.log("label forbidden= "+label);
            if(name.includes('VoltageMagnitude')|| name.includes('Vm_EBus')){
                if( value > 1.05 || value <= 0.95){
                    alert('The value of the voltage magnitude and Vm should be between 0.95 and 1.05 kV (in p.u format)')
                    proceed = false;
                }
            }           
        }
        
        
        
        
        

        
        var datatype = row.getElementsByTagName('input')[0].getAttribute('data-dataType');
        console.log('value',value,'name',name,'url',url);
        JSONArray[name] = {name: name,value:value, datatype: datatype }
    }



    if(proceed){
        var progress = document.getElementById('myProgressBar');
        progress.style.display = 'block';
        updateOwlFile(url,JSONArray,type, true);
    }


}
function updateOwlFile(filename,JSONArray,_type, flag) {

    console.log('number',Object.keys(JSONArray).length);
    console.log('JSONArray',Object.keys(JSONArray));
    console.log('filename=',filename);
    console.log('type=',_type);

    var allItemsArray = [];
    var indexCounter = 0;
    var temp = [];
    for(var item in JSONArray){
            if(((indexCounter!== 0) && (indexCounter % 10 === 0))||(indexCounter === parseInt(Object.keys(JSONArray).length - 1)))
            {
                if((indexCounter === parseInt(Object.keys(JSONArray).length - 1)))
                {
                    //allItemsArray.push(temp);
                    //temp = [];
                    allItemsArray.push(temp);
                    temp = [];
                    temp.push(item)
                }
                else
                {  
                    allItemsArray.push(temp);
                    temp = [];
                    temp.push(item)
                }


            }
            else
            {   
                temp.push(item)
            }
            indexCounter++;
        }


    var asyncLoop = function(o){
        var i=-1,
            length = o.length;
        var loop = function(){
            i++;
            if(i===length){
                o.callback(); 
                return;
            }
            o.functionToLoop(loop, i);
        };
        loop();//init
};


asyncLoop({
    length : Math.ceil(Object.keys(JSONArray).length / 10),
    functionToLoop : function(loop, i){


        var sampleUpdate = [];
        var uri = [];

        var Session = allItemsArray[i];
        console.log(length);
        console.log('Session',Session);

        for(var j = 0; j < Session.length; j++)
        {
            var item = Session[j];
            var obj = JSONArray[item];
            var targetIRI = obj.name;
            var dataType = obj.datatype;
            if(dataType === 'int')
            {
                dataType = 'integer'
            }
            var base = filename.split('#')[0] + '#';
            base = base.replace('/OntoEN','');
            base=base.replace('theworldavatar','jparksimulator'); //because in electrical it use jparksimulator instead of theworldavatar
            var value = obj.value;
            if(targetIRI)
            {

                if (targetIRI.includes("Costn1")){
                    targetIRI = targetIRI.replace("Costn1","Costn-1" );
                    
                    console.log(targetIRI);
                }else if (targetIRI.includes("Costn2")){
                    targetIRI = targetIRI.replace("Costn2","Costn-2" );
                    
                    console.log(typeof targetIRI);
                }
                var deleteUpdate = "DELETE WHERE {<" + base + targetIRI + "> <http://www.theworldavatar.com/ontology/ontocape/upper_level/system.owl#numericalValue> " + "?o.}";
                var insertUpdate = "INSERT DATA {<" + base + targetIRI + "> <http://www.theworldavatar.com/ontology/ontocape/upper_level/system.owl#numericalValue> " +value +".}";
                    
                    

                console.log('deleteUpdate',deleteUpdate);
                console.log('insertUpdate',insertUpdate);

                sampleUpdate.push(deleteUpdate);
                sampleUpdate.push(insertUpdate);
                
                uri.push(filename);
                uri.push(filename);
                
            }
        }
        var myUrl = createUrlForSparqlUpdate(scenario,base.split('#')[0], sampleUpdate.join(';'));
        var request = $.ajax({
            url: myUrl,
            type: 'GET',
            contentType: 'application/json; charset=utf-8'
        });
        console.log(myUrl);
        request.done(function(data) {
            console.log('data received', data);
            loop();


        });

        request.fail(function(jqXHR, textStatus) {
            // your failure code here
        });


    },
    callback : function(){

        //var path = "C:@TOMCAT@webapps@ROOT@OntoEN@startSimulation.bat>" + filename.split('.com/')[1].split('.owl')[0] + '>' + opt;
        if (flag == false){
            openWindow(filename);
            return;
        }
        document.getElementById("loader").style.display = "block";
        var agenturl = prefix + '/JPS_POWSYS/ENAgent/startsimulation'+opt;
        data = { "electricalnetwork":iriofnetwork}
        url = createUrlForAgent(scenario, agenturl, data);
        console.log(url);
        var delayInMilliseconds = 10000; //1 second

            setTimeout(function() {
                console.log('timeout');
            }, delayInMilliseconds);
        var request = $.ajax({
            url: url,
            type: 'GET',
            contentType: 'application/json; charset=utf-8'
        });

        request.done(function(data) {
            //check for alert
            response = JSON.parse(data);
            if (response.status != "converged"){
                alert("This simulation has not converged. Use different values.");
                var arr = Object.keys(JSONArray);
                var newJSONArr = {}
                arr.forEach(function(inde, i){
                    newJSONArr[inde] = nameSet[i][1];
                });
                nameSet = [];
                console.log(newJSONArr);
                updateOwlFile(filename,newJSONArr,_type, false);
            
                document.getElementById("loader").style.display = "none";  
            }
            else {
                json = { "electricalnetwork":iriofnetwork ,"flag": scenario };
                displayCO2(json);
                var delayInMilliseconds = 10000; //1 second
                setTimeout(function() {
                    console.log('timeout');
                }, delayInMilliseconds);
                console.log('DONE SIMULATION')
                openWindow(filename);
            }
        });


    }
});

}
/** runs when OPF button is clicked
 * @bug There's a problem with the query. Can't solve it right now, but it means that the numerals submitted creates an error in the OWL file. 
 * 
 * @param {Element} e 
 */
function SubmitTable2(e) {

    opt = e.innerHTML;
    var table = document.getElementById('inputsTable');
    var rows = table.firstElementChild.childNodes;
    var url = table.getAttribute('data-url').split('#')[0];
    var JSONArray  = {};
    var proceed = true;
    console.log(rows.length, 'Rows length');
    for(var i = 0; i < rows.length; i++)
    {
        var row = rows[i];
        var name = row.getElementsByTagName('label')[0].innerText;
        var value = row.getElementsByTagName('input')[0].value;
        if(name.includes('EBus-001')){ // This is a slack bus, the magnitude is always 1 and the angle is always 0
            //console.log("label forbidden= "+label);
            if(name.includes('VoltageMagnitude')|| name.includes('Vm_EBus')) {
                if (value !== 1){
                    alert('The value of the voltage magnitude and Vm for a slack bus should always be 1 kV (in p.u format)')
                    proceed = false;
                }
            }
            
            if (name.includes('VoltageAngle')|| name.includes('Va_EBus')){
                if (value !== 0){
                    alert('The value of the voltage angle and Va for a slack bus should always be 0 degree')
                    proceed = false;
                }
            }
        }
        else{ // This is a load bus 
        //console.log("label forbidden= "+label);
            if(name.includes('VoltageMagnitude')|| name.includes('Vm_EBus')){
                if( value > 1.05 || value <= 0.95){
                    alert('The value of the voltage magnitude and Vm should be between 0.95 and 1.05 kV (in p.u format)')
                    proceed = false;
                }
            }           
        }

        var datatype = row.getElementsByTagName('input')[0].getAttribute('data-dataType');
        console.log('value',value,'name',name,'url',url);
        JSONArray[name] = {name: name,value:value, datatype: datatype, p:"http://www.theworldavatar.com/ontology/ontocape/upper_level/system.owl#numericalValue"}
    }if(proceed){
        var progress = document.getElementById('myProgressBar');
        progress.style.display = 'block';
        //need to break up the header because of limit on size. Therefore, 20 in the first go, others in the second. 
        var queryLine = constructUpdate(url, JSONArray);
        firstQuery = queryLine[1].slice(0, 20);
        outputUpdate([queryLine[0], firstQuery]);
        setTimeout(function() {
            console.log('timeout');
        }, 10000);
        secondQuery = queryLine[1].slice(20);
        outputUpdate([queryLine[0], secondQuery]);
        setTimeout(function() {
            console.log('timeout');
        }, 10000);

    }
}

/** Send output update queries
 * @param uris        array of uris to change, corresponding to updateQs
 * @param updateQs    array of SPARQL update strings
 * @param successCB   callback when success
 * @param errorCB      callback when err
 */
function  outputUpdate(input) { 

    let uris = input[0]
    let updateQs = input[1]
    console.log(uris)
    console.log(updateQs)
    var myUrl = createUrlForSparqlUpdate(scenario,uris, updateQs.join(';'));
    console.log(myUrl);
    $.ajax({
        url: myUrl,
        method: "GET",
        contentType: "application/json; charset=utf-8",
        success: function (data) {//SUCESS updating
            //Update display
            console.log("Success");
        },
        error: function (err) {
            console.log("can not update to server");
        }
    });
}
/** runs OPF simulation. Launched from Submit table after two successful updates
 * 
 * @param {String} filename iri of object to be opened in new infowindow
 */
function runSimulation(filename){
    
    document.getElementById("loader").style.display = "block";
    var agenturl = prefix + '/JPS_POWSYS/ENAgent/startsimulationOPF';
    data = { "electricalnetwork":iriofnetwork}
    url = createUrlForAgent(scenario, agenturl, data);
    console.log(url);
    var request = $.ajax({
        url: url,
        type: 'GET',
        contentType: 'application/json; charset=utf-8'
    });

    request.done(function() {
        json = { "electricalnetwork":iriofnetwork ,"flag": scenario };
        displayCO2(json);
        console.log('DONE SIMULATION'); 

        openWindow(filename);
        document.getElementById("loader").style.display = "none";
    });

}
/** opens infowindow for lines and ebuses. 
 * 
 * @param {String} id iri of object
 * @param {String} type sparql query of respective object
 * @param {Function} callback to display content as given by innerHTML
 */
function openWindowLineAndBus(id, type, callback){ //gen has its own openWindow cos it's too large. 
    var kmlurl = createUrlForSparqlQuery(scenario, id.split('#')[0], type);
    console.log(kmlurl);
    var inputsHTML = '';
    var request = $.ajax({
        url: kmlurl,
        type: 'GET',
        contentType: 'application/json; charset=utf-8',
        success: function(){  
        },
        error: function(ts) {
            alert(ts.responseText);
        }   
    });
    request.done( function(data) {
        var obj0 = JSON.parse(data);
        obj0 = obj0['results']['bindings'][0];
        console.log(obj0)


        var result = Object.keys(obj0).map(function(key) {return [key, obj0[key]];});
        var owlName = id.split('#')[1];
        for(var item in result)
        {
            var pair = result[item];
            if (pair[0] == "entity"){}
            else if(!pair[1]['value'].includes('.owl')) //this is for values only. 
            {
                var inputLine = '<tr><td><label>' + pair[0]+"_" +owlName +'</label></td><td><input class="input_class" data-dataType="' + pair[1]['datatype'] 
                + '" value="' + pair[1]['value'] + '" style="float: right;"></td><td><input class="input_class" value="p.u." style="float: right;" disabled="disabled"></td></tr>';
                inputsHTML = inputsHTML + inputLine;
                pair[1]['name'] = pair[0]+"_" +owlName;
                nameSet.push(pair);
            }else {
                //for units, just place below the box. 
                //remove the last 
                inputsHTML = inputsHTML.slice(0, -101)
                //add in the units 
                var inputLine = '</td><td><input class="input_class" data-dataType="' + pair[1]['datatype'] + '" value="' + pair[1]['value'].split('#')[1] + '" style="float: right;" disabled="disabled"> </td></tr>';
                inputsHTML = inputsHTML + inputLine;
            }
        }

        console.log(inputsHTML);
        if (id.includes("Bus")){
            var div = document.getElementById('inputsContainer');
            div.innerHTML = '<table data-type="kml" data-url='+ selectedId +' id="inputsTable">' + inputsHTML + '</table><br/><button onclick="SubmitTable(this)">OPF</button>'+
            '<img id="myProgressBar" style="width:100px;height:100px;display:none" src="https://media.giphy.com/media/3oEjI6SIIHBdRxXI40/giphy.gif"/><br/>'
            }
            
            else if (callback == null){
                innerHTML = '<table data-type="line" data-url='+ selectedId +' id="inputsTable">' + inputsHTML + '</table><br/><button onclick="SubmitTable(this)">OPF</button>'+
                        '<img id="myProgressBar" style="width:100px;height:100px;display:none" src="https://media.giphy.com/media/3oEjI6SIIHBdRxXI40/giphy.gif"/><br/>';
                infowindow.setContent(innerHTML);
            }
            
            else{
                const newPromise = new Promise((resolve, reject) => {
                    resolve('Success');
            });
                newPromise.then((successMessage) => {
                    innerHTML = '<table data-type="line" data-url='+ selectedId +' id="inputsTable">' + inputsHTML + '</table><br/><button onclick="SubmitTable(this)">OPF</button>'+
                        '<img id="myProgressBar" style="width:100px;height:100px;display:none" src="https://media.giphy.com/media/3oEjI6SIIHBdRxXI40/giphy.gif"/><br/>';
                    console.log(innerHTML);
                    callback(innerHTML);
                });
            }

    });

}
/** constructs and calls upon openWindowLineAndBus for lines
 * 
 * @param {String} id iri of line
 * @param {function} callback displays content of infowindow as set in drawLines in PopupMap
 */
function constructLineMenu(id,callback){
    selectedId = id.split('/')[1];
    console.log(selectedId)
    var promise1 = new Promise(function (resolve, reject){
        resolve(openWindowLineAndBus('http://www.jparksimulator.com/kb/sgp/jurongisland/jurongislandpowernetwork/' + selectedId +'#'+selectedId.split('.')[0], branchInfo, callback));
    }); 
    promise1.catch(alert);
}
