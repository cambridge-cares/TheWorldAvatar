package uk.ac.cam.cares.jps.powsys.electricalnetwork.test;

import java.io.IOException;
import java.net.URISyntaxException;
import java.util.ArrayList;

import junit.framework.TestCase;
import uk.ac.cam.cares.jps.powsys.electricalnetwork.ENAgent;

public class TestEN extends TestCase {
	
	public void testloadinfogathering () throws IOException {
		String iriofnetwork="http://www.jparksimulator.com/kb/sgp/jurongisland/jurongislandpowernetwork/JurongIslandPowerNetwork.owl#JurongIsland_PowerNetwork";
		String genInfo= "PREFIX j1:<http://www.theworldavatar.com/ontology/ontopowsys/PowSysRealization.owl#> " 
				+ "PREFIX j2:<http://www.theworldavatar.com/ontology/ontocape/upper_level/system.owl#> "
				+ "PREFIX j3:<http://www.theworldavatar.com/ontology/ontopowsys/model/PowerSystemModel.owl#> "
				+ "PREFIX j4:<http://www.theworldavatar.com/ontology/meta_model/topology/topology.owl#> "
				+ "PREFIX j5:<http://www.theworldavatar.com/ontology/ontocape/model/mathematical_model.owl#> "
				+ "PREFIX j6:<http://www.theworldavatar.com/ontology/ontocape/chemical_process_system/CPS_behavior/behavior.owl#> "
				+ "PREFIX j7:<http://www.theworldavatar.com/ontology/ontocape/supporting_concepts/space_and_time/space_and_time_extended.owl#> "
				+ "PREFIX j8:<http://www.theworldavatar.com/ontology/ontocape/material/phase_system/phase_system.owl#> "
				+ "SELECT ?entity ?BusNumbervalue ?activepowervalue ?reactivepowervalue ?Qmaxvalue ?Qminvalue ?Vgvalue ?mBasevalue ?Statusvalue ?Pmaxvalue ?Pminvalue ?Pc1value ?Pc2value ?Qc1minvalue ?Qc2minvalue ?Qc1maxvalue ?Qc2maxvalue ?Rampagcvalue ?Ramp10value ?Ramp30value ?Rampqvalue ?apfvalue "
				
				+ "WHERE {?entity  a  j1:PowerGenerator  ." 
				+ "?entity   j2:isModeledBy ?model ."
				
				+ "?model   j5:hasModelVariable ?num ."
				+ "?num  a  j3:BusNumber  ."
				+ "?num  j2:hasValue ?vnum ."
				+ "?vnum   j2:numericalValue ?BusNumbervalue ."  //number
				
				+ "?model   j5:hasModelVariable ?Pg ."
				+ "?Pg  a  j3:Pg  ."
				+ "?Pg  j2:hasValue ?vpg ."
				+ "?vpg   j2:numericalValue ?activepowervalue ."  //pg
				
				+ "?model   j5:hasModelVariable ?Qg ."
				+ "?Qg  a  j3:Qg  ."
				+ "?Qg  j2:hasValue ?vqg ."
				+ "?vqg   j2:numericalValue ?reactivepowervalue ." //qg
				
				+ "?model   j5:hasModelVariable ?qmax ."
				+ "?qmax  a  j3:QMax  ."
				+ "?qmax  j2:hasValue ?vqmax ."
				+ "?vqmax   j2:numericalValue ?Qmaxvalue ." //qmax
				
				+ "?model   j5:hasModelVariable ?qmin ."
				+ "?qmin  a  j3:QMin  ."
				+ "?qmin  j2:hasValue ?vqmin ."
				+ "?vqmin   j2:numericalValue ?Qminvalue ." //qmin
				
				+ "?model   j5:hasModelVariable ?Vg ."
				+ "?Vg  a  j3:Vg  ."
				+ "?Vg  j2:hasValue ?vVg ."
				+ "?vVg   j2:numericalValue ?Vgvalue ." //vg
				
				+ "?model   j5:hasModelVariable ?mbase ."
				+ "?mbase  a  j3:mBase  ."
				+ "?mbase  j2:hasValue ?vmbase ."
				+ "?vmbase   j2:numericalValue ?mBasevalue ." //mbase
				
				+ "?model   j5:hasModelVariable ?stat ."
				+ "?stat  a  j3:Status ."
				+ "?stat  j2:hasValue ?vstat ."
				+ "?vstat   j2:numericalValue ?Statusvalue ." //status

				+ "?model   j5:hasModelVariable ?pmax ."
				+ "?pmax  a  j3:PMax  ."
				+ "?pmax  j2:hasValue ?vpmax ."
				+ "?vpmax   j2:numericalValue ?Pmaxvalue ." //pmax
				
				+ "?model   j5:hasModelVariable ?pmin ."
				+ "?pmin  a  j3:PMin  ."
				+ "?pmin  j2:hasValue ?vpmin ."
				+ "?vpmin   j2:numericalValue ?Pminvalue ." //pmin
				
				+ "?model   j5:hasModelVariable ?pc1 ."
				+ "?pc1  a  j3:Pc1  ."
				+ "?pc1  j2:hasValue ?vpc1 ."
				+ "?vpc1   j2:numericalValue ?Pc1value ." //pc1
				
				+ "?model   j5:hasModelVariable ?pc2 ."
				+ "?pc2  a  j3:Pc2  ."
				+ "?pc2  j2:hasValue ?vpc2 ."
				+ "?vpc2   j2:numericalValue ?Pc2value ." //pc2
				
				+ "?model   j5:hasModelVariable ?Qc1max ."
				+ "?Qc1max  a  j3:QC1Max  ."
				+ "?Qc1max  j2:hasValue ?vQc1max ."
				+ "?vQc1max   j2:numericalValue ?Qc1maxvalue ." //qc1max
				
				+ "?model   j5:hasModelVariable ?qc1min ."
				+ "?qc1min  a  j3:QC1Min  ."
				+ "?qc1min  j2:hasValue ?vqc1min ."
				+ "?vqc1min   j2:numericalValue ?Qc1minvalue ." //qc1min
				
				+ "?model   j5:hasModelVariable ?Qc2max ."
				+ "?Qc2max  a  j3:QC2Max  ."
				+ "?Qc2max  j2:hasValue ?vQc2max ."
				+ "?vQc2max   j2:numericalValue ?Qc2maxvalue ." //qc2max
				
				+ "?model   j5:hasModelVariable ?qc2min ."
				+ "?qc2min  a  j3:QC2Min  ."
				+ "?qc2min  j2:hasValue ?vqc2min ."
				+ "?vqc2min   j2:numericalValue ?Qc2minvalue ." //qc2min
				
				+ "?model   j5:hasModelVariable ?ramp10 ."
				+ "?ramp10  a  j3:Ramp10  ."
				+ "?ramp10  j2:hasValue ?vramp10 ."
				+ "?vramp10   j2:numericalValue ?Ramp10value ." //ramp10
				
				+ "?model   j5:hasModelVariable ?rampagc ."
				+ "?rampagc  a  j3:Rampagc  ."
				+ "?rampagc  j2:hasValue ?vrampagc ."
				+ "?vrampagc   j2:numericalValue ?Rampagcvalue ." //rampagc
				
				+ "?model   j5:hasModelVariable ?ramp30 ."
				+ "?ramp30  a  j3:Ramp30  ."
				+ "?ramp30  j2:hasValue ?vramp30 ."
				+ "?vramp30   j2:numericalValue ?Ramp30value ." //ramp30
				
				+ "?model   j5:hasModelVariable ?rampq ."
				+ "?rampq  a  j3:Rampq  ."
				+ "?rampq  j2:hasValue ?vrampq ."
				+ "?vrampq   j2:numericalValue ?Rampqvalue ." //rampq
				
				+ "?model   j5:hasModelVariable ?apf ."
				+ "?apf  a  j3:APF  ."
				+ "?apf  j2:hasValue ?vapf ."
				+ "?vapf   j2:numericalValue ?apfvalue ." //apf
				
				+ "}";
		
		ENAgent b= new ENAgent ();
		
		ArrayList<String[]>genlist=b.extractOWLinArray(iriofnetwork,genInfo,22,"generator");
		b.createNewTSV(genlist, "C:/JPS_DATA/workingdir/JPS_POWSYS/gen.txt","C:/JPS_DATA/workingdir/JPS_POWSYS/mappingforENGen.csv");
	}
	
	public void testreading() throws IOException {
		ENAgent b= new ENAgent ();
		b.readResult("C:/JPS_DATA/workingdir/JPS_POWSYS/gen.txt", 21);
	}
	
	public void testdoconversion() throws IOException, URISyntaxException {
		ENAgent b= new ENAgent ();
		b.doConversion("http://www.jparksimulator.com/kb/sgp/jurongisland/jurongislandpowernetwork/JurongIslandPowerNetwork.owl#JurongIsland_PowerNetwork");
	}

}
