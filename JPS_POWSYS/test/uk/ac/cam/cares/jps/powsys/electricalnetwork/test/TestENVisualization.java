package uk.ac.cam.cares.jps.powsys.electricalnetwork.test;

import java.io.File;
import java.io.IOException;
import java.util.ArrayList;
import java.util.List;

import org.apache.jena.ontology.OntModel;

import junit.framework.TestCase;
import uk.ac.cam.cares.jps.powsys.electricalnetwork.ENAgent;
import uk.ac.cam.cares.jps.powsys.envisualization.ENVisualization;
import uk.ac.cam.cares.jps.powsys.envisualization.ENVisualization.StaticobjectgenClass;
import uk.ac.cam.cares.jps.powsys.envisualization.MapPoint;

public class TestENVisualization extends TestCase {

	public void testcreateKML() {
		ENVisualization a=new ENVisualization();
		ENAgent b= new ENAgent ();
		OntModel model = b.readModelGreedy("http://www.jparksimulator.com/kb/sgp/jurongisland/jurongislandpowernetwork/JurongIslandPowerNetwork.owl#JurongIsland_PowerNetwork");
		List<String[]> generators=a.queryElementCoordinate(model, "PowerGenerator");
		
		
		
		//-----------------------------
		ArrayList<ENVisualization.StaticobjectgenClass> gensmerged = new ArrayList<ENVisualization.StaticobjectgenClass>();
		ArrayList<String> coorddata = new ArrayList<String>();
		for (int e = 0; e < generators.size(); e++) {
			StaticobjectgenClass gh = a.new StaticobjectgenClass();
			gh.setnamegen("/" + generators.get(e)[0].split("#")[1] + ".owl");
			gh.setx(generators.get(e)[1]);
			gh.sety(generators.get(e)[2]);
			System.out.println("/" + generators.get(e)[0].split("#")[1] + ".owl");

			if (coorddata.contains(gh.getx()) && coorddata.contains(gh.gety())) {
				int index = coorddata.indexOf(gh.getx()) / 2;
				gensmerged.get(index).setnamegen(gensmerged.get(index).getnamegen() + gh.getnamegen());
			} else {
				gensmerged.add(gh);
				coorddata.add(generators.get(e)[1]);
				coorddata.add(generators.get(e)[2]);
			}

		}
		
		
		for(int g=0;g<gensmerged.size();g++) {
			MapPoint c= new MapPoint(Double.valueOf(gensmerged.get(g).gety()),Double.valueOf(gensmerged.get(g).getx()),0.0,"/"+gensmerged.get(g).getnamegen());
			a.addMark(c,"generator");
		}
		
		//--------------------------------
		
		List<String[]> bus=a.queryElementCoordinate(model, "BusNode");
		int size2=bus.size();
		for(int g=0;g<size2;g++) {
		MapPoint c= new MapPoint(Double.valueOf(bus.get(g)[2]),Double.valueOf(bus.get(g)[1]),0.0,"/"+bus.get(g)[0].split("#")[1]+".owl");
		a.addMark(c,"bus");
		}
		
		a.writeFile(new File("C:/JPS_DATA/workingdir/JPS_POWSYS/scenario of Powsys not needed/test2.kml"));
	}
	
	public void testcreateLineJS() throws IOException {
		ENVisualization a=new ENVisualization();
		ENAgent b= new ENAgent ();
		OntModel model = b.readModelGreedy("http://www.jparksimulator.com/kb/sgp/jurongisland/jurongislandpowernetwork/JurongIslandPowerNetwork.owl#JurongIsland_PowerNetwork");
		a.createLineJS(model,"C:/JPS_DATA/workingdir/JPS_POWSYS/scenario of Powsys not needed/line.js");

	}
	
	
}
