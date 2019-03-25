package uk.ac.cam.cares.jps.powsys.nuclear.test;

import java.io.IOException;
import java.net.URISyntaxException;

import org.json.JSONObject;

import junit.framework.TestCase;
import uk.ac.cam.cares.jps.base.discovery.AgentCaller;
import uk.ac.cam.cares.jps.powsys.nuclear.NuclearAgent;

public class TestNuclear extends TestCase{

	
	public void testPrepareCSVLoad() throws IOException {
		String irinetwork="http://www.jparksimulator.com/kb/sgp/jurongisland/jurongislandpowernetwork/JurongIslandPowerNetwork.owl#JurongIsland_PowerNetwork";
		NuclearAgent b=new NuclearAgent();
		
		String diroutput= b.prepareCSVLoad(irinetwork);
		assertEquals("C:/JPS_DATA/workingdir/JPS_POWSYS/inputloadpoints.csv", diroutput);
	}
	
	public void testPrepareCSVLots() throws IOException {
		String lotiri="http://www.theworldavatar.com/kb/sgp/jurongisland/JurongIslandLandlots.owl";
		NuclearAgent b=new NuclearAgent();
		
		String diroutput= b.prepareCSVLandlot(lotiri);
		assertEquals("C:/JPS_DATA/workingdir/JPS_POWSYS/inputlandlots.csv", diroutput);
		
	}
	
	public void testreadcsv() throws IOException {
		String oldcsv="D:/JPS/JParkSimulator-git/JPS_POWSYS/testres/Landlots.csv";
		NuclearAgent b=new NuclearAgent();
		b.readCSV(oldcsv,"s");
	}
	
	public void testrunGAMS() {
		NuclearAgent b=new NuclearAgent();
		b.runGAMS();
	}
	
	public void testCallAgent() throws IOException, URISyntaxException {
		
		JSONObject jo = new JSONObject();
		jo.put("landlot", "http://www.theworldavatar.com/kb/sgp/jurongisland/JurongIslandLandlots.owl");
		jo.put("electricalnetwork", "http://www.jparksimulator.com/kb/sgp/jurongisland/jurongislandpowernetwork/JurongIslandPowerNetwork.owl#JurongIsland_PowerNetwork");

		System.out.println ("jsonoverall= "+jo.toString());
		String resultAsString = AgentCaller.executeGetWithJsonParameter("JPS_POWSYS//NuclearAgent", jo.toString());
	
		
		JSONObject result = new JSONObject(resultAsString);

			//result should be the list of iri for the nuclear power plant in json
	}
}
