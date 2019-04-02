package uk.ac.cam.cares.jps.powsys.nuclear.test;

import java.io.IOException;
import java.net.URISyntaxException;

import org.json.JSONObject;

import junit.framework.TestCase;
import uk.ac.cam.cares.jps.base.discovery.AgentCaller;
import uk.ac.cam.cares.jps.powsys.nuclear.NuclearAgent;

public class TestNuclear extends TestCase {

	
	public void testPrepareCSVLoad() throws IOException {
		String irinetwork="http://www.jparksimulator.com/kb/sgp/jurongisland/jurongislandpowernetwork/JurongIslandPowerNetwork.owl#JurongIsland_PowerNetwork";
		NuclearAgent b=new NuclearAgent();
		String outputdir="C:/JPS_DATA/workingdir/JPS_POWSYS/inputloadpoints.csv";
		b.prepareCSVLoad(irinetwork,outputdir);
		
	}
	
	public void testPrepareCSVLots() throws IOException {
		String lotiri="http://www.theworldavatar.com/kb/sgp/jurongisland/JurongIslandLandlots.owl";
		NuclearAgent b=new NuclearAgent();
		String outputdir2="C:/JPS_DATA/workingdir/JPS_POWSYS/inputlandlots.csv";
		b.prepareCSVLandlot(lotiri,outputdir2);
		
		
	}
	
	
	public void testrunGAMS() throws IOException, InterruptedException {
		NuclearAgent b=new NuclearAgent();
		b.runGAMS();
	}
	
	public void testCallAgent() throws IOException, URISyntaxException {
		
		JSONObject jo = new JSONObject();
		jo.put("landlot", "http://www.theworldavatar.com/kb/sgp/jurongisland/JurongIslandLandlots.owl");
		jo.put("electricalnetwork", "http://www.jparksimulator.com/kb/sgp/jurongisland/jurongislandpowernetwork/JurongIslandPowerNetwork.owl#JurongIsland_PowerNetwork");

		System.out.println ("jsonoverall= "+jo.toString());
		String resultAsString = AgentCaller.executeGetWithJsonParameter("JPS_POWSYS/NuclearAgent/startsimulation", jo.toString());


		System.out.println("result overall= "+resultAsString);
		

			//result should be the list of iri for the nuclear power plant in json
	}
	
	public void testProcessResult() {
		
		
		String url = "http://www.theworldavatar.com/NuclearAgent/processresult";
		String json  = null;
		
		//test for web jps use this:
		//String result = AgentCaller.executeGetWithURLAndJSON(url, json);
		
		//test for local use this:
		String resultAsString = AgentCaller.executeGetWithJsonParameter("JPS_POWSYS/NuclearAgent/processresult", json);
		
	}
}
