package uk.ac.cam.cares.jps.ship.test;


import java.io.IOException;
import java.util.ArrayList;

import org.json.JSONObject;

import junit.framework.TestCase;
import uk.ac.cam.cares.jps.base.config.AgentLocator;
import uk.ac.cam.cares.jps.base.discovery.AgentCaller;
import uk.ac.cam.cares.jps.base.util.CommandHelper;


public class TestSpeedLoadMap extends TestCase {
	private static final String slmDir = "\\python\\ADMS-speed-load-map";
	private static final String slmPython = "\\env\\Scripts\\python.exe";
	private static final String slmScript = "ADMS-Map-SpeedTorque-NOxSoot.py";

	public void testcallspeedloadmapWrapper() throws IOException {
		
		
			JSONObject jo = new JSONObject();
//			jo.put("speed",0.1); //ori=11
//			jo.put("type","passenger");
			jo.put("speed",10.3); //ori=11
			jo.put("type","cargo");
			String result = AgentCaller.executeGetWithJsonParameter("JPS_SHIP/SLMAgent",jo.toString());
			System.out.println("result= "+result);
			
			JSONObject joresult = new JSONObject(result);
//			assertEquals(joresult.getJSONObject("mixture").getJSONObject("massflux").get("value"), 13.392369102033804); //mass flux
//			assertEquals(joresult.getJSONArray("pollutants").getJSONObject(4).get("value"), 0.006041259571151734); //no2
			
		
	}
	
	public void testdebugsurrogate() throws IOException {
		JSONObject in= new JSONObject();
		double valuecalc=30.0;
		if(valuecalc>2500) {
			valuecalc=2500;
		}
		String type="passenger";
		JSONObject speedob= new JSONObject();		
		speedob.put("value", valuecalc); //600-2500
		speedob.put("unit", "RPM");
		JSONObject torob= new JSONObject();
		torob.put("value", 250); //50-550 range
		torob.put("unit", "Nm");
		in.put("speed", speedob);
		in.put("torque", torob);
		

		String smlWorkingDir =  AgentLocator.getCurrentJpsAppDirectory(this) + slmDir;
		String pythonExec = smlWorkingDir + slmPython;

		ArrayList<String> args = new ArrayList<String>();
		args.add(pythonExec);
        args.add(slmScript);
		args.add(in.toString().replace("\"", "'"));

		CommandHelper.executeCommands(smlWorkingDir, args);

	}
}
