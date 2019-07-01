package uk.ac.cam.cares.jps.ship.test;

import java.util.List;

import org.json.JSONArray;
import org.json.JSONObject;

import junit.framework.TestCase;
import uk.ac.cam.cares.jps.base.config.AgentLocator;
import uk.ac.cam.cares.jps.base.discovery.AgentCaller;
import uk.ac.cam.cares.jps.base.query.JenaResultSetFormatter;
import uk.ac.cam.cares.jps.base.query.QueryBroker;
import uk.ac.cam.cares.jps.base.util.MatrixConverter;
import uk.ac.cam.cares.jps.ship.SoftSensor;

public class TestSoftSensor extends TestCase {
	
	public void testCallingSoftsensor() {
		JSONArray ja = new JSONArray();
		JSONObject location1 = new JSONObject();
		location1.put("x",833776.38);
		location1.put("y",816731.54);
		location1.put("z",4.5);
		ja.put(location1);
		JSONObject location2 = new JSONObject();
		location2.put("x",833776.38);
		location2.put("y",816731.54);
		location2.put("z",23.2);
		ja.put(location2);
		JSONObject location3 = new JSONObject();
		location3.put("x",124);
		location3.put("y",33.4);
		location3.put("z",23.2);
		ja.put(location3);
		
		JSONObject time = new JSONObject();
		time.put("from", "2019-06-24T08:00:00");
		time.put("to", "2019-06-24T12:00:00");
		JSONObject jo = new JSONObject();
		jo.put("timeinterval", time);
		jo.put("coordinates", ja);
		jo.put("agent", "http://www.theworldavatar.com/kb/agents/Service__ADMS.owl#Service");
		String result = AgentCaller.executeGetWithJsonParameter("JPS_SHIP/SoftSensor",jo.toString());
		System.out.println("result= "+result);
		System.out.println("simplified result= "+JenaResultSetFormatter.convertToSimplifiedList(result));
		int number=JenaResultSetFormatter.convertToSimplifiedList(result).getJSONArray("results").length();
		assertEquals(36, number); //2time x 3point x 6pollutant  
		
	}

	public void testcalculationclosest() {
		SoftSensor a= new SoftSensor();
		String csv = new QueryBroker().readFile(AgentLocator.getCurrentJpsAppDirectory(this) + "/workingdir/test.levels.gst");
		
		List<String[]> simulationResult = MatrixConverter.fromCsvToArray(csv);
		System.out.println("the closest number= "+a.findtheclosest(simulationResult,833776.38,816731.54,17.0));
		double realx=Double.valueOf(a.findtheclosest(simulationResult,833776.38,816731.54,17.0).get(0));
		double realy=Double.valueOf(a.findtheclosest(simulationResult,833776.38,816731.54,17.0).get(1));
		double realz=Double.valueOf(a.findtheclosest(simulationResult,833776.38,816731.54,17.0).get(2));
		assertEquals(833782.44, realx, 0.01);
		assertEquals(816748.75, realy, 0.01);
		assertEquals(20.0, realz, 0.01);
	}
	
	public void testcalculationconcentration() {
		SoftSensor a= new SoftSensor();
		String csv = new QueryBroker().readFile(AgentLocator.getCurrentJpsAppDirectory(this) + "/workingdir/test.levels.gst");
		List<String[]> simulationResult = MatrixConverter.fromCsvToArray(csv);
		double realx=Double.valueOf(a.findtheclosest(simulationResult,833776.38,816731.54,17.0).get(0));
		double realy=Double.valueOf(a.findtheclosest(simulationResult,833776.38,816731.54,17.0).get(1));
		double realz=Double.valueOf(a.findtheclosest(simulationResult,833776.38,816731.54,17.0).get(2));
		System.out.println("the concentration result= "+a.findtheconcentration(simulationResult,realx,realy,realz));
		assertEquals("CO2", a.findtheconcentration(simulationResult,realx,realy,realz).get(0).split("\\|")[2]);
		assertEquals(585168, Double.valueOf(a.findtheconcentration(simulationResult,realx,realy,realz).get(1)), 0.01);
		assertEquals("CO", a.findtheconcentration(simulationResult,realx,realy,realz).get(2).split("\\|")[2]);
		assertEquals(58.4218, Double.valueOf(a.findtheconcentration(simulationResult,realx,realy,realz).get(3)), 0.01);
		

	}
}
