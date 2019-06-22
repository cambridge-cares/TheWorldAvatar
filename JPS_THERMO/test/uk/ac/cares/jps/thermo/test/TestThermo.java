package uk.ac.cares.jps.thermo.test;
import org.json.JSONObject;
import org.junit.Test;

import junit.framework.TestCase;
import uk.ac.cam.cares.jps.base.discovery.AgentCaller;
import uk.ac.cam.cares.jps.base.query.QueryBroker;

public class TestThermo extends TestCase {
 
	
	public void testReadFile() {
		
		String content = new QueryBroker().readFile("http://www.theworldavatar.com/66d74432-d44a-354f-a07e-0e1a15c049f1/Cl2O6.owl");
		
		System.out.println(content);
	}
	
	
	@Test
	public void testThermoCalculationAgent() { 
		
		JSONObject json = new JSONObject();
		
		json.put("gaussian", "http://www.theworldavatar.com/66d74432-d44a-354f-a07e-0e1a15c049f1/Cl2O6.owl");
		
		// GET ...twa.com/JPS_THERMO/thermocalcualtion?query={"gaussian":".......owl"}
		
		String result = AgentCaller.executeGetWithJsonParameter("JPS_THERMO/calculation", json.toString());
		
		System.out.println("result = " + result);
		
		
	}
	
}