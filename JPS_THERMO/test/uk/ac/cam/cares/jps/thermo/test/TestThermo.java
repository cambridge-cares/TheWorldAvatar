package uk.ac.cam.cares.jps.thermo.test;
import org.json.JSONObject;
import org.junit.Test;

import junit.framework.TestCase;
import uk.ac.cam.cares.jps.base.discovery.AgentCaller;
import uk.ac.cam.cares.jps.base.query.QueryBroker;

/**
 * 
 * @author Nk510 (caresssd@hermes.cam.ac.uk)
 * Dear Kevin,
 * must use the complete iri instance and the file itself must be .  
 * 
 *
 */
public class TestThermo extends TestCase {
 
	
	public void xxxtestReadFile() {
		
		String content = new QueryBroker().readFile("http://www.theworldavatar.com/66d74432-d44a-354f-a07e-0e1a15c049f1/Cl2O6.owl");
		
		System.out.println(content);
	}
	
	
	@Test
	public void testThermoCalculationAgent() { 
		
		JSONObject json = new JSONObject();
		
		//json.put("gaussian", "http://www.theworldavatar.com/66d74432-d44a-354f-a07e-0e1a15c049f1/Cl2O6.owl");
		json.put("gaussian", "http://www.theworldavatar.com/kb/ontocompchem/f072d20b-9a95-3abc-85af-b3e5e268c52a/f072d20b-9a95-3abc-85af-b3e5e268c52a.owl#f072d20b-9a95-3abc-85af-b3e5e268c52a");
		
		// GET ...twa.com/JPS_THERMO/thermocalcualtion?query={"gaussian":".......owl"}
		
		String result = AgentCaller.executeGetWithJsonParameter("/JPS_THERMO/calculation", json.toString());
		
		System.out.println("result = " + result);
		
		
	}
	
}