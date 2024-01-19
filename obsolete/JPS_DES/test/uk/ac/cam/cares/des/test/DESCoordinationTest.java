package uk.ac.cam.cares.des.test;

import static org.junit.Assert.assertFalse;

import java.io.IOException;

import org.json.JSONObject;
import org.junit.Test;

import uk.ac.cam.cares.jps.base.discovery.AgentCaller;
import uk.ac.cam.cares.jps.des.DESCoordination;

public class DESCoordinationTest {
	
	/** test if validateInput method is working in WeatherIrradiationTest
	 * 
	 */
    @Test
	public void testInputValidatorDESCoordinationTest() {
		JSONObject jo = new JSONObject();
		assertFalse(new DESCoordination().validateInput(jo));		
	}
    
	 /**
	 * Periodic call to run the (Forecast+DESpython wrapper)
	 * Every four hours, so six calls in a day this would be called
	 * This test should not be run due to the limited number of calls alloted to a free account. 
	 * This test should not return a result
	 */
//    @Test
	public void testStartCoordinationDES() throws IOException  {
		JSONObject jo = new JSONObject();	
		AgentCaller.executeGetWithJsonParameter("JPS_DES/DESCoordination", jo.toString());
		
	}

}
