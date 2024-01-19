package uk.ac.cam.cares.des.test;

import static org.junit.Assert.assertTrue;

import java.io.IOException;
import java.util.Date;

import org.json.JSONObject;
import org.junit.Before;
import org.junit.Test;

import uk.ac.cam.cares.jps.base.util.InputValidator;
import uk.ac.cam.cares.jps.des.ForecastAgent;
import uk.ac.cam.cares.jps.des.n.DESAgentNew;

public class ForecastAgentTest {
	private String irioftempF=null;
	private String iriofirrF=null;
	@Before
    public void setUp() {
    	irioftempF="http://www.theworldavatar.com/kb/sgp/singapore/SGTemperatureForecast-001.owl#SGTemperatureForecast-001";
		iriofirrF="http://www.theworldavatar.com/kb/sgp/singapore/SGSolarIrradiationForecast-001.owl#SGSolarIrradiationForecast-001";
		
	}
	/** test if validateInput method is working in ForecastAgent
	 * 
	 */
    @Test
	public void testInputValidatorForecast() {
		JSONObject jo = new JSONObject().put("temperatureforecast", irioftempF);
		jo.put("irradiationforecast", iriofirrF);
		assertTrue(new ForecastAgent().validateInput(jo));		
	}

	/** This tests if the forecast was ran correctly when called directly.  
	 * This test should not be run due to the limited number of calls alloted to a free account. 
	 * @throws IOException 
	 * 
	 */
//	    @Test
	public void testWeatherForecast() throws IOException {

		long timeLast = new Date().getTime();
    	new ForecastAgent().nextForecastDaySolcast(irioftempF,iriofirrF);
    	String fileStr = DESAgentNew.tempIRItoFile(iriofirrF);
    	assertTrue(InputValidator.checkIfFileGotUpdated(fileStr,  timeLast));
    	fileStr = DESAgentNew.tempIRItoFile(irioftempF);
    	assertTrue(InputValidator.checkIfFileGotUpdated(fileStr,  timeLast));
    	timeLast = new Date().getTime();
    	new ForecastAgent().nextForecastDayTemperature(irioftempF);
    	assertTrue(InputValidator.checkIfFileGotUpdated(fileStr,  timeLast));		
	}
		

}
