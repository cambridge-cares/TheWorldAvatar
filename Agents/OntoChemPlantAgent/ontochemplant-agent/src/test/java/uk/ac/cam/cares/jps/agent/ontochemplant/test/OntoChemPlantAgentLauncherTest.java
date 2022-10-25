package uk.ac.cam.cares.jps.agent.ontochemplant.test;

import org.json.JSONArray;
import org.json.JSONException;
import org.json.JSONObject;
import org.junit.Test;
import uk.ac.cam.cares.jps.agent.ontochemplant.OntoChemPlantAgentLauncher;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertTrue;
import static org.junit.Assert.fail;

import java.lang.reflect.InvocationTargetException;
import java.lang.reflect.Method;

import javax.ws.rs.BadRequestException;

public class OntoChemPlantAgentLauncherTest {
	

	@Test
	public void testProcessRequestParameters() throws NoSuchMethodException, Exception {
		OntoChemPlantAgentLauncher testLauncher = new OntoChemPlantAgentLauncher();
		Method processRequestParameters = null;
		try {
			processRequestParameters = testLauncher.getClass().getDeclaredMethod("processRequestParameters", JSONObject.class);
		} catch (Exception e) {
			fail();
		}
		
		// Empty input
		JSONObject testEmptyRequestParams = new JSONObject();
		try {processRequestParameters.invoke(testLauncher, testEmptyRequestParams);
        } catch(Exception e) {
            assert e instanceof InvocationTargetException;
            assertEquals(BadRequestException.class, ((InvocationTargetException) e).getTargetException().getClass());
        }
				
		JSONObject testRequestParams = new JSONObject();
		JSONArray testIRI = new JSONArray();
		testIRI.put("http://www.theworldavatar.com:83/citieskg/namespace/jriEPSG24500/sparql/cityobject/UUID_bd07e1dd-7ffe-4776-8cf0-5409c007e437/");
		testRequestParams.put("iris", testIRI);

		// TODO: Test if result is returned with correct inputs, invoke method leads to NoClassDefFound
//		JSONObject actual= (JSONObject) processRequestParameters.invoke(testLauncher, testRequestParams);
//		assertNotNull(actual);
		
		
	}
	
	@Test
	public void testValidateInput() throws NoSuchMethodException, Exception {
		OntoChemPlantAgentLauncher testLauncher = new OntoChemPlantAgentLauncher();
		Method validateInput = null;
		try {
			validateInput = testLauncher.getClass().getDeclaredMethod("validateInput", JSONObject.class);
		} catch (BadRequestException e) {
			fail();
		}
		
		// Empty input
		JSONObject testRequestParams = new JSONObject();
		try {
			validateInput.invoke(testLauncher, testRequestParams);		
		} catch (Exception e) {
			assert e instanceof InvocationTargetException;
			assertEquals(BadRequestException.class, ((InvocationTargetException) e).getTargetException().getClass());
		}
		
		// Empty value in JSONobject
		JSONArray testIRI = new JSONArray();
		testRequestParams.put("iris", testIRI);
		try {
			validateInput.invoke(testLauncher, testRequestParams);		
		} catch (Exception e) {
			assert e instanceof InvocationTargetException;
			assertEquals(JSONException.class, ((InvocationTargetException) e).getTargetException().getClass());
		}
		
		// Correct input
		testIRI.put("http://www.theworldavatar.com:83/citieskg/namespace/jriEPSG24500/sparql/cityobject/UUID_bd07e1dd-7ffe-4776-8cf0-5409c007e437/");
		testRequestParams.put("iris", testIRI);
		boolean result = (boolean) validateInput.invoke(testLauncher, testRequestParams);
		assertTrue(result);

	}
}