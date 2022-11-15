package uk.ac.cam.cares.jps.agent.ontochemplant.test;

import org.json.JSONArray;
import org.json.JSONException;
import org.json.JSONObject;
import org.junit.Test;
import org.mockito.Mockito;
import uk.ac.cam.cares.jps.agent.ontochemplant.OntoChemPlantAgentLauncher;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertTrue;
import static org.junit.Assert.fail;
import static org.mockito.Mockito.*;

import java.lang.reflect.InvocationTargetException;
import java.lang.reflect.Method;

import javax.ws.rs.BadRequestException;

public class OntoChemPlantAgentLauncherTest {
	

	@Test
	public void testProcessRequestParameters() throws NoSuchMethodException, Exception {
		OntoChemPlantAgentLauncher testLauncher = Mockito.spy(new OntoChemPlantAgentLauncher());
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
            assertEquals(RuntimeException.class, ((InvocationTargetException) e).getTargetException().getClass());
        }
				
		JSONObject testRequestParams = new JSONObject();
		JSONArray testIRI = new JSONArray();
		testIRI.put("http://example.com/UUID_1");
		testRequestParams.put("iris", testIRI);
		JSONObject result = new JSONObject();
		doReturn(result).when(testLauncher).processRequestParameters(testRequestParams);
		testLauncher.processRequestParameters(testRequestParams);
		verify(testLauncher).processRequestParameters(testRequestParams);
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
			assertEquals(RuntimeException.class, ((InvocationTargetException) e).getTargetException().getClass());
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
		testIRI.put("http://example.com/UUID_1");
		testRequestParams.put("iris", testIRI);
		boolean result = (boolean) validateInput.invoke(testLauncher, testRequestParams);
		assertTrue(result);

	}
}