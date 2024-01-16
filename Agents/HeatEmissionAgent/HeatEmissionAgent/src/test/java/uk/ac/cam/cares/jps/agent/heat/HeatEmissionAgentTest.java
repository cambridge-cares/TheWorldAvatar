package uk.ac.cam.cares.jps.agent.heat;

import org.json.JSONArray;
import org.json.JSONObject;
import org.junit.Test;
import org.mockito.MockedStatic;
import org.mockito.Mockito;
import static org.junit.Assert.assertEquals;
import static org.junit.Assert.fail;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.ArgumentMatchers.anyString;
import static org.mockito.Mockito.doNothing;
import static org.mockito.Mockito.spy;
import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verify;

import java.lang.reflect.InvocationTargetException;
import java.lang.reflect.Method;

import javax.ws.rs.BadRequestException;

public class HeatEmissionAgentTest {

    // processRequestParameters(JSONObject requestParams)
    @Test
    public void testProcessRequestParameters() throws NoSuchMethodException, Exception {
        HeatEmissionAgent Query = new HeatEmissionAgent();
        Method processRequestParameters = Query.getClass().getDeclaredMethod("processRequestParameters",
                JSONObject.class);

        // Test empty request parameters
        JSONObject requestParams_1 = new JSONObject();
        try {
            processRequestParameters.invoke(Query, requestParams_1);
        } catch (Exception e) {
            assert e instanceof InvocationTargetException;
            assertEquals(((InvocationTargetException) e).getTargetException().getClass(),
                    BadRequestException.class);
        }

        // Test data
        String inputBounds = "{\"job\":{\"lower_bounds\":\"8464.256074442204#23588.08319044689#0\",\"upper_bounds\":\"17619.669922658715#30520.376177137474#105\"}}\r\n";
        JSONObject requestParams_2 = new JSONObject(inputBounds);
        JSONObject expected = new JSONObject();
        JSONObject row = new JSONObject();
        row.put("Coordinate", "13435.86861#26485.17517#60");
        row.put("Heat Emission", "100");
        JSONArray heatresult = new JSONArray()
                .put(new JSONObject().put("Coordinate", "13435.86861#26485.17517#60").put("Heat Emission", "100"));
        expected.put("result", heatresult);
        JSONObject actual = (JSONObject) processRequestParameters.invoke(Query, requestParams_2);
        assertEquals(expected, actual);
    }

    // Test validateInput(JSONObject requestParams)
    @Test
    public void testValidateInput() {
        HeatEmissionAgent Query = new HeatEmissionAgent();
        Method validateInput = null;
        try {
            validateInput = Query.getClass().getDeclaredMethod("validateInput", JSONObject.class);
        } catch (Exception e) {
            fail();
        }

        // Check failure with empty request parameters
        JSONObject requestInput1 = new JSONObject();
        try {
            validateInput.invoke(Query, requestInput1);
        } catch (Exception e) {
            assert e instanceof InvocationTargetException;
            assertEquals(((InvocationTargetException) e).getTargetException().getClass(), BadRequestException.class);
        }

        // Check failure with empty upper limits
        String inputBounds_2 = "{\"job\":{\"lower_bounds\":\"10#20#0\",\"upper_bounds\":\"\"}}\r\n";
        JSONObject requestInput2 = new JSONObject(inputBounds_2);
        try {
            validateInput.invoke(Query, requestInput2);
        } catch (Exception e) {
            assert e instanceof InvocationTargetException;
            assertEquals(((InvocationTargetException) e).getTargetException().getClass(), BadRequestException.class);
        }

        // Check failure with empty lower limits
        String inputBounds_3 = "{\"job\":{\"lower_bounds\":\"\",\"upper_bounds\":\"20#30#40\"}}\r\n";
        JSONObject requestInput3 = new JSONObject(inputBounds_3);
        try {
            validateInput.invoke(Query, requestInput3);
        } catch (Exception e) {
            assert e instanceof InvocationTargetException;
            assertEquals(((InvocationTargetException) e).getTargetException().getClass(), BadRequestException.class);
        }

        // Should pass now
        String inputBounds_4 = "{\"job\":{\"lower_bounds\":\"10#20#0\",\"upper_bounds\":\"20#30#40\"}}\r\n";
        JSONObject requestInput4 = new JSONObject(inputBounds_4);
        try {
            validateInput.invoke(Query, requestInput4);
        } catch (Exception e) {
            fail();
        }
    }

    @Test
    public void testAgent() {
        JSONObject requestInput = new JSONObject();
        requestInput.put("endpoint", "http://localhost:48889/blazegraph/namespace/jibusinessunits/sparql");
        // requestInput.put("endpoint",
        // "http://localhost:3838/blazegraph/namespace/jibusinessunits/sparql");
        requestInput.put("ontology", "ontochemplant");
        JSONObject result = new HeatEmissionAgent().processRequestParameters(requestInput);
        assertTrue(result.getString("success").length() > 0);

    }

    @Test
    public void testAgentMainland() {
        JSONObject requestInput = new JSONObject();
        requestInput.put("endpoint", "http://localhost:48889/blazegraph/namespace/sgbusinessunits/sparql");
        requestInput.put("ontology", "ontocompany");
        JSONObject result = new HeatEmissionAgent().processRequestParameters(requestInput);
        assertTrue(result.getString("success").equals("true"));

    }

}
