package uk.ac.cam.cares.jps.agent.AERMODAgent;

import org.json.JSONArray;
import org.json.JSONObject;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Test;

/**
 * Example tests for the AdditionAgent.
 *
 * @author Michael Hillman {@literal <mdhillman@cmclinnovations.com>}
 */
public class AERMODAgentTest {

    /**
     * Tests the AERMOD agent by creating a mock request.
     */
    @Test
    public void testAgent() {
        JSONObject mockRequest = new JSONObject();
        JSONObject params = new JSONObject();
        params.put("location","Jurong Island");
        params.put("latitude","1.25377");
        params.put("longitude","103.7007921");

        mockRequest.put("job", params);

        AERMODAgent agent = new AERMODAgent();
        JSONObject result = agent.processRequestParameters(mockRequest);
        Assertions.assertNotNull(result, "Expected a JSON Object to be returned!");
        Assertions.assertTrue(result.has("status"), "Expected to find a field named 'c'.");
        
        try {
            int resultingNumber = result.getInt("status");
            Assertions.assertEquals(0, resultingNumber, "Expected value of field 'c' to be '5'.");
        } catch (NumberFormatException excep) {
            Assertions.fail("Could not parse value of field 'c' as an integer!");
        }
    }


    @Test
    public void testStackQuery() {
        JSONArray res = AERMODAgent.StackQuery("jibusinessunits");
        Assertions.assertNotNull(res);
    }

    @Test
    public void testBuildingQuery() {
        AERMODAgent agent = new AERMODAgent();
        JSONArray res = agent.BuildingQuery("jibusinessunits");
        Assertions.assertNotNull(res);
    }





}
// End of class.
