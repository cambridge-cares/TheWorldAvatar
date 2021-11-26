package uk.ac.cam.cares.jps.agent.agent_eg;

import org.json.JSONObject;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Test;

/**
 * Example tests for the AdditionAgent.
 *
 * @author Michael Hillman {@literal <mdhillman@cmclinnovations.com>}
 */
public class AdditionAgentTest {

    /**
     * Tests the addition agent by creating a mock request.
     */
    @Test
    public void testAgent() {
        JSONObject mockRequest = new JSONObject();
        mockRequest.put("a", 2);
        mockRequest.put("b", 3);

        AdditionAgent agent = new AdditionAgent();
        JSONObject result = agent.processRequestParameters(mockRequest);
        Assertions.assertNotNull(result, "Expected a JSON Object to be returned!");
        Assertions.assertTrue(result.has("c"), "Expected to find a field named 'c'.");
        
        try {
            int resultingNumber = result.getInt("c");
            Assertions.assertEquals(5, resultingNumber, "Expected value of field 'c' to be '5'.");
        } catch (NumberFormatException excep) {
            Assertions.fail("Could not parse value of field 'c' as an integer!");
        }
    }

}
// End of class.
