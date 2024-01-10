package uk.ac.cam.cares.jps.agent.buildingidentification;

import org.json.JSONObject;
import org.junit.jupiter.api.Disabled;
import org.junit.jupiter.api.Test;

import static org.junit.jupiter.api.Assertions.*;

public class BuildingIdentificationAgentTest {

    @Test
    public void testAgent() {
        JSONObject request = new JSONObject();
        request.put("maxDistance", "100.0");
        request.put("endpoint", System.getenv("endpoint"));
        request.put("dbUrl", System.getenv("dbUrl"));
        request.put("dbUser", System.getenv("dbUser"));
        request.put("dbPassword", System.getenv("dbPassword"));

        JSONObject result = new BuildingIdentificationAgent().processRequestParameters(request);
        assertTrue(result.getInt("number_factories") > 0);
        assertTrue(result.getInt("number_buildings") > 0);

    }

}