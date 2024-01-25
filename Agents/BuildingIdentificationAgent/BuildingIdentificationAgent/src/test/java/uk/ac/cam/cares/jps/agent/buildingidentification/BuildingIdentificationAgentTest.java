package uk.ac.cam.cares.jps.agent.buildingidentification;

import org.json.JSONObject;
import org.junit.jupiter.api.Test;

import static org.junit.jupiter.api.Assertions.*;

import org.json.JSONArray;

class BuildingIdentificationAgentTest {

    @Test
    void testAgentArray() {
        JSONObject request = new JSONObject();
        request.put("maxDistance", "100.0");
        // request.put("endpoint", System.getenv("endpoint"));
        request.put("dbUrl", System.getenv("dbUrl"));
        request.put("dbUser", System.getenv("dbUser"));
        request.put("dbPassword", System.getenv("dbPassword"));
        JSONArray coordinates = new JSONArray();

        JSONArray coord1 = new JSONArray(new double[] { 103.67455581177452, 1.2711156279472327 });
        JSONArray coord2 = new JSONArray(new double[] { 103.68455581177452, 1.2611156279472327 });
        coordinates.put(coord1);
        coordinates.put(coord2);

        request.put("coordinates", coordinates);
        request.put("requestUrl", "array");

        JSONObject result = new BuildingIdentificationAgent().processRequestParameters(request);

        assertTrue(result.getInt("number_matched") > 0);

    }

    @Test
    void testAgentTable() {
        JSONObject request = new JSONObject();
        request.put("maxDistance", "100.0");
        // request.put("endpoint", System.getenv("endpoint"));
        request.put("dbUrl", System.getenv("dbUrl"));
        request.put("dbUser", System.getenv("dbUser"));
        request.put("dbPassword", System.getenv("dbPassword"));

        request.put("requestUrl", "table");
        request.put("table", "industry.test");

        JSONObject result = new BuildingIdentificationAgent().processRequestParameters(request);

        assertTrue(result.getInt("number_matched") > 0);

    }

}