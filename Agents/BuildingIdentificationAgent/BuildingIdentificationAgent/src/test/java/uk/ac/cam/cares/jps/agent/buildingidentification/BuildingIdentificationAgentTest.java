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

        JSONArray coord1 = new JSONArray(new double[] { 103.86009, 1.29251 });
        JSONArray coord2 = new JSONArray(new double[] { 103.85728, 1.29115 });
        coordinates.put(coord1);
        coordinates.put(coord2);

        request.put("coordinates", coordinates);
        request.put("requestUrl", "/location");

        JSONObject result = new BuildingIdentificationAgent().processRequestParameters(request);

        assertTrue(result.getInt("number_matched") > 0);

    }

    @Test
    void testAgentTablePoint() {
        JSONObject request = new JSONObject();
        request.put("maxDistance", "100.0");
        // request.put("endpoint", System.getenv("endpoint"));
        request.put("dbUrl", System.getenv("dbUrl"));
        request.put("dbUser", System.getenv("dbUser"));
        request.put("dbPassword", System.getenv("dbPassword"));

        request.put("requestUrl", "/postgis");
        request.put("table", "public.carpark");

        JSONObject result = new BuildingIdentificationAgent().processRequestParameters(request);

        assertTrue(result.getInt("number_matched") > 0);

    }

    @Test
    void testAgentTableNonPoint() {
        JSONObject request = new JSONObject();

        request.put("dbUrl", System.getenv("dbUrl"));
        request.put("dbUser", System.getenv("dbUser"));
        request.put("dbPassword", System.getenv("dbPassword"));

        request.put("requestUrl", "/postgis");
        request.put("table", "public.osm_agent");

        JSONObject result = new BuildingIdentificationAgent().processRequestParameters(request);

        assertTrue(result.getInt("number_matched") > 0);
    }

    @Test
    void testAgentTableOneToMany() {
        JSONObject request = new JSONObject();
        request.put("maxDistance", "100.0");
        request.put("dbUrl", System.getenv("dbUrl"));
        request.put("dbUser", System.getenv("dbUser"));
        request.put("dbPassword", System.getenv("dbPassword"));

        request.put("requestUrl", "/postgis");
        request.put("table", "public.landplot");
        request.put("column", "lod1Geometry");
        request.put("oneToMany", "true");
        request.put("filterColumns", new JSONArray(new String[] { "LU_DESC" }));
        JSONArray excludedValues = new JSONArray();
        JSONArray excludedLandType = new JSONArray(new String[] { "ROAD", "PARK" });
        excludedValues.put(excludedLandType);
        request.put("excludedValues", excludedValues);

        JSONObject result = new BuildingIdentificationAgent().processRequestParameters(request);

        assertTrue(result.getInt("number_matched") > 0);

    }

}